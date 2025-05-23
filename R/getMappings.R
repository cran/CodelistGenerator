# Copyright 2025 DARWIN EU®
#
# This file is part of CodelistGenerator
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


#' Show mappings from non-standard vocabularies to standard.
#'
#' @param candidateCodelist Dataframe.
#' @inheritParams cdmDoc
#' @param nonStandardVocabularies Character vector.
#'
#' @return Tibble with the information of potential standard to non-standard
#' mappings for the codelist of interest.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- CodelistGenerator::mockVocabRef()
#' codes <- CodelistGenerator::getCandidateCodes(
#'   cdm = cdm,
#'   keywords = "osteoarthritis"
#' )
#' CodelistGenerator::getMappings(
#'   cdm = cdm,
#'   candidateCodelist = codes,
#'   nonStandardVocabularies = "READ"
#')
#'}
getMappings <- function(candidateCodelist,
                         cdm = NULL,
                         nonStandardVocabularies = c(
                           "ATC", "ICD10CM", "ICD10PCS",
                           "ICD9CM", "ICD9Proc",
                           "LOINC", "OPCS4", "Read",
                           "RxNorm", "RxNorm Extension",
                           "SNOMED"
                         )) {

  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertCharacter(nonStandardVocabularies)
  omopgenerics::assertTable(candidateCodelist)
  omopgenerics::assertTrue("concept" %in% names(cdm))
  omopgenerics::assertTrue("concept_relationship" %in% names(cdm))
  omopgenerics::assertTrue("concept_ancestor" %in% names(cdm))
  omopgenerics::assertTrue("concept_synonym" %in% names(cdm))
  omopgenerics::assertTrue("vocabulary" %in% names(cdm))

  conceptDb <- cdm$concept
  conceptRelationshipDb <- cdm$concept_relationship

  # lowercase names
  conceptDb <- dplyr::rename_with(conceptDb, tolower)
  conceptRelationshipDb <- dplyr::rename_with(
    conceptRelationshipDb,
    tolower
  )

  # vocabs to upper case
  nonStandardVocabularies <- toupper(nonStandardVocabularies)
  conceptDb <- conceptDb |>
    dplyr::mutate(vocabulary_id = toupper(.data$vocabulary_id))

  # check nonStandardVocabularies exist
  nonStandardVocabulariesInDb <- conceptDb |>
    dplyr::select("vocabulary_id") |>
    dplyr::distinct() |>
    dplyr::collect() |>
    dplyr::pull()
  for (i in seq_along(nonStandardVocabularies)) {
    nonStandardVocabulariesCheck <- nonStandardVocabularies[i] %in%
      nonStandardVocabulariesInDb
    omopgenerics::assertTrue(nonStandardVocabulariesCheck)
    if (!isTRUE(nonStandardVocabulariesCheck)) {
        cli::cli_abort("- Vocabulary {nonStandardVocabularies[i]} not found")
    }
  }

  mappedCodes <- conceptDb |>
    dplyr::inner_join(conceptRelationshipDb |>
      dplyr::filter(.data$relationship_id == "Mapped from") |>
      dplyr::filter(.data$concept_id_1 %in% !!candidateCodelist$concept_id) |>
      dplyr::select("concept_id_1", "concept_id_2") |>
      dplyr::rename("concept_id" = "concept_id_2"),
    by = c("concept_id")
    ) |>
    dplyr::filter(.data$vocabulary_id %in% .env$nonStandardVocabularies) |>
    dplyr::distinct() |>
    dplyr::collect()

  mappedCodes <- mappedCodes |>
    dplyr::select(
      "concept_id_1", "concept_id",
      "concept_name", "concept_code",
      "vocabulary_id"
    )

  mappedCodes <- mappedCodes |>
    dplyr::select("concept_id_1") |>
    dplyr::rename("concept_id" = "concept_id_1") |>
    dplyr::left_join(conceptDb |>
      dplyr::filter(.data$concept_id %in% !!mappedCodes$concept_id_1) |>
      dplyr::collect(),
    by = c("concept_id")
    ) |>
    dplyr::select("concept_id", "concept_name", "vocabulary_id") |>
    dplyr::rename("standard_vocabulary_id" = "vocabulary_id") |>
    dplyr::rename("concept_id_1" = "concept_id") |>
    dplyr::rename("standard_concept_name" = "concept_name") |>
    dplyr::full_join(mappedCodes,
      by = "concept_id_1"
    ) |>
    dplyr::rename("standard_concept_id" = "concept_id_1") |>
    dplyr::rename("non_standard_concept_id" = "concept_id") |>
    dplyr::rename("non_standard_concept_code" = "concept_code") |>
    dplyr::rename("non_standard_concept_name" = "concept_name") |>
    dplyr::rename("non_standard_vocabulary_id" = "vocabulary_id")

  mappedCodes <- mappedCodes |>
    dplyr::distinct() |>
    dplyr::arrange(.data$standard_concept_id)

  return(mappedCodes)
}
