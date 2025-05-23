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

#' Filter a codelist to keep only the codes being used in patient records
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @inheritParams minimumCountDoc
#' @inheritParams tableDoc
#'
#' @return The filtered codelist with only the codes used in the database
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockVocabRef("database")
#' codes <- getCandidateCodes(cdm = cdm,
#'                            keywords = "arthritis",
#'                            domains = "Condition",
#'                            includeDescendants = FALSE)
#' x <- subsetToCodesInUse(list("cs1" = codes$concept_id,
#'                                "cs2" = 999),
#'                                 cdm = cdm)
#'
#' x
#' CDMConnector::cdmDisconnect(cdm)
#' }
subsetToCodesInUse <- function(x,
                               cdm,
                               minimumCount = 0L,
                               table = c("condition_occurrence",
                                         "device_exposure",
                                         "drug_exposure",
                                         "measurement",
                                         "observation",
                                         "procedure_occurrence",
                                         "visit_occurrence")){

  # initial checks
  omopgenerics::assertList(x)
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm,
                                           requiredTables = c("achilles_analysis",
                                                              "achilles_results",
                                                              "achilles_results_dist"))
  omopgenerics::assertNumeric(minimumCount, integerish = T, min = 0, length = 1)
  omopgenerics::assertCharacter(table)

  dbCodes <- codesInUse(cdm = cdm,
                        minimumCount = minimumCount,
                        table = table)

  if(is.null(dbCodes)){
    for(i in seq_along(x)){
      cli::cli_inform("No codes from any codelist found in the database")
      return(invisible(omopgenerics::emptyCodelist()))
    }
  } else {
    for(i in seq_along(x)){
      x[[i]] <- intersect(x[[i]], dbCodes)
      if(!length(x[[i]]) >= 1){
        cli::cli_inform("No codes from codelist {names(x)[i]} found in the database")
      }
    }
  }

  x <- vctrs::list_drop_empty(x)

  if(length(x) == 0){
    return(invisible(omopgenerics::emptyCodelist()))
  }

  x

}

#' Get the concepts being used in patient records
#'
#' @inheritParams cdmDoc
#' @inheritParams minimumCountDoc
#' @inheritParams tableDoc
#'
#' @return A list of integers indicating codes being used in the database.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockVocabRef("database")
#' x <- codesInUse(cdm = cdm)
#' x
#' CDMConnector::cdmDisconnect(cdm)
#' }
codesInUse <- function(cdm,
                       minimumCount = 0L,
                       table = c("condition_occurrence",
                                 "device_exposure",
                                 "drug_exposure",
                                 "measurement",
                                 "observation",
                                 "procedure_occurrence",
                                 "visit_occurrence")){

  # initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm,
                                           requiredTables = c("achilles_analysis",
                                                              "achilles_results",
                                                              "achilles_results_dist"))
  omopgenerics::assertNumeric(minimumCount, integerish = T, min = 0, length = 1)
  omopgenerics::assertCharacter(table)

  codes <- fetchAchillesCodesInUse(cdm, minimumCount = minimumCount)

  codes
}

#' Get the source codes being used in patient records
#'
#' @inheritParams cdmDoc
#' @inheritParams tableDoc
#'
#' @return A list of source codes used in the database.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockVocabRef("database")
#' x <- sourceCodesInUse(cdm = cdm)
#' x
#' CDMConnector::cdmDisconnect(cdm)
#' }
sourceCodesInUse <- function(cdm,
                             table = c("condition_occurrence",
                                       "device_exposure",
                                       "drug_exposure",
                                       "measurement",
                                       "observation",
                                       "procedure_occurrence",
                                       "visit_occurrence")){

  # initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm,
                                           requiredTables = c("achilles_analysis",
                                                              "achilles_results",
                                                              "achilles_results_dist"))
  omopgenerics::assertCharacter(table)

  codes <- fetchAchillesSourceCodesInUse(cdm)

  codes
}

unmappedSourceCodesInUse <- function(cdm,
                                     table = c("condition_occurrence",
                                               "device_exposure",
                                               "drug_exposure",
                                               "measurement",
                                               "observation",
                                               "procedure_occurrence")){

  # initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertCharacter(table)

  # note, no achilles query for this so will have to query the cdm

  codes <- list()
  for(i in seq_along(table)){
    workingTable <- table[i]
    standardConcept <- dplyr::case_when(
      workingTable == "condition_occurrence" ~ "condition_concept_id",
      workingTable == "device_exposure" ~ "device_concept_id",
      workingTable == "drug_exposure" ~ "drug_concept_id",
      workingTable == "measurement" ~ "measurement_concept_id",
      workingTable == "observation" ~ "observation_concept_id",
      workingTable == "procedure_occurrence" ~ "procedure_concept_id"
    )

    workingConcept <- dplyr::case_when(
      workingTable == "condition_occurrence" ~ "condition_source_concept_id",
      workingTable == "device_exposure" ~ "device_source_concept_id",
      workingTable == "drug_exposure" ~ "drug_source_concept_id",
      workingTable == "measurement" ~ "measurement_source_concept_id",
      workingTable == "observation" ~ "observation_source_concept_id",
      workingTable == "procedure_occurrence" ~ "procedure_source_concept_id"
    )

    # keep unmapped codes
    codes[[i]] <- as.integer(cdm[[workingTable]] |>
                               dplyr::filter(!!rlang::sym(standardConcept) == 0) |>
                               dplyr::select(dplyr::all_of(workingConcept)) |>
                               dplyr::distinct() |>
                               dplyr::pull())
    codes[[i]] <- stats::na.omit(codes[[i]])
  }

  codes <- unlist(codes)

  codes
}

fetchAchillesCodesInUse <- function(cdm, minimumCount = 0L, collect = TRUE){

  minimumCount <- as.integer(minimumCount)
  codes <- cdm[["achilles_results"]] |>
    dplyr::filter(.data$analysis_id %in%
                    c(
                      401L, # condition occurrence
                      701L, # drug_exposure
                      801L, # observation
                      1801L, # measurement
                      201L, # visit_occurrence
                      601L, # procedure_occurrence
                      2101L # device_exposure
                    ),
                  .data$count_value >= .env$minimumCount) |>
    dplyr::select("concept_id" = "stratum_1") |>
    dplyr::mutate(concept_id = as.integer(.data$concept_id)) |>
    dplyr::distinct()

  if(isTRUE(collect)){
    codes <- codes |>
      dplyr::pull("concept_id")
  }

  codes

}

fetchAchillesSourceCodesInUse <- function(cdm, minimumCount = 0L){

  minimumCount <- as.integer(minimumCount)

  cdm[["achilles_results"]] |>
    dplyr::filter(.data$analysis_id %in%
                    c(
                      425L, # condition occurrence
                      725L, # drug_exposure
                      825L, # observation
                      1825L, # measurement
                      225L, # visit_occurrence
                      625L, # procedure_occurrence
                      2125L # device_exposure
                    )) |>
    dplyr::filter(.data$count_value >= .env$minimumCount) |>
    dplyr::select("stratum_1") |>
    dplyr::distinct() |>
    dplyr::mutate(stratum_1 = as.integer(.data$stratum_1)) |>
    dplyr::pull("stratum_1")
}
