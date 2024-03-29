# Copyright 2023 DARWIN EU®
#
# This file is part of IncidencePrevalence
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

runSearch <- function(keywords,
                      cdm,
                      exclude,
                      domains,
                      standardConcept,
                      searchInSynonyms,
                      searchNonStandard,
                      includeDescendants,
                      includeAncestor) {

  if(!is.null(attr(cdm, "dbcon"))){
    prefix <- paste0(sample(letters, 5, TRUE),
                     collapse = "")
  }

  # connect to relevant vocabulary tables
  # will return informative error if not found
  conceptDb <- cdm$concept
  conceptAncestorDb <- cdm$concept_ancestor
  conceptSynonymDb <- cdm$concept_synonym
  conceptRelationshipDb <- cdm$concept_relationship
  drugStrengthDb <- cdm$drug_strength

  ## domains, standardConcept vocab to lower
  domains <- tolower(domains)
  standardConcept <- tolower(standardConcept)
  # new name for clarity
  standardConceptFlags <- standardConcept

  # formatting of conceptDb variables
  conceptDb <- conceptDb %>%
    dplyr::mutate(
      domain_id = tolower(.data$domain_id),
      standard_concept = dplyr::case_when(
        is.na(.data$standard_concept) ~ "non-standard",
        .data$standard_concept == "C" ~ "classification",
        .data$standard_concept == "S" ~ "standard",
        .default = as.character(.data$standard_concept)
      )
    )

  cli::cli_inform("{domains} domain: Limiting to domains of interest")
  concept <- conceptDb %>%
    dplyr::filter(.data$standard_concept %in% .env$standardConceptFlags,
                  .data$domain_id %in% .env$domains)

  # will only collect conceptSynonym later if needed
  if (searchInSynonyms == TRUE) {
    conceptSynonymDb <- conceptSynonymDb %>%
      dplyr::left_join(
        conceptDb %>%
          dplyr::select("concept_id", "domain_id", "standard_concept"),
        by = "concept_id"
      )

    conceptSynonymDb <- conceptSynonymDb %>%
      dplyr::filter(.data$domain_id %in% .env$domains &
                      .data$standard_concept %in% .env$standardConceptFlags) %>%
      dplyr::select(-c("domain_id", "standard_concept"))

    conceptSynonym <- conceptSynonymDb %>%
      # dplyr::collect() %>%
      dplyr::rename_with(tolower)
  }

  # collect the drug_strength table if drug
  if (domains == "drug") {
    drugStrength <- drugStrengthDb %>%
      dplyr::left_join(
        conceptDb %>%
          dplyr::rename("drug_concept_id" = "concept_id") %>%
          dplyr::select("drug_concept_id", "domain_id", "standard_concept"),
        by = "drug_concept_id"
      ) %>%
      dplyr::filter(.data$domain_id %in% .env$domains &
                      .data$standard_concept %in% .env$standardConceptFlags) %>%
      # dplyr::collect() %>%
      dplyr::rename_with(tolower)
  }


  candidateCodesList <- list()


  workingConcept <- concept %>%
    dplyr::filter(.data$domain_id == .env$domains)

  if (exists("conceptSynonym") == TRUE) {
    workingconceptSynonym <- conceptSynonym %>%
      dplyr::left_join(
        concept %>%
          dplyr::select("concept_id", "domain_id"),
        by = "concept_id"
      ) %>%
      dplyr::filter(.data$domain_id == .env$domains) %>%
      dplyr::select(!"domain_id")
  }


  # Start finding candidate codes
  # 1) first, get codes to exclude
  # and anti_join throughout to make sure these don't appear
  # exact matches

  if (length(exclude) > 0) {
    # Get standard, condition concepts which include one of the exclusion words
    # always use exact matching
    excludeCodes <- getMatches(
      words = tidyWords(exclude),
      conceptDf = workingConcept
    )
  }

  # 2) Get standard, condition concepts which
  # include one of the keywords
  cli::cli_inform("{domains}: Getting concepts to include")
  candidateCodes <- getMatches(
    words = tidyWords(keywords),
    conceptDf = workingConcept
  )

  candidateCodes <- candidateCodes %>%
    dplyr::mutate(found_from = "From initial search")

  # run exclusion
  if (length(exclude) > 0) {
    if (excludeCodes %>%
        utils::head(10) %>%
        dplyr::tally() %>%
        dplyr::pull("n") > 0) {
      candidateCodes <- candidateCodes %>%
        dplyr::anti_join(
          excludeCodes %>%
            dplyr::select("concept_id"),
          by = "concept_id"
        )
    }
  }

  # 3) also search in synonyms if option set to true
  # left join back to concept from workingConcept table
  if (searchInSynonyms == TRUE) {
    cli::cli_inform("{domains} domain: Adding concepts using synonymns")
    candidateCodesInSynonyms <- getMatches(
      words = tidyWords(keywords),
      conceptDf = workingconceptSynonym %>%
        dplyr::rename("concept_name" = "concept_synonym_name")
    ) %>%
      dplyr::select("concept_id") %>%
      dplyr::distinct() %>%
      dplyr::left_join(workingConcept,
                       by = "concept_id", copy = TRUE
      )

    candidateCodes <- dplyr::union_all(
      candidateCodes,
      candidateCodesInSynonyms %>%
        dplyr::mutate(found_from = "In synonyms")
    ) %>%
      dplyr::distinct()
  }

  # run exclusion
  if (length(exclude) > 0) {
    if (excludeCodes %>%
        utils::head(10) %>%
        dplyr::tally() %>%
        dplyr::pull("n") > 0) {
      candidateCodes <- candidateCodes %>%
        dplyr::anti_join(
          excludeCodes %>%
            dplyr::select("concept_id"),
          by = "concept_id"
        )
    }
  }

  candidateCodesList[[domains]] <- candidateCodes


  # 5) add any codes lower in the hierarchy
  if (includeDescendants == TRUE) {
    if (candidateCodes %>%
        utils::head(10) %>%
        dplyr::tally() %>%
        dplyr::pull("n") > 0) {
      cli::cli_inform("{domains} domain: Adding descendants")
      candidateCodeDescendants <- addDescendants(
        workingCandidateCodes = candidateCodes,
        conceptAncestorDf = conceptAncestorDb,
        conceptDf = concept
      )

      candidateCodes <- dplyr::union_all(
        candidateCodes,
        candidateCodeDescendants %>%
          dplyr::mutate(found_from = "From descendants") %>%
          dplyr::anti_join(candidateCodes %>%
                             dplyr::select("concept_id"),
                           by = "concept_id"))

      # run exclusion
      if (length(exclude) > 0) {
        if (excludeCodes %>%
            utils::head(10) %>%
            dplyr::tally() %>%
            dplyr::pull("n") > 0) {
          candidateCodes <- candidateCodes %>%
            dplyr::anti_join(excludeCodes %>%
                               dplyr::select("concept_id"),
                             by = "concept_id", copy = TRUE
            )
        }
      }
    }
  }

  # 6) add any codes one level above in the hierarchy
  if (includeAncestor == TRUE) {
    if (candidateCodes %>%
        utils::head(10) %>%
        dplyr::tally() %>%
        dplyr::pull("n") > 0) {
      cli::cli_inform("{domains} domain: Adding ancestor")

      candidateCodeAncestor <- addAncestor(
        workingCandidateCodes = candidateCodes,
        conceptAncestorDf = conceptAncestorDb,
        conceptDf = conceptDb
      )

      candidateCodes <- dplyr::union_all(
        candidateCodes,
        candidateCodeAncestor %>%
          dplyr::mutate(found_from = "From ancestor")
      ) %>%
        dplyr::distinct()

      # run exclusion
      if (length(exclude) > 0) {
        if (excludeCodes %>%
            utils::head(10) %>%
            dplyr::tally() %>%
            dplyr::pull("n") > 0) {
          candidateCodes <- candidateCodes %>%
            dplyr::anti_join(excludeCodes %>% dplyr::select("concept_id"),
                             by = "concept_id"
            )
        }
      }
    }
  }

  # 7) add codes from non-standard
  # nb we do this last so as to not include descendants
  # which can blow up candiate codelist when there
  # are multiple mappings
  if (searchNonStandard == TRUE) {
    cli::cli_inform("{domains} domain: Adding codes from non-standard")
    conceptNs <- conceptDb %>%
      dplyr::filter(.data$standard_concept == "non-standard") %>%
      dplyr::rename_with(tolower)

    if (conceptNs %>%
        utils::head(10) %>%
        dplyr::tally() %>%
        dplyr::pull("n") > 0) {
      candidateCodesNs <- getMatches(
        words = tidyWords(keywords),
        conceptDf = conceptNs
      )
    }

    if (conceptNs %>%
        utils::head(10) %>%
        dplyr::tally() %>%
        dplyr::pull("n") > 0) {
      candidateCodesNs <- candidateCodesNs %>%
        dplyr::select("concept_id") %>%
        dplyr::left_join(
          conceptRelationshipDb %>%
            dplyr::filter(.data$relationship_id == "Mapped from") %>%
            # dplyr::collect() %>%
            dplyr::rename_with(tolower),
          by = c("concept_id" = "concept_id_2"), copy = TRUE
        ) %>%
        dplyr::select("concept_id_1") %>%
        dplyr::rename("concept_id" = "concept_id_1") %>%
        dplyr::distinct() %>%
        dplyr::left_join(concept,
                         by = "concept_id", copy = TRUE
        )

      candidateCodes <- dplyr::union_all(
        candidateCodes,
        candidateCodesNs %>%
          dplyr::mutate(found_from = "From non-standard")
      ) %>%
        dplyr::distinct()
    }

    # run exclusion
    if (length(exclude) > 0) {
      if (excludeCodes %>%
          utils::head(10) %>%
          dplyr::tally() %>%
          dplyr::pull("n") > 0) {
        candidateCodes <- candidateCodes %>%
          dplyr::anti_join(excludeCodes %>% dplyr::select("concept_id"),
                           by = "concept_id"
          )
      }
    }
  }

  candidateCodes <- candidateCodes %>%
    dplyr::collect()



  if (nrow(candidateCodes) > 0) {
    # 8) Finish up
    if (domains == "drug") { #add drug_strength information and dose form
      candidateCodes <- candidateCodes %>%
        dplyr::left_join(
          drugStrength %>%
            dplyr::rename("concept_id" = "drug_concept_id") %>%
            dplyr::mutate("concept_id" = as.integer(.data$concept_id)) %>%
            dplyr::select(
              "concept_id",
              "ingredient_concept_id", "amount_value", "amount_unit_concept_id",
              "numerator_value", "numerator_unit_concept_id", "denominator_value",
              "denominator_unit_concept_id", "box_size"
            ),
          by = "concept_id", copy = TRUE
        )

      candidateCodes <- candidateCodes %>%
        dplyr::select(
          "concept_id", "concept_name",
          "domain_id", "concept_class_id",
          "vocabulary_id", "found_from",
          "ingredient_concept_id", "amount_value", "amount_unit_concept_id",
          "numerator_value", "numerator_unit_concept_id",
          "denominator_value", "denominator_unit_concept_id", "box_size"
        )

      drugConceptForm <- conceptRelationshipDb %>%
        dplyr::filter(.data$relationship_id == "RxNorm has dose form") %>%
        dplyr::select("concept_id_1", "concept_id_2") %>%
        dplyr::rename("concept_id" = "concept_id_2") %>%
        dplyr::distinct() %>%
        dplyr::left_join(conceptDb, by = "concept_id") %>%
        dplyr::collect() %>%
        dplyr::select("concept_id_1", "concept_id", "concept_name") %>%
        dplyr::rename("dose_form_id" = "concept_id") %>%
        dplyr::rename("dose_form" = "concept_name") %>%
        dplyr::rename("concept_id" = "concept_id_1") %>%
        dplyr::select(!"dose_form_id")
      # can have multiple forms so pivot
      drugConceptForm <- drugConceptForm %>%
        dplyr::group_by(.data$concept_id) %>%
        dplyr::mutate(seq = dplyr::row_number()) %>%
        tidyr::pivot_wider(
          names_from = "seq",
          values_from = "dose_form"
        )
      if (nrow(drugConceptForm) > 0) {
        drugConceptForm <- drugConceptForm %>%
          tidyr::unite(
            col = "dose_form", 2:ncol(drugConceptForm), sep = "; ",
            na.rm = TRUE
          )
        candidateCodes <- candidateCodes %>%
          dplyr::left_join(
            drugConceptForm,
            by = "concept_id"
          )
      }
    }

    candidateCodes <- candidateCodes %>%
      dplyr::select(dplyr::any_of(c("concept_id", "found_from",
                                    "ingredient_concept_id", "amount_value",
                                    "amount_unit_concept_id",
                                    "numerator_value",
                                    "numerator_unit_concept_id",
                                    "denominator_value",
                                    "denominator_unit_concept_id",
                                    "box_size", "dose_form"))) %>%
      dplyr::distinct()

    # remove duplicates (found in different ways)
    # keep first time it was found
    # for drug,  same concept_id with different ingredient_concept_id
    # will be removed as well (with only the first kept).
    candidateCodes <- candidateCodes %>%
      dplyr::group_by(.data$concept_id) %>%
      dplyr::mutate(seq = dplyr::row_number(.data$concept_id)) %>%
      dplyr::filter(seq == 1) %>%
      dplyr::select(-"seq") %>%
      dplyr::ungroup()

  }


  if(!is.null(attr(cdm, "dbcon"))){
    CDMConnector::dropTable(cdm = cdm,
                            name = dplyr::starts_with(paste0("cg_",prefix)))
  }

  return(candidateCodes)

}


# helper functions for runSearch
tidyWords <- function(words) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertVector(words, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  # to avoid invalid UTF-8 error
  Encoding(words) <- "latin1"

  # some generic formatting
  workingWords <- trimws(words)
  workingWords <- stringr::str_replace_all(workingWords, "-", " ")
  workingWords <- stringr::str_replace_all(workingWords, "[[:punct:]]", "")
  workingWords <- stringr::str_remove_all(workingWords, "[^[\\da-zA-Z ]]")
  workingWords <- stringr::str_remove_all(workingWords, "[^\x01-\x7F]+")
  workingWords <- stringr::str_to_lower(workingWords)
  workingWords <- trimws(workingWords)

  return(workingWords)
}

getMatches <- function(words,
                       conceptDf) {
  conceptDf <- conceptDf %>% # start with all
    dplyr::mutate(concept_name = tolower(.data$concept_name))

  # because there may be a lot of synonyms, get these from a loop
  # (stringr::str_detect slows considerably
  # as more options are added in a single call using "|")

  # note, where one term is multiple words (e.g "knee osteoarthritis"),
  # split up and search
  # so that they don´t need to be next to each other
  # (e.g. to find "osteoarthritis of knee"))

  conceptsFound <- list()
  for (i in seq_along(words)) {
    workingExclude <- unlist(strsplit(words[i], " "))
    workingConcepts <- conceptDf # start with all

    for (j in seq_along(workingExclude)) {
      if (nchar(workingExclude[j]) >= 1) {

        if(inherits(workingConcepts, "tbl_sql")){
          workingConcepts <- workingConcepts %>%
            dplyr::filter(dplyr::sql(paste0("concept_name LIKE '%", .env$workingExclude[j], "%'")))
        } else {
          workingConcepts <- workingConcepts %>%
            dplyr::filter(stringr::str_detect(
              .data$concept_name,
              .env$workingExclude[j]
            ))
        }

      }
    }
    conceptsFound[[i]] <- workingConcepts %>%
      dplyr::collect()
  }

  if(length(conceptsFound)==1){
    conceptsFound <- conceptsFound[[1]] %>% dplyr::distinct()
  } else {
    conceptsFoundList <- list()
    conceptsFoundList[[1]] <- conceptsFound[[1]]
    for(i in 1:(length(conceptsFound)-1)){
      conceptsFoundList[[1]] <- dplyr::union_all(conceptsFoundList[[1]],
                                                 conceptsFound[[i+1]])

    }
    conceptsFound <- conceptsFoundList[[1]] %>% dplyr::distinct()
  }


  return(conceptsFound)
}

addDescendants <- function(workingCandidateCodes,
                           conceptAncestorDf,
                           conceptDf) {

  candidateCodeDescendants <- workingCandidateCodes %>%
    dplyr::select("concept_id") %>%
    dplyr::rename("ancestor_concept_id" = "concept_id") %>%
    dplyr::distinct() %>%
    dplyr::left_join(
      conceptAncestorDf %>%
        dplyr::filter("ancestor_concept_id" != "descendant_concept_id"),
      by = "ancestor_concept_id", copy = TRUE
    ) %>%
    dplyr::select("descendant_concept_id") %>%
    dplyr::filter(!is.na(.data$descendant_concept_id)) %>%
    dplyr::distinct() %>%
    dplyr::rename("concept_id" = "descendant_concept_id")

  candidateCodeDescendants <- candidateCodeDescendants %>%
    dplyr::left_join(conceptDf, by = "concept_id", copy = TRUE)

  return(candidateCodeDescendants)
}

addAncestor <- function(workingCandidateCodes,
                        conceptAncestorDf,
                        conceptDf) {
  candidateCodeAncestor <- workingCandidateCodes %>%
    dplyr::select("concept_id") %>%
    dplyr::rename("descendant_concept_id" = "concept_id") %>%
    dplyr::left_join(conceptAncestorDf,
                     by = "descendant_concept_id", copy = TRUE
    ) %>%
    dplyr::filter(.data$min_levels_of_separation == "1") %>%
    dplyr::select("ancestor_concept_id") %>%
    dplyr::rename("concept_id" = "ancestor_concept_id") %>%
    dplyr::left_join(conceptDf,
                     by = "concept_id", copy = TRUE
    )

  # keep if not already in candidateCodes
  candidateCodeAncestor <- candidateCodeAncestor %>%
    dplyr::anti_join(
      workingCandidateCodes %>%
        dplyr::select("concept_id"),
      by = "concept_id"
    )

  return(candidateCodeAncestor)
}
