#' Find orphan codes related to a codelist using achilles counts and, if
#' available, PHOEBE concept recommendations
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @inheritParams domainDoc
#'
#' @return A summarised result containg the frequency of codes related
#' to (but not in) the codelist.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockVocabRef("database")
#' codes <- getCandidateCodes(cdm = cdm,
#' keywords = "Musculoskeletal disorder",
#' domains = "Condition",
#' includeDescendants = FALSE)
#'
#' orphan_codes <- summariseOrphanCodes(x = list("msk" = codes$concept_id),
#' cdm = cdm)
#'
#' orphan_codes
#' CDMConnector::cdmDisconnect(cdm)
#' }
summariseOrphanCodes <- function(x,
                                 cdm,
                                 domain = c("condition",
                                            "device",
                                            "drug",
                                            "measurement",
                                            "observation",
                                            "procedure",
                                            "visit")){
  omopgenerics::validateCdmArgument(cdm,
                                    requiredTables = c("achilles_analysis",
                                                       "achilles_results",
                                                       "achilles_results_dist"))
  x <- omopgenerics::newCodelist(x)
  omopgenerics::assertCharacter(domain)

  # will only return codes that are used in the database
  # which we get from achilles tables
  codesUsed <- fetchAchillesCodesInUse(cdm = cdm,
                                       minimumCount = 0,
                                       collect = FALSE)
  descendantsUsed <- cdm$concept_ancestor |>
    dplyr::inner_join(codesUsed,
                      by = c("descendant_concept_id"="concept_id"))
  ancestorsUsed <- cdm$concept_ancestor |>
    dplyr::inner_join(codesUsed,
                      by = c("ancestor_concept_id"="concept_id"))
  relationshipUsed1 <- cdm$concept_relationship |>
    dplyr::inner_join(codesUsed,
                      by = c("concept_id_1"="concept_id"))
  relationshipUsed2 <- cdm$concept_relationship |>
    dplyr::inner_join(codesUsed,
                      by = c("concept_id_2"="concept_id"))

  if("concept_recommended" %in% names(cdm)){
    phoebe <- TRUE
    phoebeUsed <- cdm$concept_recommended |>
      dplyr::inner_join(codesUsed,
                        by = c("concept_id_2"="concept_id"))
  } else {
    phoebe <- FALSE
    cli::cli_inform(c("PHOEBE results not available",
                      "i" = "The concept_recommended table is not present in the cdm."))
  }

  orphanCodes <- list()
  tableCodelist <- paste0(omopgenerics::uniqueTableName(),
                          omopgenerics::uniqueId())

  for(i in seq_along(x)){
    cli::cli_inform("Getting orphan codes for {names(x)[i]}")
    cdm <- omopgenerics::insertTable(cdm = cdm,
                                     name = tableCodelist,
                                     table = dplyr::tibble(concept_id = x[[i]]),
                                     overwrite = TRUE,
                                     temporary = FALSE)

    # get descendants used in db
    orphanDescendants <- cdm[[tableCodelist]] |>
      dplyr::inner_join(descendantsUsed,
                       by = c("concept_id" = "ancestor_concept_id")) |>
      dplyr::select("concept_id" = "descendant_concept_id") |>
      dplyr::filter(!is.na(.data$concept_id)) |>
      dplyr::distinct() |>
      dplyr::pull("concept_id")

    # get direct ancestors used in db
    orphanAncestors <- cdm[[tableCodelist]] |>
      dplyr::left_join(ancestorsUsed,
                       by = c("concept_id" = "descendant_concept_id")) |>
      dplyr::filter(.data$min_levels_of_separation == 1) |>
      dplyr::select("concept_id" = "ancestor_concept_id")  |>
      dplyr::filter(!is.na(.data$concept_id)) |>
      dplyr::distinct() |>
      dplyr::pull("concept_id")

    # get relationship 1
    orphanRelationship1 <- cdm[[tableCodelist]] |>
      dplyr::left_join(relationshipUsed1,
                       by = c("concept_id" = "concept_id_2")) |>
      dplyr::select("concept_id" = "concept_id_1")  |>
      dplyr::filter(!is.na(.data$concept_id)) |>
      dplyr::distinct() |>
      dplyr::pull("concept_id")

    # get relationship 2
    orphanRelationship2 <- cdm[[tableCodelist]] |>
      dplyr::left_join(relationshipUsed1,
                       by = c("concept_id" = "concept_id_1")) |>
      dplyr::select("concept_id" = "concept_id_2")  |>
      dplyr::filter(!is.na(.data$concept_id)) |>
      dplyr::distinct() |>
      dplyr::pull("concept_id")

    orphanCodes[[names(x)[i]]] <- c(orphanDescendants,
                                    orphanAncestors,
                                    orphanRelationship1,
                                    orphanRelationship2)


    if(isTRUE(phoebe)){
      phoebeCodes <-  cdm[[tableCodelist]] |>
        dplyr::left_join(phoebeUsed,
                         by = c("concept_id" = "concept_id_1")) |>
        dplyr::select("concept_id" = "concept_id_2")  |>
        dplyr::filter(!is.na(.data$concept_id)) |>
        dplyr::distinct() |>
        dplyr::pull("concept_id")

      orphanCodes[[names(x)[i]]] <- c(orphanCodes[[names(x)[i]]],
                                      phoebeCodes)
    }

    # make sure we don't have any of the original codes
    orphanCodes[[names(x)[i]]] <- setdiff(orphanCodes[[names(x)[i]]], x[[i]])
  }

  orphanCodes <- orphanCodes |> vctrs::list_drop_empty()
  if(length(orphanCodes) == 0){
  orphanCodes <- omopgenerics::emptySummarisedResult() |>
      omopgenerics::newSummarisedResult(
        settings = dplyr::tibble(
          result_id = as.integer(1),
          result_type = "orphan_code_use",
          package_name = "CodelistGenerator",
          package_version = as.character(utils::packageVersion(
            pkg = "CodelistGenerator"))
        )
      )
  } else {
    orphanCodes <- subsetOnDomain(orphanCodes,
                                  cdm = cdm,
                                  domain = domain)
    orphanCodes <- summariseAchillesCodeUse(orphanCodes, cdm = cdm)
    attr(orphanCodes, "settings")$result_type <- "orphan_code_use"
  }

  orphanCodes

}


