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

#' Summarise code use in patient-level data.
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @inheritParams countByDoc
#' @inheritParams byConceptDoc
#' @inheritParams bySexDoc
#' @inheritParams byYearDoc
#' @inheritParams ageGroupDoc
#' @param dateRange Two dates. The first indicating the earliest cohort start
#' date and the second indicating the latest possible cohort end date. If NULL
#' or the first date is set as missing, the earliest observation_start_date in
#' the observation_period table will be used for the former. If NULL or the
#' second date is set as missing, the latest observation_end_date in the
#' observation_period table will be used for the latter.
#'
#' @return A tibble with count results overall and, if specified, by strata.
#' @export
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(duckdb::duckdb(),
#'                       dbdir = CDMConnector::eunomiaDir())
#' cdm <- CDMConnector::cdmFromCon(con,
#'                                 cdmSchema = "main",
#'                                 writeSchema = "main")
#'acetiminophen <- c(1125315,  1127433, 40229134,
#'40231925, 40162522, 19133768,  1127078)
#'poliovirus_vaccine <- c(40213160)
#'cs <- list(acetiminophen = acetiminophen,
#'           poliovirus_vaccine = poliovirus_vaccine)
#'results <- summariseCodeUse(cs,cdm = cdm)
#'results
#'CDMConnector::cdmDisconnect(cdm)
#'}
#'
summariseCodeUse <- function(x,
                             cdm,
                             countBy = c("record", "person"),
                             byConcept = TRUE,
                             byYear = FALSE,
                             bySex = FALSE,
                             ageGroup = NULL,
                             dateRange = as.Date(c(NA, NA))){

  omopgenerics::assertList(x, named = TRUE)

  codeUse <- list()
  for(i in seq_along(x)){
    cli::cli_inform("Getting use of codes from {names(x)[i]} ({i} of {length(x)})")
    codeUse[[i]] <- getCodeUse(x[i],
                               cdm = cdm,
                               cohortTable = NULL,
                               cohortId = NULL,
                               timing = "any",
                               countBy = countBy,
                               byConcept = byConcept,
                               byYear = byYear,
                               bySex = bySex,
                               ageGroup = ageGroup,
                               dateRange = dateRange)
  }
  codeUse <- dplyr::bind_rows(codeUse)

  if(nrow(codeUse) > 0) {
    codeUse <- codeUse |>
      dplyr::mutate(
        result_id = as.integer(1),
        cdm_name = omopgenerics::cdmName(cdm)
      ) |>
      omopgenerics::newSummarisedResult(
        settings = dplyr::tibble(
          result_id = as.integer(1),
          result_type = "code_use",
          package_name = "CodelistGenerator",
          package_version = as.character(utils::packageVersion("CodelistGenerator")),
          date_range_start = as.character(dateRange[1]),
          date_range_end   = as.character(dateRange[2])
        )
      )
  } else {
    codeUse <- omopgenerics::emptySummarisedResult()
  }

  return(codeUse)

}

#' Summarise code use among a cohort in the cdm reference
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @param cohortTable A cohort table from the cdm reference.
#' @param cohortId A vector of cohort IDs to include
#' @param timing When to assess the code use relative cohort dates. This can
#' be "any"(code use any time by individuals in the cohort) or  "entry" (code
#' use on individuals' cohort start date).
#' @inheritParams countByDoc
#' @inheritParams byConceptDoc
#' @inheritParams bySexDoc
#' @inheritParams byYearDoc
#' @inheritParams ageGroupDoc
#'
#' @return A tibble with results overall and, if specified, by strata
#' @export
#'
#' @examples
#' \dontrun{
#' library(CodelistGenerator)
#' library(duckdb)
#' library(DBI)
#' library(CDMConnector)
#' con <- dbConnect(duckdb(),
#'                  dbdir = eunomiaDir())
#' cdm <- cdmFromCon(con,
#'                  cdmSchema = "main",
#'                  writeSchema = "main")
#' cdm <- generateConceptCohortSet(cdm = cdm,
#'                   conceptSet = list(a = 260139,
#'                                     b = 1127433),
#'                   name = "cohorts",
#'                   end = "observation_period_end_date",
#'                   overwrite = TRUE)
#'
#'results_cohort_mult <-
#'summariseCohortCodeUse(list(cs = c(260139,19133873)),
#'                       cdm = cdm,
#'                       cohortTable = "cohorts",
#'                       timing = "entry")
#'
#'results_cohort_mult
#'CDMConnector::cdmDisconnect(cdm)
#'}
summariseCohortCodeUse <- function(x,
                                   cdm,
                                   cohortTable,
                                   cohortId = NULL,
                                   timing = "any",
                                   countBy = c("record", "person"),
                                   byConcept = TRUE,
                                   byYear = FALSE,
                                   bySex = FALSE,
                                   ageGroup = NULL) {

  omopgenerics::assertList(x, named = TRUE)
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertTrue("GeneratedCohortSet" %in% class(cdm[[cohortTable]]))
  omopgenerics::assertTrue(all(c("cohort_definition_id", "subject_id", "cohort_start_date",
                               "cohort_end_date") %in% colnames(cdm[[cohortTable]])))

  if(is.null(cohortId)){
    cohortId <- sort(CDMConnector::settings(cdm[[cohortTable]]) |>
                       dplyr::pull("cohort_definition_id"))
  }

  settings <- omopgenerics::settings(cdm[[cohortTable]]) |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId)

  cohortCodeUse <- list()
  for(i in seq_along(cohortId)){
    workingCohortName <- settings$cohort_name[settings$cohort_definition_id == i]
    for(j in seq_along(x)){
      cli::cli_inform(" Getting counts of {names(x)[j]} codes for cohort {workingCohortName}")
      cohortCodeUse[[paste0(i, "_", j)]] <- getCodeUse(x[j],
                                                       cdm = cdm,
                                                       cohortTable = cohortTable,
                                                       cohortId = cohortId[[i]],
                                                       timing = timing,
                                                       countBy = countBy,
                                                       byConcept = byConcept,
                                                       byYear = byYear,
                                                       bySex = bySex,
                                                       ageGroup = ageGroup,
                                                       dateRange = as.Date(c(NA,NA)))
    }}
  cohortCodeUse <- dplyr::bind_rows(cohortCodeUse) |>
    dplyr::arrange(dplyr::across(!c("variable_level", "estimate_value")))

  if (nrow(cohortCodeUse) > 0) {
    cohortCodeUse <- cohortCodeUse |>
      dplyr::mutate(
        result_id = as.integer(1),
        cdm_name = omopgenerics::cdmName(cdm)
      ) |>
      omopgenerics::newSummarisedResult(
        settings = dplyr::tibble(

          result_id = as.integer(1),
          result_type = "cohort_code_use",
          package_name = "CodelistGenerator",
          package_version = as.character(utils::packageVersion("CodelistGenerator")),
          timing = timing
        )
      )
  } else {
    cohortCodeUse <- omopgenerics::emptySummarisedResult()
  }

  return(cohortCodeUse)
}

getCodeUse <- function(x,
                       cdm,
                       cohortTable,
                       cohortId,
                       timing,
                       countBy,
                       byConcept,
                       byYear,
                       bySex,
                       ageGroup,
                       dateRange,
                       call = parent.frame()) {

  # initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertCharacter(timing, length = 1)
  omopgenerics::assertChoice(timing, choices = c("any","entry"))
  omopgenerics::assertChoice(countBy, choices = c("record", "person"))
  omopgenerics::assertNumeric(x[[1]], integerish = T)
  omopgenerics::assertList(x)
  omopgenerics::assertLogical(byConcept)
  omopgenerics::assertLogical(byYear)
  omopgenerics::assertLogical(bySex)
  checkAgeGroup(ageGroup = ageGroup)
  omopgenerics::assertDate(dateRange, length = 2, na = TRUE)

  if(is.null(attr(cdm, "write_schema"))){
    cli::cli_abort("cdm must have a write_schema specified",
                   call = call)
  }

  tableCodelist <- paste0(omopgenerics::uniqueTableName(),
                          omopgenerics::uniqueId())
  cdm <- omopgenerics::insertTable(cdm = cdm,
                            name = tableCodelist,
                            table = dplyr::tibble(concept_id = x[[1]]),
                            overwrite = TRUE,
                            temporary = FALSE)
  cdm[[tableCodelist]] <- cdm[[tableCodelist]] |>
    dplyr::left_join(
      cdm[["concept"]] |> dplyr::select("concept_id", "domain_id"),
      by = "concept_id")


  tableDomainsData <- paste0(omopgenerics::uniqueTableName(),
                             omopgenerics::uniqueId())
  cdm <- omopgenerics::insertTable(cdm = cdm,
                                   name = tableDomainsData,
                                   table = conceptDomainsData,
                                   overwrite = TRUE,
                                   temporary = FALSE)

  cdm[[tableCodelist]] <- cdm[[tableCodelist]] |>
    dplyr::mutate(domain_id = tolower(.data$domain_id)) |>
    dplyr::left_join(cdm[[tableDomainsData]],
                     by = "domain_id") |>
    dplyr::compute(name = tableCodelist,
                   temporary = FALSE,
                   overwrite = TRUE)
  CDMConnector::dropTable(cdm = cdm, name = tableDomainsData)
  cdm[[tableDomainsData]] <- NULL

  intermediateTable <- paste0(omopgenerics::uniqueTableName(),
                              omopgenerics::uniqueId())
  records <- getRelevantRecords(cdm = cdm,
                                tableCodelist = tableCodelist,
                                cohortTable = cohortTable,
                                cohortId = cohortId,
                                timing = timing,
                                intermediateTable = intermediateTable)

  if(!is.null(records) &&
     (records |> utils::head(1) |> dplyr::tally() |> dplyr::pull("n") > 0)) {

    if((!is.na(dateRange[1])) | (!is.na(dateRange[2]))){
      records <- filterDateRange(records, cdm, dateRange)
    }

    if(bySex == TRUE | !is.null(ageGroup)){
      records <- records |>
        PatientProfiles::addDemographicsQuery(age = !is.null(ageGroup),
                                         ageGroup = ageGroup,
                                         sex = bySex,
                                         priorObservation = FALSE,
                                         futureObservation =  FALSE,
                                         indexDate = "date") |>
        dplyr::compute(overwrite = TRUE,
                       name = omopgenerics::tableName(records),
                       temporary = FALSE)
    }

    byAgeGroup <- !is.null(ageGroup)
    codeCounts <- getSummaryCounts(records = records,
                                   cdm = cdm,
                                   countBy = countBy,
                                   byConcept = byConcept,
                                   byYear = byYear,
                                   bySex = bySex,
                                   byAgeGroup = byAgeGroup)

    if (is.null(cohortTable)) {
      cohortName <- NA
    } else {
      cohortName <- omopgenerics::settings(cdm[[cohortTable]]) |>
        dplyr::filter(.data$cohort_definition_id == cohortId) |>
        dplyr::pull("cohort_name")
    }
    codeCounts <-  codeCounts |>
      dplyr::mutate(
        "codelist_name" := !!names(x),
        "cohort_name" = .env$cohortName,
        "estimate_type" = "integer",
        "variable_name" = dplyr::if_else(is.na(.data$standard_concept_name), "overall", .data$standard_concept_name),
        "variable_level" = as.character(.data$standard_concept_id)
      ) |>
      omopgenerics::uniteGroup(cols = c("cohort_name", "codelist_name")) |>
      omopgenerics::uniteAdditional(
        cols = c("source_concept_name", "source_concept_id",
                 "source_concept_value", "domain_id"),
        ignore = "overall"
      ) |>
      dplyr::select(
        "group_name", "group_level", "strata_name", "strata_level",
        "variable_name", "variable_level", "estimate_name", "estimate_type",
        "estimate_value", "additional_name", "additional_level"
      )

  } else {
    codeCounts <- omopgenerics::emptySummarisedResult()
    cli::cli_inform(c(
      "i" = "No records found in the cdm for the concepts provided."
    ))
  }

  CDMConnector::dropTable(cdm = cdm,
                          name = tableCodelist)
  cdm[[tableCodelist]] <- NULL
  CDMConnector::dropTable(
    cdm = cdm,
    name = dplyr::starts_with(intermediateTable)
  )

  return(codeCounts)
}
#
# addDomainInfo <- function(codes,
#                           cdm) {
#
#   codes <- codes |>
#     dplyr::mutate(domain_id = tolower(.data$domain_id)) |>
#     dplyr::mutate(table_name =
#                     dplyr::case_when(
#                       stringr::str_detect(domain_id,"condition") ~ "condition_occurrence",
#                       stringr::str_detect(domain_id,"drug") ~ "drug_exposure",
#                       stringr::str_detect(domain_id,"observation") ~ "observation",
#                       stringr::str_detect(domain_id,"measurement") ~ "measurement",
#                       stringr::str_detect(domain_id,"visit") ~ "visit_occurrence",
#                       stringr::str_detect(domain_id,"procedure") ~ "procedure_occurrence",
#                       stringr::str_detect(domain_id,"device") ~ "device_exposure"
#                     )
#     ) |>
#     dplyr::mutate(standard_concept_id_name =
#                     dplyr::case_when(
#                       stringr::str_detect(domain_id,"condition") ~ "condition_concept_id",
#                       stringr::str_detect(domain_id,"drug") ~ "drug_concept_id",
#                       stringr::str_detect(domain_id,"observation") ~ "observation_concept_id",
#                       stringr::str_detect(domain_id,"measurement") ~ "measurement_concept_id",
#                       stringr::str_detect(domain_id,"visit") ~ "visit_concept_id",
#                       stringr::str_detect(domain_id,"procedure") ~ "procedure_concept_id",
#                       stringr::str_detect(domain_id,"device") ~ "device_concept_id"
#                     )
#     ) |>
#     dplyr::mutate(source_concept_id_name =
#                     dplyr::case_when(
#                       stringr::str_detect(domain_id,"condition") ~ "condition_source_concept_id",
#                       stringr::str_detect(domain_id,"drug") ~ "drug_source_concept_id",
#                       stringr::str_detect(domain_id,"observation") ~ "observation_source_concept_id",
#                       stringr::str_detect(domain_id,"measurement") ~ "measurement_source_concept_id",
#                       stringr::str_detect(domain_id,"visit") ~ "visit_source_concept_id",
#                       stringr::str_detect(domain_id,"procedure") ~ "procedure_source_concept_id",
#                       stringr::str_detect(domain_id,"device") ~ "device_source_concept_id"
#                     )
#     ) |>
#     dplyr::mutate(date_name =
#                     dplyr::case_when(
#                       stringr::str_detect(domain_id,"condition") ~ "condition_start_date",
#                       stringr::str_detect(domain_id,"drug") ~ "drug_exposure_start_date",
#                       stringr::str_detect(domain_id,"observation") ~ "observation_date",
#                       stringr::str_detect(domain_id,"measurement") ~ "measurement_date",
#                       stringr::str_detect(domain_id,"visit") ~ "visit_start_date",
#                       stringr::str_detect(domain_id,"procedure") ~ "procedure_date",
#                       stringr::str_detect(domain_id,"device") ~ "device_exposure_start_date"
#                     )
#     )
#
#   unsupported_domains <- codes |>
#     dplyr::filter(!is.na(.data$domain_id)) |>
#     dplyr::filter(is.na(.data$table_name)) |>
#     dplyr::pull("domain_id")
#
#   if(length(unsupported_domains)>0){
#     cli::cli_warn("Concepts included from non-supported domains
#                    ({unsupported_domains})")
#   }
#
#   return(codes)
#
# }

getRelevantRecords <- function(cdm,
                               tableCodelist,
                               cohortTable,
                               cohortId,
                               timing,
                               intermediateTable){

  codes <- cdm[[tableCodelist]] |> dplyr::collect()

  tableName <- purrr::discard(unique(codes$table), is.na)
  standardConceptIdName <- purrr::discard(unique(codes$standard_concept), is.na)
  sourceConceptIdName <- purrr::discard(unique(codes$source_concept), is.na)
  sourceConceptValueName <- purrr::discard(unique(codes$source_concept_value), is.na)
  dateName <- purrr::discard(unique(codes$date_name), is.na)

  if(!is.null(cohortTable)){
    if(is.null(cohortId)){
      cohortSubjects <- cdm[[cohortTable]] |>
        dplyr::select("subject_id", "cohort_start_date") |>
        dplyr::rename("person_id" = "subject_id") |>
        dplyr::distinct()
    } else {
      cohortSubjects <- cdm[[cohortTable]] |>
        dplyr::filter(.data$cohort_definition_id %in% cohortId) |>
        dplyr::select("subject_id", "cohort_start_date") |>
        dplyr::rename("person_id" = "subject_id") |>
        dplyr::distinct()
    }
  }

  if(length(tableName)>0){
    codeRecords <- cdm[[tableName[[1]]]]
    if(!is.null(cohortTable)){
      # keep only records of those in the cohorts of interest
      codeRecords <- codeRecords |>
        dplyr::inner_join(cohortSubjects,
                          by = "person_id")
      if(timing == "entry"){
        codeRecords <- codeRecords |>
          dplyr::filter(.data$cohort_start_date == !!dplyr::sym(dateName[[1]]))
      }
    }

    if(is.null(codeRecords)){
      return(NULL)
    }


    tableCodes <- paste0(omopgenerics::uniqueTableName(),
                            omopgenerics::uniqueId())
    cdm <- omopgenerics::insertTable(cdm = cdm,
                                     name = tableCodes,
                                     table = codes |>
                                       dplyr::filter(.data$table == !!tableName[[1]]) |>
                                       dplyr::select("concept_id", "domain_id"),
                                     overwrite = TRUE,
                                     temporary = FALSE)
    codeRecords <- codeRecords |>
      dplyr::mutate(date = !!dplyr::sym(dateName[[1]])) |>
      dplyr::mutate(year = clock::get_year(date)) |>
      dplyr::select(dplyr::all_of(c("person_id",
                                    standardConceptIdName[[1]],
                                    sourceConceptIdName[[1]],
                                    sourceConceptValueName[[1]],
                                    "date", "year"))) |>
      dplyr::rename("standard_concept_id" = .env$standardConceptIdName[[1]],
                    "source_concept_id" = .env$sourceConceptIdName[[1]],
                    "source_concept_value" = .env$sourceConceptValueName[[1]]) |>
      dplyr::inner_join(cdm[[tableCodes]],
                        by = c("standard_concept_id"="concept_id")) |>
      dplyr::compute(
        name = paste0(intermediateTable,"_grr"),
        temporary = FALSE,
        schema = attr(cdm, "write_schema"),
        overwrite = TRUE
      )

    CDMConnector::dropTable(cdm = cdm, name = tableCodes)
    cdm[[tableCodes]] <- NULL

  } else {
    return(NULL)
  }

  # get for any additional domains and union
  if(length(tableName) > 1) {
    for(i in 1:(length(tableName)-1)) {
      workingRecords <-  cdm[[tableName[[i+1]]]]
      if(!is.null(cohortTable)){
        # keep only records of those in the cohorts of interest
        workingRecords <- workingRecords |>
          dplyr::inner_join(cohortSubjects,
                            by = "person_id")
        if(timing == "entry"){
          workingRecords <- workingRecords |>
            dplyr::filter(.data$cohort_start_date == !!dplyr::sym(dateName[[i+1]]))
        }
      }

      tmpTblName <- omopgenerics::uniqueTableName()
      cdm <- omopgenerics::insertTable(cdm = cdm,
                                       name = tmpTblName,
                                       table = codes |>
                                         dplyr::filter(.data$table == tableName[[i+1]]) |>
                                         dplyr::select("concept_id", "domain_id"),
                                       overwrite = TRUE,
                                       temporary = FALSE)
      workingRecords <-  workingRecords |>
        dplyr::mutate(date = !!dplyr::sym(dateName[[i+1]])) |>
        dplyr::mutate(year = clock::get_year(date)) |>
        dplyr::select(dplyr::all_of(c("person_id",
                                      standardConceptIdName[[i+1]],
                                      sourceConceptIdName[[i+1]],
                                      "date", "year"))) |>
        dplyr::rename("standard_concept_id" = .env$standardConceptIdName[[i+1]],
                      "source_concept_id" = .env$sourceConceptIdName[[i+1]]) |>
        dplyr::inner_join(cdm[[tmpTblName]],
                          by = c("standard_concept_id"="concept_id"))

      if(workingRecords |> utils::head(1) |> dplyr::tally() |> dplyr::pull("n") >0){
        codeRecords <- codeRecords |>
          dplyr::union_all(workingRecords)  |>
          dplyr::compute(
            name = paste0(intermediateTable,"_grr_i"),
            temporary = FALSE,
            schema = attr(cdm, "write_schema"),
            overwrite = TRUE
          )
      }

   omopgenerics::dropSourceTable(cdm = cdm, name = tmpTblName)
   }
  }

  if(codeRecords |> utils::head(1) |> dplyr::tally() |> dplyr::pull("n") >0){
    codeRecords <- codeRecords |>
      dplyr::left_join(cdm[["concept"]] |>
                         dplyr::select("concept_id", "concept_name"),
                       by = c("standard_concept_id"="concept_id")) |>
      dplyr::rename("standard_concept_name"="concept_name") |>
      dplyr::left_join(cdm[["concept"]] |>
                         dplyr::select("concept_id", "concept_name"),
                       by = c("source_concept_id"="concept_id")) |>
      dplyr::rename("source_concept_name"="concept_name")  |>
      dplyr::mutate(source_concept_name = dplyr::if_else(is.na(.data$source_concept_name),
                                                         "NA", .data$source_concept_name)) |>
      dplyr::compute(
        name = paste0(intermediateTable,"_grr_cr"),
        temporary = FALSE,
        schema = attr(cdm, "write_schema"),
        overwrite = TRUE
      )
  }

  return(codeRecords)

}

filterDateRange <- function(records, cdm, dateRange){
  dateRange <- getDateRange(cdm, dateRange)
  records |>
    dplyr::filter(.data$date >= !!dateRange[1],
                  .data$date <= !!dateRange[2])
}

getSummaryCounts <- function(records,
                             cdm,
                             countBy,
                             byConcept,
                             byYear,
                             bySex,
                             byAgeGroup) {
  if ("record" %in% countBy) {
    recordSummary <- records |>
      dplyr::tally(name = "estimate_value") |>
      dplyr::mutate(estimate_value = as.character(.data$estimate_value)) |>
      dplyr::collect()
    if(isTRUE(byConcept)) {
      recordSummary <- dplyr::bind_rows(
        recordSummary,
        records |>
          dplyr::group_by(
            .data$standard_concept_id, .data$standard_concept_name,
            .data$source_concept_id, .data$source_concept_name,
            .data$source_concept_value, .data$domain_id
          ) |>
          dplyr::tally(name = "estimate_value") |>
          dplyr::mutate(estimate_value = as.character(.data$estimate_value)) |>
          dplyr::collect()
      )
    }else{
      recordSummary <- recordSummary |>
        dplyr::mutate("standard_concept_id" = NA_integer_,
                      "standard_concept_name" = NA_character_,
                      "source_concept_id" = NA_integer_,
                      "source_concept_name" = NA_character_,
                      "source_concept_value" = NA_character_,
                      "domain_id" = NA_character_)
    }
    recordSummary <- recordSummary |>
      dplyr::mutate(
        strata_name = "overall",
        strata_level = "overall",
        estimate_name = "record_count"
      )
  } else {
    recordSummary <- dplyr::tibble()
  }

  if ("person" %in% countBy) {
    personSummary <- records |>
      dplyr::select("person_id") |>
      dplyr::distinct() |>
      dplyr::tally(name = "estimate_value") |>
      dplyr::mutate(estimate_value = as.character(.data$estimate_value)) |>
      dplyr::collect()

    if (isTRUE(byConcept)) {
      personSummary <- dplyr::bind_rows(
        personSummary,
        records |>
          dplyr::select(
            "person_id", "standard_concept_id", "standard_concept_name",
            "source_concept_id", "source_concept_name",
            "source_concept_value", "domain_id"
          ) |>
          dplyr::distinct() |>
          dplyr::group_by(
            .data$standard_concept_id, .data$standard_concept_name,
            .data$source_concept_id, .data$source_concept_name,
            .data$source_concept_value, .data$domain_id
          ) |>
          dplyr::tally(name = "estimate_value") |>
          dplyr::mutate(estimate_value = as.character(.data$estimate_value)) |>
          dplyr::collect()
      )
    }
    personSummary <- personSummary |>
      dplyr::mutate(
        strata_name = "overall",
        strata_level = "overall",
        estimate_name = "person_count")
  } else {
    personSummary <- dplyr::tibble()
  }


  if ("record" %in% countBy & byYear == TRUE) {
    recordSummary <- dplyr::bind_rows(
      recordSummary,
      getGroupedRecordCount(records = records, cdm = cdm, groupBy = "year")
    )
  }
  if ("person" %in% countBy & byYear == TRUE) {
    personSummary <- dplyr::bind_rows(
      personSummary,
      getGroupedPersonCount(records = records, cdm = cdm, groupBy = "year")
    )
  }
  if ("record" %in% countBy & bySex == TRUE) {
    recordSummary <- dplyr::bind_rows(
      recordSummary,
      getGroupedRecordCount(records = records, cdm = cdm, groupBy = "sex")
    )
  }
  if ("person" %in% countBy & bySex == TRUE) {
    personSummary <- dplyr::bind_rows(
      personSummary,
      getGroupedPersonCount(records = records, cdm = cdm, groupBy = "sex")
    )
  }
  if ("record" %in% countBy & byAgeGroup == TRUE) {
    recordSummary <- dplyr::bind_rows(
      recordSummary,
      getGroupedRecordCount(records = records, cdm = cdm, groupBy = "age_group")
    )
  }
  if ("person" %in% countBy & byAgeGroup == TRUE) {
    personSummary <- dplyr::bind_rows(
      personSummary,
      getGroupedPersonCount(records = records, cdm = cdm, groupBy = "age_group")
    )
  }
  if ("record" %in% countBy && byAgeGroup == TRUE && bySex == TRUE) {
    recordSummary <- dplyr::bind_rows(
      recordSummary,
      getGroupedRecordCount(records = records, cdm = cdm, groupBy = c("age_group", "sex"))
    )
  }
  if ("person" %in% countBy && byAgeGroup == TRUE && bySex == TRUE) {
    personSummary <- dplyr::bind_rows(
      personSummary,
      getGroupedPersonCount(records = records, cdm = cdm, groupBy = c("age_group", "sex"))
    )
  }

  summary <- dplyr::bind_rows(recordSummary, personSummary)
  return(summary)
}

getGroupedRecordCount <- function(records,
                                  cdm,
                                  groupBy){

  groupedCounts <- dplyr::bind_rows(
    records |>
      dplyr::group_by(dplyr::pick(.env$groupBy)) |>
      dplyr::tally(name = "estimate_value") |>
      dplyr::mutate(estimate_value = as.character(.data$estimate_value)) |>
      dplyr::collect(),
    records |>
      dplyr::group_by(dplyr::pick(.env$groupBy,
                                  "standard_concept_id", "standard_concept_name",
                                  "source_concept_id", "source_concept_name",
                                  "source_concept_value", "domain_id")) |>
      dplyr::tally(name = "estimate_value") |>
      dplyr::mutate(estimate_value = as.character(.data$estimate_value)) |>
      dplyr::collect()
    )  |>
    omopgenerics::uniteStrata(cols = groupBy) |>
    dplyr::mutate(estimate_name = "record_count")

  return(groupedCounts)

}

getGroupedPersonCount <- function(records,
                                  cdm,
                                  groupBy){

  groupedCounts <- dplyr::bind_rows(
    records |>
      dplyr::select(dplyr::all_of(c("person_id", .env$groupBy))) |>
      dplyr::distinct() |>
      dplyr::group_by(dplyr::pick(.env$groupBy)) |>
      dplyr::tally(name = "estimate_value") |>
      dplyr::mutate(estimate_value = as.character(.data$estimate_value)) |>
      dplyr::collect(),
    records |>
      dplyr::select(dplyr::all_of(c(
        "person_id", "standard_concept_id", "standard_concept_name",
        "source_concept_id", "source_concept_name",
        "source_concept_value", "domain_id", .env$groupBy
      ))) |>
      dplyr::distinct() |>
      dplyr::group_by(dplyr::pick(
        .env$groupBy, "standard_concept_id", "standard_concept_name",
        "source_concept_id", "source_concept_name",
        "source_concept_value", "domain_id"
      )) |>
      dplyr::tally(name = "estimate_value") |>
      dplyr::mutate(estimate_value = as.character(.data$estimate_value)) |>
      dplyr::collect()) |>
    omopgenerics::uniteStrata(cols = groupBy) |>
    dplyr::mutate(estimate_name = "person_count")

  return(groupedCounts)

}

checkCategory <- function(category, overlap = FALSE) {
  omopgenerics::assertList(category, unique = T)

  if (is.null(names(category))) {
    names(category) <- rep("", length(category))
  }

  # check length
  category <- lapply(category, function(x) {
    if (length(x) == 1) {
      x <- c(x, x)
    } else if (length(x) > 2) {
      cli::cli_abort(
        paste0(
          "Categories should be formed by a lower bound and an upper bound, ",
          "no more than two elements should be provided."
        ),
        call. = FALSE
      )
    }
    return(x)
  })

  # check lower bound is smaller than upper bound
  checkLower <- unlist(lapply(category, function(x) {
    x[1] <= x[2]
  }))
  if (!(all(checkLower))) {
    cli::cli_abort("Lower bound should be equal or smaller than upper bound")
  }

  # built tibble
  result <- lapply(category, function(x) {
    dplyr::tibble(lower_bound = x[1], upper_bound = x[2])
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(category_label = names(.env$category)) |>
    dplyr::mutate(category_label = dplyr::if_else(
      .data$category_label == "",
      paste0(.data$lower_bound, " to ", .data$upper_bound),
      .data$category_label
    )) |>
    dplyr::arrange(.data$lower_bound)

  # check overlap
  if(!overlap) {
    if (nrow(result) > 1) {
      lower <- result$lower_bound[2:nrow(result)]
      upper <- result$upper_bound[1:(nrow(result) - 1)]
      if (!all(lower > upper)) {
        cli::cli_abort("There can not be overlap between categories")
      }
    }
  }

  return(result)
}

checkAgeGroup <- function(ageGroup, overlap = FALSE) {
  omopgenerics::assertList(ageGroup, null = TRUE)
  if (!is.null(ageGroup)) {
    if (is.numeric(ageGroup[[1]])) {
      ageGroup <- list("age_group" = ageGroup)
    }
    for (k in seq_along(ageGroup)) {
      invisible(checkCategory(ageGroup[[k]], overlap))
    }
    if (is.null(names(ageGroup))) {
      names(ageGroup) <- paste0("age_group_", 1:length(ageGroup))
    }
    if ("" %in% names(ageGroup)) {
      id <- which(names(ageGroup) == "")
      names(ageGroup)[id] <- paste0("age_group_", id)
    }
  }
  return(ageGroup)
}

getDateRange <- function(cdm, dateRange) {

  if(is.na(dateRange[1])){
    obs_date_range <- cdm[["observation_period"]] |>
      dplyr::summarise(
        min_start = as.Date(min(.data$observation_period_start_date, na.rm = TRUE))
      ) |>
      dplyr::collect()
    dateRange[1] <- obs_date_range$min_start
  }

  if(is.na(dateRange[2])){
    obs_date_range <- cdm[["observation_period"]] |>
      dplyr::summarise(
        max_end = as.Date(max(.data$observation_period_end_date, na.rm = TRUE))
      ) |>
      dplyr::collect()
    dateRange[2] <- obs_date_range$max_end
  }

  return(dateRange)
}
