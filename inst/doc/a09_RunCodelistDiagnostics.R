## ----include = FALSE----------------------------------------------------------
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = NOT_CRAN)

## ----include = FALSE----------------------------------------------------------
# CDMConnector::requireEunomia("synpuf-1k", "5.3")

## ----message=FALSE, warning=FALSE---------------------------------------------
# library(DBI)
# library(duckdb)
# library(dplyr)
# library(CDMConnector)
# library(CodelistGenerator)
# library(CohortConstructor)
# library(omopgenerics)
# 
# # Connect to the database and create the cdm object
# con <- dbConnect(duckdb(),
#                       eunomiaDir("synpuf-1k", "5.3"))
# cdm <- cdmFromCon(con = con,
#                   cdmName = "Eunomia Synpuf",
#                   cdmSchema   = "main",
#                   writeSchema = "main",
#                   achillesSchema = "main")
# 
# # Create a codelist for depression
# depression <- getCandidateCodes(cdm,
#                                 keywords = "depression")
# depression <- newCodelist(list("depression" = depression$concept_id))

## ----message=FALSE, warning=FALSE---------------------------------------------
# achilles_code_use <- summariseAchillesCodeUse(depression,
#                                               cdm,
#                                               countBy = c("record", "person"))

## ----message=FALSE, warning=FALSE---------------------------------------------
# tableAchillesCodeUse(achilles_code_use,
#                      type = "gt")

## ----message=FALSE, warning=FALSE---------------------------------------------
# code_use <- summariseCodeUse(depression,
#                              cdm,
#                              countBy = c("record", "person"),
#                              byYear  = FALSE,
#                              bySex   = FALSE,
#                              ageGroup =  list("<=50" = c(0,50), ">50" = c(51,Inf)),
#                              dateRange = as.Date(c("2010-01-01", "2020-01-01")))
# 
# tableCodeUse(code_use, type = "gt")

## ----message=FALSE, warning=FALSE---------------------------------------------
# orphan <- summariseOrphanCodes(depression, cdm)
# tableOrphanCodes(orphan, type = "gt")

## ----message=FALSE, warning=FALSE---------------------------------------------
# cdm[["depression"]] <- conceptCohort(cdm,
#                                      conceptSet = depression,
#                                      name = "depression")

## ----message=FALSE, warning=FALSE---------------------------------------------
# cohort_code_use <- summariseCohortCodeUse(cdm,
#                                           cohortTable = "depression",
#                                           countBy = c("record", "person"))
# tableCohortCodeUse(cohort_code_use)

## ----message=FALSE, warning=FALSE---------------------------------------------
# cohort_code_use <- summariseCohortCodeUse(cdm,
#                                           cohortTable = "depression",
#                                           countBy = c("record", "person"),
#                                           timing = "entry")
# tableCohortCodeUse(cohort_code_use)

## ----message=FALSE, warning=FALSE---------------------------------------------
# anxiety <- getCandidateCodes(cdm,
#                              keywords = "anxiety")
# anxiety <- newCodelist(list("anxiety" = anxiety$concept_id))
# 
# cohort_code_use <- summariseCohortCodeUse(cdm,
#                                           cohortTable = "depression",
#                                           x = anxiety,
#                                           countBy = c("record", "person"),
#                                           timing = "entry")
# tableCohortCodeUse(cohort_code_use)

## ----message=FALSE, warning=FALSE---------------------------------------------
# cohort_code_use <- summariseCohortCodeUse(cdm = cdm,
#                                           cohortTable = "depression",
#                                           countBy = c("record", "person"),
#                                           byYear = FALSE,
#                                           bySex = TRUE,
#                                           ageGroup = NULL)
# tableCohortCodeUse(cohort_code_use)

