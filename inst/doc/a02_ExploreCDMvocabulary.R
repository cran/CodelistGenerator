## ----include = FALSE----------------------------------------------------------
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = NOT_CRAN
)

## ----include = FALSE----------------------------------------------------------
#  CDMConnector::requireEunomia("synpuf-1k", "5.3")

## ----warning=FALSE, message=FALSE---------------------------------------------
#  library(DBI)
#  library(dplyr)
#  library(CDMConnector)
#  library(CodelistGenerator)
#  
#  # Connect to the database and create the cdm object
#  con <- dbConnect(duckdb::duckdb(),
#                        eunomiaDir("synpuf-1k", "5.3"))
#  cdm <- cdmFromCon(con = con,
#                    cdmName = "Eunomia Synpuf",
#                    cdmSchema   = "main",
#                    writeSchema = "main",
#                    achillesSchema = "main")

## -----------------------------------------------------------------------------
#  getVocabVersion(cdm)

## -----------------------------------------------------------------------------
#  getVocabularies(cdm)

## -----------------------------------------------------------------------------
#  getDomains(cdm)

## -----------------------------------------------------------------------------
#  getDomains(cdm,
#             standardConcept = "Standard")

## -----------------------------------------------------------------------------
#  getConceptClassId(cdm)

## -----------------------------------------------------------------------------
#  getConceptClassId(cdm,
#                    standardConcept = "Non-standard",
#                    domain = "Condition")

## -----------------------------------------------------------------------------
#  getRelationshipId(cdm)

## -----------------------------------------------------------------------------
#  getRelationshipId(cdm,
#                    standardConcept1 = "standard",
#                    standardConcept2 = "standard",
#                    domains1 = "observation",
#                    domains2 = "observation")

## -----------------------------------------------------------------------------
#  result <- sourceCodesInUse(cdm)
#  head(result, n = 5) # Only the first 5 will be shown

## -----------------------------------------------------------------------------
#  result <- sourceCodesInUse(cdm, table = c("device_exposure", "condition_occurrence"))
#  head(result, n = 5) # Only the first 5 will be shown

