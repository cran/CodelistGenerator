## ----include = FALSE----------------------------------------------------------
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = NOT_CRAN
)

## ----include = FALSE----------------------------------------------------------
# CDMConnector::requireEunomia("synpuf-1k", "5.3")

## ----warning=FALSE, message=FALSE---------------------------------------------
# library(DBI)
# library(dplyr)
# library(CDMConnector)
# library(CodelistGenerator)
# 
# # Connect to the database and create the cdm object
# con <- dbConnect(duckdb::duckdb(),
#                  eunomiaDir("synpuf-1k", "5.3"))
# cdm <- cdmFromCon(con = con,
#                   cdmName = "Eunomia Synpuf",
#                   cdmSchema   = "main",
#                   writeSchema = "main",
#                   achillesSchema = "main")

## -----------------------------------------------------------------------------
# vocabularyVersion(cdm)

## -----------------------------------------------------------------------------
# availableVocabularies(cdm)

## -----------------------------------------------------------------------------
# availableDomains(cdm)

## -----------------------------------------------------------------------------
# availableDomains(cdm,
#                  standardConcept = "Standard")

## -----------------------------------------------------------------------------
# availableConceptClassIds(cdm)

## -----------------------------------------------------------------------------
# availableConceptClassIds(cdm,
#                          standardConcept = "Non-standard",
#                          domain = "Condition")

## -----------------------------------------------------------------------------
# availableRelationshipIds(cdm)

## -----------------------------------------------------------------------------
# availableRelationshipIds(cdm,
#                          standardConcept1 = "Standard",
#                          standardConcept2 = "Standard",
#                          domains1 = "Observation",
#                          domains2 = "Observation")

