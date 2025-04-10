## ----include = FALSE----------------------------------------------------------
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%",
  eval = NOT_CRAN
)

## ----message=FALSE, warning=FALSE---------------------------------------------
#  # Loading necessary files
#  library(omopgenerics)
#  library(CodelistGenerator)
#  library(dplyr)
#  library(jsonlite)
#  
#  # Creating mock cdm
#  cdm <- mockVocabRef()
#  
#  # Reading mock json files
#  arthritis_desc <- fromJSON(system.file("concepts_for_mock/arthritis_desc.json", package = "CodelistGenerator")) |> toJSON(pretty = TRUE, auto_unbox = TRUE)
#  arthritis_no_desc <- fromJSON(system.file("concepts_for_mock/arthritis_no_desc.json", package = "CodelistGenerator")) |> toJSON(pretty = TRUE, auto_unbox = TRUE)
#  arthritis_with_excluded <- fromJSON(system.file("concepts_for_mock/arthritis_with_excluded.json", package = "CodelistGenerator")) |> toJSON(pretty = TRUE, auto_unbox = TRUE)
#  arthritis_desc_cohort <- fromJSON(system.file("cohorts_for_mock/oa_desc.json", package = "CodelistGenerator")) |> toJSON(pretty = TRUE, auto_unbox = TRUE)

## ----echo=FALSE---------------------------------------------------------------
#  knitr::include_graphics("Figures/1.png")

## ----message=FALSE, warning=FALSE---------------------------------------------
#  concepts <- codesFromConceptSet(cdm,
#                      path =  system.file(package = "CodelistGenerator","concepts_for_mock"),
#                      type = "codelist_with_details")

## -----------------------------------------------------------------------------
#  concepts

## -----------------------------------------------------------------------------
#  arthritis_desc

## ----message=FALSE------------------------------------------------------------
#  concepts$arthritis_desc

## -----------------------------------------------------------------------------
#  arthritis_no_desc
#  
#  concepts$arthritis_no_desc

## -----------------------------------------------------------------------------
#  arthritis_with_excluded
#  
#  concepts$arthritis_with_excluded

## ----message=FALSE, warning=FALSE---------------------------------------------
#  concepts <- codesFromCohort(cdm,
#                      path =  system.file(package = "CodelistGenerator","cohorts_for_mock"),
#                      type = "codelist_with_details")
#  concepts <- newCodelistWithDetails(list("arthritis" = concepts$arthritis))

## -----------------------------------------------------------------------------
#  arthritis_desc_cohort
#  
#  concepts$arthritis

