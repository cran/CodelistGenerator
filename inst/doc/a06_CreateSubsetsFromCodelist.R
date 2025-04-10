## ----include = FALSE----------------------------------------------------------
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = NOT_CRAN)

## ----include = FALSE----------------------------------------------------------
#  CDMConnector::requireEunomia("synpuf-1k", "5.3")

## ----warning=FALSE, message=FALSE---------------------------------------------
#  library(DBI)
#  library(duckdb)
#  library(dplyr)
#  library(CDMConnector)
#  library(CodelistGenerator)
#  
#  # Connect to the database and create the cdm object
#  con <- dbConnect(duckdb(),
#                        eunomiaDir("synpuf-1k", "5.3"))
#  cdm <- cdmFromCon(con = con,
#                    cdmName = "Eunomia Synpuf",
#                    cdmSchema   = "main",
#                    writeSchema = "main",
#                    achillesSchema = "main")

## ----warning=FALSE, message=FALSE---------------------------------------------
#  acetaminophen <- getDrugIngredientCodes(cdm,
#                                          name = "acetaminophen",
#                                          nameStyle = "{concept_name}",
#                                          type = "codelist")

## -----------------------------------------------------------------------------
#  acetaminophen_in_use <- subsetToCodesInUse(x = acetaminophen,
#                                             cdm,
#                                             minimumCount = 0,
#                                             table = "drug_exposure")
#  acetaminophen_in_use # Only the first 5 concepts will be shown

## ----warning=FALSE, messages=FALSE--------------------------------------------
#  acetaminophen_drug <- subsetOnDomain(acetaminophen_in_use, cdm, domain = "Drug")
#  
#  acetaminophen_drug

## ----warning=FALSE, messages=FALSE--------------------------------------------
#  acetaminophen_no_drug <- subsetOnDomain(acetaminophen_in_use, cdm, domain = "Drug", negate = TRUE)
#  
#  acetaminophen_no_drug

## ----warning=FALSE, messages=FALSE--------------------------------------------
#  acetaminophen_mg_unit <- subsetOnDoseUnit(acetaminophen_drug, cdm, c("milligram", "unit"))
#  acetaminophen_mg_unit

## ----warning=FALSE, messages=FALSE--------------------------------------------
#  acetaminophen_route <- subsetOnRouteCategory(acetaminophen_mg_unit,
#                                               cdm, c("transmucosal_rectal","unclassified_route"),
#                                               negate = TRUE)
#  acetaminophen_route

## ----warning=FALSE, messages=FALSE--------------------------------------------
#  acetaminophen_doses <- stratifyByDoseUnit(acetaminophen, cdm, keepOriginal = TRUE)
#  
#  acetaminophen_doses

## ----warning=FALSE, messages=FALSE--------------------------------------------
#  acetaminophen_routes <- stratifyByRouteCategory(acetaminophen, cdm)
#  
#  acetaminophen_routes

## ----warning=FALSE, messages=FALSE--------------------------------------------
#  acetaminophen <- getDrugIngredientCodes(cdm,
#                                             name = "acetaminophen",
#                                             nameStyle = "{concept_name}",
#                                             type = "codelist_with_details")
#  hydrocodone <- getDrugIngredientCodes(cdm,
#                                        name = "hydrocodone",
#                                        doseUnit = "milligram",
#                                        nameStyle = "{concept_name}",
#                                        type = "codelist_with_details")

## -----------------------------------------------------------------------------
#  comparison <- compareCodelists(acetaminophen$acetaminophen, hydrocodone$hydrocodone)
#  
#  comparison |> glimpse()
#  
#  comparison |> filter(codelist == "Both")

