## ----include = FALSE----------------------------------------------------------
  NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
  
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = NOT_CRAN)

## ----include = FALSE----------------------------------------------------------
#   CDMConnector::requireEunomia("synpuf-1k", "5.3")

## ----warning=FALSE, message=FALSE---------------------------------------------
# library(DBI)
# library(duckdb)
# library(dplyr)
# library(CDMConnector)
# library(CodelistGenerator)
# 
# # Download mock database
# requireEunomia(datasetName = "synpuf-1k", cdmVersion = "5.3")
# 
# # Connect to the database and create the cdm object
# con <- dbConnect(duckdb(), eunomiaDir("synpuf-1k", "5.3"))
# cdm <- cdmFromCon(con = con,
# cdmName = "Eunomia Synpuf",
# cdmSchema   = "main",
# writeSchema = "main",
# achillesSchema = "main")

## ----warning=FALSE, message=FALSE---------------------------------------------
# acetaminophen <- getDrugIngredientCodes(cdm,
#                                         name = "acetaminophen",
#                                         nameStyle = "{concept_name}",
#                                         type = "codelist")

## ----warning=FALSE, messages=FALSE--------------------------------------------
# acetaminophen_drug <- subsetOnDomain(acetaminophen,
#                                      cdm,
#                                      domain = "Drug")
# 
# acetaminophen_drug

## ----warning=FALSE, messages=FALSE--------------------------------------------
# acetaminophen_no_drug <- subsetOnDomain(acetaminophen,
#                                         cdm,
#                                         domain = "Drug",
#                                         negate = TRUE)
# 
# acetaminophen_no_drug

## ----warning=FALSE, messages=FALSE--------------------------------------------
# acetaminophen_rxnorm <- subsetOnVocabulary(acetaminophen_drug,
#                                            cdm,
#                                            c("RxNorm"))
# acetaminophen_rxnorm

## ----warning=FALSE, messages=FALSE--------------------------------------------
# acetaminophen_mg_unit <- subsetOnDoseUnit(acetaminophen_rxnorm,
#                                           cdm,
#                                           c("milligram", "unit"))
# acetaminophen_mg_unit

## ----warning=FALSE, messages=FALSE--------------------------------------------
# acetaminophen_ingredient <- subsetOnIngredientRange(acetaminophen_drug,
#                                                 cdm,
#                                                 ingredientRange = c(3, 30))
# 
# acetaminophen_ingredient

## ----warning=FALSE, messages=FALSE--------------------------------------------
# acetaminophen_route <- subsetOnRouteCategory(acetaminophen_mg_unit,
#                                              cdm,
#                                              c("transmucosal_rectal","unclassified_route"),
#                                              negate = TRUE)
# acetaminophen_route

## ----warning=FALSE, messages=FALSE--------------------------------------------
# # First, check which dose forms are available in our codelist
# acetaminophen_drug |>
#   associatedDoseForms(cdm)
# acetaminophen_oral <- subsetOnDoseForm(acetaminophen_drug,
#                                             cdm,
#                                             c("Oral Solution","Oral Capsule"))
# acetaminophen_oral

## ----warning=FALSE, messages=FALSE--------------------------------------------
# acetaminophen_doses <- stratifyByDoseUnit(acetaminophen, cdm, keepOriginal = TRUE)
# 
# acetaminophen_doses

## ----warning=FALSE, messages=FALSE--------------------------------------------
# acetaminophen_routes <- stratifyByRouteCategory(acetaminophen, cdm)
# 
# acetaminophen_routes

## ----warning=FALSE, messages=FALSE--------------------------------------------
# acetaminophen_dose_forms <- stratifyByDoseForm(acetaminophen, cdm)
# 
# acetaminophen_dose_forms

## ----warning=FALSE, messages=FALSE--------------------------------------------
# acetaminophen_routes1 <- addConcepts(acetaminophen_routes,
#                                      cdm,
#                                      concepts = c(1125315L))
# acetaminophen_routes1

## ----warning=FALSE, messages=FALSE--------------------------------------------
# x <- getDescendants(cdm = cdm, conceptId = c(1125315L))
# acetaminophen_routes2 <- addConcepts(acetaminophen_routes,
#                                      cdm,
#                                      concepts = x$concept_id,
#                                      codelistName = "acetaminophen_unclassified_route_category")
# acetaminophen_routes2

## ----warning=FALSE, messages=FALSE--------------------------------------------
# acetaminophen_routes3 <- excludeConcepts(acetaminophen_routes,
#                                          cdm,
#                                          concepts = x$concept_id,
#                                          codelistName = "acetaminophen_inhalable")
# acetaminophen_routes3

## ----warning=FALSE, messages=FALSE--------------------------------------------
# acetaminophen <- getDrugIngredientCodes(cdm,
#                                         name = "acetaminophen",
#                                         nameStyle = "{concept_name}",
#                                         type = "codelist")
# 
# new_codelist <- acetaminophen |>
#   addConcepts(cdm,
#               concepts = c(1L, 2L, 3L)) |>
#   subsetOnDomain(cdm,
#                  domain = "Drug") |>
#   stratifyByDoseUnit(cdm = cdm) |>
#   excludeConcepts(cdm,
#                   concepts = c(1127898))
# 
# new_codelist

## ----warning=FALSE, messages=FALSE--------------------------------------------
# acetaminophen <- getDrugIngredientCodes(cdm,
#                                         name = "acetaminophen",
#                                         nameStyle = "{concept_name}",
#                                         type = "codelist_with_details")
# hydrocodone <- getDrugIngredientCodes(cdm,
#                                       name = "hydrocodone",
#                                       doseUnit = "milligram",
#                                       nameStyle = "{concept_name}",
#                                       type = "codelist_with_details")

## -----------------------------------------------------------------------------
# comparison <- compareCodelists(acetaminophen,
#                                hydrocodone)
# 
# comparison |> glimpse()
# 
# comparison |> filter(codelist == "Both")

