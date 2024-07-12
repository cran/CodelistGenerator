## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(CDMConnector)
if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") Sys.setenv("EUNOMIA_DATA_FOLDER" = tempdir())
if (!dir.exists(Sys.getenv("EUNOMIA_DATA_FOLDER"))) dir.create(Sys.getenv("EUNOMIA_DATA_FOLDER"))
if (!eunomia_is_available()) downloadEunomiaData()

## ----message=FALSE, warning=FALSE, echo=FALSE---------------------------------
library(CDMConnector)
library(CodelistGenerator)
library(dplyr)
library(tidyr)

db <- DBI::dbConnect(duckdb::duckdb(), 
                     dbdir = CDMConnector::eunomia_dir())
cdm <- cdm_from_con(
  con = db,
  cdm_schema = "main", 
  write_schema = "main"
)

## -----------------------------------------------------------------------------
acetaminophen_codes <- getDrugIngredientCodes(
  cdm = cdm,
  name = "acetaminophen"
)

acetaminophen_codes
acetaminophen_codes$acetaminophen

## -----------------------------------------------------------------------------
acetaminophen_codes_with_details <- getDrugIngredientCodes(
  cdm = cdm,
  name = "acetaminophen",
  type = "codelist_with_details"
)

acetaminophen_codes_with_details

acetaminophen_codes_with_details[[1]] |> 
  glimpse()

## -----------------------------------------------------------------------------
acetaminophen_two_or_more_ingredients <- getDrugIngredientCodes(
  cdm = cdm,
  name = "acetaminophen",
  ingredientRange = c(2,Inf),
  type = "codelist_with_details"
)

acetaminophen_two_or_more_ingredients

acetaminophen_two_or_more_ingredients[[1]] |> 
  glimpse()

## -----------------------------------------------------------------------------
acetaminophen_one_ingredient <- getDrugIngredientCodes(
  cdm = cdm,
  name = "acetaminophen",
  ingredientRange = c(1,1),
  type = "codelist_with_details"
)

acetaminophen_one_ingredient

acetaminophen_one_ingredient[[1]] |> 
  glimpse()

## -----------------------------------------------------------------------------
acetaminophen_injections <- getDrugIngredientCodes(
  cdm = cdm,
  name = "acetaminophen",
  doseForm = "injection",
  type = "codelist_with_details"
)

acetaminophen_injections

## -----------------------------------------------------------------------------
acetaminophen_heparin_codes <- getDrugIngredientCodes(
  cdm = cdm,
  name = c("acetaminophen", "heparin")
  )

acetaminophen_heparin_codes

## -----------------------------------------------------------------------------
ingredient_codes <- getDrugIngredientCodes(cdm = cdm)
ingredient_codes

## -----------------------------------------------------------------------------
cdm_mock <- mockVocabRef()

## -----------------------------------------------------------------------------
atc_codelist <- getATCCodes(
  cdm = cdm_mock,
  level = "ATC 1st",
  name = "alimentary tract and metabolism"
)

atc_codelist

## -----------------------------------------------------------------------------
arthropathy_codes <- getICD10StandardCodes(
  cdm = cdm_mock,
  name = "arthropathies"
)
arthropathy_codes
arthropathy_codes$arthropathies

## -----------------------------------------------------------------------------
arthropathy_codes <- getICD10StandardCodes(
  cdm = cdm_mock,
  name = "arthropathies", 
  type = "codelist_with_details"
)
arthropathy_codes

arthropathy_codes[[1]]

