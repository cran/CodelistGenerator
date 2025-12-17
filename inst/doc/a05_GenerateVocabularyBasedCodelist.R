## ----include = FALSE----------------------------------------------------------
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = NOT_CRAN
)

CDMConnector::requireEunomia("synpuf-1k", "5.3")

## ----message=FALSE, warning=FALSE---------------------------------------------
# library(DBI)
# library(duckdb)
# library(dplyr)
# library(CDMConnector)
# library(CodelistGenerator)
# 
# # Connect to the database and create the cdm object
# con <- dbConnect(duckdb(),
#                  eunomiaDir("synpuf-1k", "5.3"))
# cdm <- cdmFromCon(con = con,
#                   cdmName = "Eunomia Synpuf",
#                   cdmSchema = "main",
#                   writeSchema = "main",
#                   achillesSchema = "main")

## -----------------------------------------------------------------------------
# availableDrugIngredients(cdm) |> glimpse()

## -----------------------------------------------------------------------------
# acetaminophen_codes <- getDrugIngredientCodes(
#   cdm = cdm,
#   name = c("acetaminophen")
# )
# 
# acetaminophen_codes

## -----------------------------------------------------------------------------
# acetaminophen_codes <- getDrugIngredientCodes(
#   cdm = cdm,
#   name = 1125315
# )
# 
# acetaminophen_codes

## -----------------------------------------------------------------------------
# acetaminophen_two_or_more_ingredients <- getDrugIngredientCodes(
#   cdm = cdm,
#   name = "acetaminophen",
#   ingredientRange = c(2, Inf),
#   type = "codelist_with_details"
# )
# 
# acetaminophen_two_or_more_ingredients
# 
# acetaminophen_two_or_more_ingredients[[1]] |>
#   pull("concept_name") |>
#   head(n = 5) # Only the first five will be shown

## -----------------------------------------------------------------------------
# acetaminophen_one_ingredient <- getDrugIngredientCodes(
#   cdm = cdm,
#   name = "acetaminophen",
#   ingredientRange = c(1, 1),
#   type = "codelist_with_details"
# )
# 
# acetaminophen_one_ingredient
# 
# acetaminophen_one_ingredient[[1]] |>
#   pull("concept_name") |>
#   head(n = 5) # Only the first five will be shown

## -----------------------------------------------------------------------------
# availableDoseForms(cdm) |> glimpse()

## -----------------------------------------------------------------------------
# acetaminophen_injections <- getDrugIngredientCodes(
#   cdm = cdm,
#   name = "acetaminophen",
#   doseForm = "injection",
#   type = "codelist_with_details"
# )
# 
# acetaminophen_injections[[1]] |>
#   pull("concept_name") |>
#   head(n = 5)

## -----------------------------------------------------------------------------
# availableDoseUnits(cdm) |> glimpse()

## -----------------------------------------------------------------------------
# acetaminophen_miligram <- getDrugIngredientCodes(
#   cdm = cdm,
#   name = "acetaminophen",
#   doseUnit = "milligram",
#   type = "codelist_with_details"
# )
# 
# acetaminophen_miligram[[1]] |>
#   pull("concept_name") |>
#   head(n = 5)

## -----------------------------------------------------------------------------
# availableRouteCategories(cdm) |> glimpse()

## -----------------------------------------------------------------------------
# acetaminophen_inhalable <- getDrugIngredientCodes(
#   cdm = cdm,
#   name = "acetaminophen",
#   routeCategory = "inhalable",
#   type = "codelist_with_details"
# )
# 
# acetaminophen_inhalable[[1]] |>
#   pull("concept_name") |>
#   head(n = 5)

## -----------------------------------------------------------------------------
# acetaminophen_heparin_codes <- getDrugIngredientCodes(
#   cdm = cdm,
#   name = c("acetaminophen", "heparin")
#   )
# 
# acetaminophen_heparin_codes

## -----------------------------------------------------------------------------
# availableATC(cdm, level = c("ATC 1st")) |> glimpse()
# availableATC(cdm, level = c("ATC 2nd")) |> glimpse()
# availableATC(cdm, level = c("ATC 3rd")) |> glimpse()
# availableATC(cdm, level = c("ATC 4th")) |> glimpse()
# availableATC(cdm, level = c("ATC 5th")) |> glimpse()

## -----------------------------------------------------------------------------
# atc_codelist <- getATCCodes(
#   cdm = cdm,
#   level = "ATC 1st",
#   name = "alimentary tract and metabolism"
# )
# 
# atc_codelist

