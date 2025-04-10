## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

CDMConnector::requireEunomia("synpuf-1k", "5.3")

## ----message=FALSE, warning=FALSE---------------------------------------------
library(DBI)
library(duckdb)
library(dplyr)
library(CDMConnector)
library(CodelistGenerator)

# Connect to the database and create the cdm object
con <- dbConnect(duckdb(), 
                 eunomiaDir("synpuf-1k", "5.3"))
cdm <- cdmFromCon(con = con, 
                  cdmName = "Eunomia Synpuf",
                  cdmSchema = "main",
                  writeSchema = "main",
                  achillesSchema = "main")

## -----------------------------------------------------------------------------
availableICD10(cdm, level = "ICD10 Chapter") |> 
  glimpse()
availableICD10(cdm, level = "ICD10 SubChapter") |> 
  glimpse()
availableICD10(cdm, level = "ICD10 Hierarchy") |> 
  glimpse()
availableICD10(cdm, level = "ICD10 Code") |> 
  glimpse()

## -----------------------------------------------------------------------------
icd_chapters <- getICD10StandardCodes(cdm = cdm,
                                      level = "ICD10 Chapter")
icd_chapters |> length()
icd_chapters

## -----------------------------------------------------------------------------
mental_and_behavioural_disorders <- getICD10StandardCodes(
  cdm = cdm,
  name = "Mental and behavioural disorders",
  level = "ICD10 Chapter"
)
mental_and_behavioural_disorders

## -----------------------------------------------------------------------------
icd_subchapters <- getICD10StandardCodes(
  cdm = cdm,
  level = "ICD10 SubChapter"
)
icd_subchapters |> length()
icd_subchapters

## -----------------------------------------------------------------------------
mood_affective_disorders <- getICD10StandardCodes(
  cdm = cdm,
  name = "Mood [affective] disorders", 
  level = "ICD10 SubChapter"
)
mood_affective_disorders

## -----------------------------------------------------------------------------
icd_hierarchy <- getICD10StandardCodes(
  cdm = cdm,
  level = "ICD10 Hierarchy"
)
icd_hierarchy |> length()
icd_hierarchy

## -----------------------------------------------------------------------------
persistent_mood_affective_disorders   <- getICD10StandardCodes(
  cdm = cdm,
  name = "Persistent mood [affective] disorders", 
  level = "ICD10 Hierarchy"
)
persistent_mood_affective_disorders

## -----------------------------------------------------------------------------
icd_code <- getICD10StandardCodes(
  cdm = cdm,
  level = "ICD10 Code"
)
icd_code |> length()
icd_hierarchy

## -----------------------------------------------------------------------------
dysthymia   <- getICD10StandardCodes(
  cdm = cdm,
  name = "dysthymia", 
  level = "ICD10 Code"
)
dysthymia

## -----------------------------------------------------------------------------
dysthymia_descendants <- getICD10StandardCodes(
  cdm = cdm,
  name = "dysthymia", 
  level = "ICD10 Code",
  includeDescendants = TRUE
)
dysthymia_descendants

dysthymia_no_descendants <- getICD10StandardCodes(
  cdm = cdm,
  name = "dysthymia", 
  level = "ICD10 Code",
  includeDescendants = FALSE
)
dysthymia_no_descendants

## -----------------------------------------------------------------------------
compareCodelists(dysthymia_no_descendants, 
                 dysthymia_descendants) |> 
  filter(codelist == "Both") |> 
  pull("concept_id")

compareCodelists(dysthymia_no_descendants, 
                 dysthymia_descendants) |> 
  filter(codelist == "Only codelist 2") |> 
  pull("concept_id")

## -----------------------------------------------------------------------------
dysthymia <- getICD10StandardCodes(
  cdm = cdm,
  name = "dysthymia", 
  level = "ICD10 Code",
  nameStyle = "{concept_name}"
)
dysthymia

## -----------------------------------------------------------------------------
dysthymia <- getICD10StandardCodes(
  cdm = cdm,
  name = "dysthymia", 
  level = "ICD10 Code",
  type = "codelist"
)
dysthymia[[1]] |> glimpse()

## -----------------------------------------------------------------------------
dysthymia <- getICD10StandardCodes(
  cdm = cdm,
  name = "dysthymia", 
  level = "ICD10 Code",
  type = "codelist_with_details"
)
dysthymia[[1]] |> glimpse()

