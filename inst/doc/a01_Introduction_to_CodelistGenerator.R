## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----  message=FALSE, warning=FALSE,echo=FALSE--------------------------------
library(here)
library(readr)
library(DBI)
library(here)
library(dplyr)
library(stringr)
library(DT)
library(kableExtra)
library(CodelistGenerator)
library(CDMConnector)

## ---- eval=FALSE--------------------------------------------------------------
#  # example with postgres database connection details
#  db <- DBI::dbConnect(RPostgres::Postgres(),
#    dbname = Sys.getenv("server"),
#    port = Sys.getenv("port"),
#    host = Sys.getenv("host"),
#    user = Sys.getenv("user"),
#    password = Sys.getenv("password")
#  )
#  # name of vocabulary schema
#  vocabularyDatabaseSchema <- Sys.getenv("vocabulary_schema")
#  
#  # create cdm reference
#  cdm <- CDMConnector::cdm_from_con(
#    con = db,
#    cdm_schema = vocabularyDatabaseSchema,
#    cdm_tables = tidyselect::all_of(c(
#      "concept",
#      "concept_relationship",
#      "concept_ancestor",
#      "concept_synonym",
#      "vocabulary"
#    ))
#  )
#  # vocab to arrow
#  # save in temp folder for this example
#  dOut <- here(tempdir(), "db_vocab")
#  dir.create(dOut)
#  CDMConnector::stow(cdm, dOut)
#  
#  # new cdm reference using arrow
#  cdmArrow <- CDMConnector::cdm_from_files(
#    path = dOut,
#    cdm_tables = tidyselect::all_of(c(
#      "concept",
#      "concept_relationship",
#      "concept_ancestor",
#      "concept_synonym",
#      "vocabulary"
#    )),
#    as_data_frame = FALSE
#  )
#  
#  rm(cdm)

## ---- eval=FALSE--------------------------------------------------------------
#  getVocabVersion(cdm = cdmArrow)

## ----  message=FALSE, warning=FALSE,echo=FALSE--------------------------------
vocabVersion <- readRDS(here("vignettes", "introVocab.RData"))
vocabVersion

## ---- eval=FALSE--------------------------------------------------------------
#  codesFromDescendants <- tbl(
#    db,
#    sql(paste0(
#      "SELECT * FROM ",
#      vocabularyDatabaseSchema,
#      ".concept_ancestor"
#    ))
#  ) %>%
#    filter(ancestor_concept_id == "4182210") %>%
#    select("descendant_concept_id") %>%
#    rename("concept_id" = "descendant_concept_id") %>%
#    left_join(tbl(db, sql(paste0(
#      "SELECT * FROM ",
#      vocabularyDatabaseSchema,
#      ".concept"
#    )))) %>%
#    select(
#      "concept_id", "concept_name",
#      "domain_id", "vocabulary_id"
#    ) %>%
#    collect()

## ----  message=FALSE, warning=FALSE,echo=FALSE--------------------------------
codesFromDescendants <- readRDS(here("vignettes", "introData01.RData"))

## ----  message=FALSE, warning=FALSE-------------------------------------------
datatable(codesFromDescendants,
  rownames = FALSE,
  options = list(
    pageLength = 10,
    lengthMenu = c(10, 20, 50)
  )
)

## ---- eval=FALSE--------------------------------------------------------------
#  dementiaCodes1 <- getCandidateCodes(
#    cdm = cdm,
#    keywords = "dementia",
#    domains = "Condition",
#    includeDescendants = TRUE
#  )

## ----  message=FALSE, warning=FALSE,echo=FALSE--------------------------------
dementiaCodes1 <- readRDS(here("vignettes", "introData02.RData"))

## ----  message=FALSE, warning=FALSE-------------------------------------------
datatable(dementiaCodes1,
  rownames = FALSE,
  options = list(
    pageLength = 10,
    lengthMenu = c(10, 20, 50)
  )
)

## ----  eval=FALSE-------------------------------------------------------------
#  codeComparison <- compareCodelists(
#    codesFromDescendants,
#    dementiaCodes1
#  )

## ----  message=FALSE, warning=FALSE,echo=FALSE--------------------------------
codeComparison <- readRDS(here("vignettes", "introData03.RData"))

## ----  message=FALSE, warning=FALSE-------------------------------------------
kable(codeComparison %>%
  group_by(codelist) %>%
  tally())

## ----  message=FALSE, warning=FALSE-------------------------------------------
datatable(
  codeComparison %>%
    filter(codelist == "Only codelist 2"),
  rownames = FALSE,
  options = list(
    pageLength = 10,
    lengthMenu = c(10, 20, 50)
  )
)

## ----  message=FALSE, warning=FALSE,echo=FALSE--------------------------------
icdMappings <- readRDS(here("vignettes", "introData04.RData"))

## ----  eval=FALSE-------------------------------------------------------------
#  icdMappings <- getMappings(
#    cdm = cdm,
#    candidateCodelist = dementiaCodes1,
#    nonStandardVocabularies = "ICD10CM"
#  )

## ----  message=FALSE, warning=FALSE-------------------------------------------
datatable(icdMappings,
  rownames = FALSE,
  options = list(
    pageLength = 10,
    lengthMenu = c(10, 20, 50)
  )
)

## ----  message=FALSE, warning=FALSE,echo=FALSE--------------------------------
readMappings <- readRDS(here("vignettes", "introData05.RData"))

## ----  eval=FALSE-------------------------------------------------------------
#  readMappings <- getMappings(
#    cdm = cdm,
#    candidateCodelist = dementiaCodes1,
#    nonStandardVocabularies = "Read"
#  )

## ----  message=FALSE, warning=FALSE-------------------------------------------
datatable(readMappings,
  rownames = FALSE,
  options = list(
    pageLength = 10,
    lengthMenu = c(10, 20, 50)
  )
)

