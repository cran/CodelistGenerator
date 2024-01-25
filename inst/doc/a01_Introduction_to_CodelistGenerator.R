## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE, warning=FALSE,echo=FALSE----------------------------------
library(DBI)
library(dplyr)
library(CodelistGenerator)
library(CDMConnector)

## ----eval=FALSE---------------------------------------------------------------
#  # example with postgres database connection details
#  db <- DBI::dbConnect(RPostgres::Postgres(),
#    dbname = Sys.getenv("server"),
#    port = Sys.getenv("port"),
#    host = Sys.getenv("host"),
#    user = Sys.getenv("user"),
#    password = Sys.getenv("password")
#  )
#  
#  # create cdm reference
#  cdm <- CDMConnector::cdm_from_con(
#    con = db,
#    cdm_schema = Sys.getenv("vocabulary_schema")
#  )

## ----eval=FALSE---------------------------------------------------------------
#  getVocabVersion(cdm = cdm)

## ----message=FALSE, warning=FALSE,echo=FALSE----------------------------------
vocabVersion <- load(system.file("introVocab.RData", 
                 package = "CodelistGenerator"))
vocabVersion

## ----eval=FALSE---------------------------------------------------------------
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

## ----message=FALSE, warning=FALSE,echo=FALSE----------------------------------
codesFromDescendants <- readRDS(system.file("introData01.RData", 
                                            package = "CodelistGenerator"))

## ----message=FALSE, warning=FALSE---------------------------------------------
codesFromDescendants %>% 
  glimpse()

## ----eval=FALSE---------------------------------------------------------------
#  dementiaCodes1 <- getCandidateCodes(
#    cdm = cdm,
#    keywords = "dementia",
#    domains = "Condition",
#    includeDescendants = TRUE
#  )

## ----message=FALSE, warning=FALSE,echo=FALSE----------------------------------
dementiaCodes1 <- readRDS(system.file("introData02.RData", 
                                            package = "CodelistGenerator"))

## ----message=FALSE, warning=FALSE---------------------------------------------
dementiaCodes1%>% 
  glimpse()

## ----eval=FALSE---------------------------------------------------------------
#  codeComparison <- compareCodelists(
#    codesFromDescendants,
#    dementiaCodes1
#  )

## ----message=FALSE, warning=FALSE,echo=FALSE----------------------------------
codeComparison <- readRDS(system.file("introData03.RData", 
                                            package = "CodelistGenerator"))

## ----message=FALSE, warning=FALSE---------------------------------------------
codeComparison %>%
  group_by(codelist) %>%
  tally()

## ----message=FALSE, warning=FALSE---------------------------------------------
codeComparison %>%
  filter(codelist == "Only codelist 2") %>% 
  glimpse()

## ----message=FALSE, warning=FALSE,echo=FALSE----------------------------------
icdMappings <- readRDS(system.file("introData04.RData", 
                                            package = "CodelistGenerator"))

## ----eval=FALSE---------------------------------------------------------------
#  icdMappings <- getMappings(
#    cdm = cdm,
#    candidateCodelist = dementiaCodes1,
#    nonStandardVocabularies = "ICD10CM"
#  )

## ----message=FALSE, warning=FALSE---------------------------------------------
icdMappings %>% 
  glimpse()

## ----message=FALSE, warning=FALSE,echo=FALSE----------------------------------
readMappings <- readRDS(system.file("introData05.RData", 
                                            package = "CodelistGenerator"))

## ----eval=FALSE---------------------------------------------------------------
#  readMappings <- getMappings(
#    cdm = cdm,
#    candidateCodelist = dementiaCodes1,
#    nonStandardVocabularies = "Read"
#  )

## ----message=FALSE, warning=FALSE---------------------------------------------
readMappings %>% 
  glimpse()

