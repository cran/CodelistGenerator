## ----include = FALSE----------------------------------------------------------
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = NOT_CRAN
)

## ----warning=FALSE, message=FALSE---------------------------------------------
#  library(DBI)
#  library(duckdb)
#  library(dplyr)
#  library(CDMConnector)
#  library(CodelistGenerator)

## ----warning=FALSE------------------------------------------------------------
#  requireEunomia()
#  db <- dbConnect(duckdb(), dbdir = eunomiaDir())
#  cdm <- cdmFromCon(db,
#                    cdmSchema = "main",
#                    writeSchema = "main",
#                    writePrefix = "cg_")
#  cdm

## -----------------------------------------------------------------------------
#  cdm$concept |> glimpse()
#  cdm$concept_relationship |> glimpse()
#  cdm$concept_ancestor |> glimpse()
#  cdm$concept_synonym |> glimpse()
#  cdm$drug_strength |> glimpse()

## -----------------------------------------------------------------------------
#  getVocabVersion(cdm)

## -----------------------------------------------------------------------------
#  getVocabularies(cdm)

## ----eval = FALSE-------------------------------------------------------------
#  library(readr)
#  library(DBI)
#  library(duckdb)
#  library(omopgenerics)
#  
#  vocab_folder <- here() # add path to directory
#  
#  # read in files
#  concept <- read_delim(here(vocab_folder, "CONCEPT.csv"),
#                        "\t",
#                        escape_double = FALSE, trim_ws = TRUE
#  )
#  concept_relationship <- read_delim(here(vocab_folder, "CONCEPT_RELATIONSHIP.csv"),
#                                     "\t",
#                                     escape_double = FALSE, trim_ws = TRUE
#  )
#  concept_ancestor <- read_delim(here(vocab_folder, "CONCEPT_ANCESTOR.csv"),
#                                 "\t",
#                                 escape_double = FALSE, trim_ws = TRUE
#  )
#  concept_synonym <- read_delim(here(vocab_folder, "CONCEPT_SYNONYM.csv"),
#                                "\t",
#                                escape_double = FALSE, trim_ws = TRUE
#  )
#  vocabulary <- read_delim(here(vocab_folder, "VOCABULARY.csv"), "\t",
#                           escape_double = FALSE, trim_ws = TRUE
#  )
#  
#  # write to duckdb
#  db <- dbConnect(duckdb(), here(vocab_folder,"vocab.duckdb"))
#  dbWriteTable(db, "concept", concept, overwrite = TRUE)
#  dbWriteTable(db, "concept_relationship", concept_relationship, overwrite = TRUE)
#  dbWriteTable(db, "concept_ancestor", concept_ancestor, overwrite = TRUE)
#  dbWriteTable(db, "concept_synonym", concept_synonym, overwrite = TRUE)
#  dbWriteTable(db, "vocabulary", vocabulary, overwrite = TRUE)
#  # add empty person and observation period tables
#  person_cols <- omopColumns("person")
#  person <- data.frame(matrix(ncol = length(person_cols), nrow = 0))
#  colnames(person) <- person_cols
#  dbWriteTable(db, "person", person, overwrite = TRUE)
#  observation_period_cols <- omopColumns("observation_period")
#  observation_period <- data.frame(matrix(ncol = length(observation_period_cols), nrow = 0))
#  colnames(observation_period) <- observation_period_cols
#  dbWriteTable(db, "observation_period", observation_period, overwrite = TRUE)
#  dbDisconnect(db)

## ----eval = FALSE-------------------------------------------------------------
#  db <- dbConnect(duckdb(), here(vocab_folder,"vocab.duckdb"))
#  cdm <- cdmFromCon(db, "main", "main", cdmName = "vocabularise", .softValidation = TRUE)

