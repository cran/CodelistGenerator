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
#  library(DBI)
#  library(RPostgres)

## ----eval=FALSE---------------------------------------------------------------
#  # postgres database connection details
#  serverDbi <- Sys.getenv("server")
#  user <- Sys.getenv("user")
#  password <- Sys.getenv("password")
#  port <- Sys.getenv("port")
#  host <- Sys.getenv("host")
#  
#  db <- dbConnect(RPostgres::Postgres(),
#    dbname = serverDbi,
#    port = port,
#    host = host,
#    user = user,
#    password = password
#  )
#  
#  # name of vocabulary schema
#  vocabularyDatabaseSchema <- "vocabulary"
#  
#  # create cdm reference
#  cdm <- CDMConnector::cdm_from_con(
#    con = db,
#    cdm_schema = vocabularyDatabaseSchema
#  )

## ----message=FALSE, warning=FALSE,echo=FALSE----------------------------------
oaCodes1 <- readRDS(system.file("optionsData01.RData", 
                                            package = "CodelistGenerator"))

## ----eval=FALSE---------------------------------------------------------------
#  oaCodes1 <- getCandidateCodes(
#    cdm = cdm,
#    keywords = "osteoarthritis",
#    domains = "Condition",
#    searchInSynonyms = FALSE,
#    searchNonStandard = FALSE,
#    exclude = c(
#      "post-infection",
#      "post-traumatic"
#    ),
#    includeDescendants = FALSE,
#    includeAncestor = FALSE
#  )

## ----message=FALSE, warning=FALSE---------------------------------------------
oaCodes1 |> 
  glimpse()

## ----message=FALSE, warning=FALSE,echo=FALSE----------------------------------
oaCodes2 <- readRDS(system.file("optionsData02.RData", 
                                            package = "CodelistGenerator"))

## ----eval=FALSE---------------------------------------------------------------
#  oaCodes2 <- getCandidateCodes(
#    cdm = cdm,
#    keywords = "osteoarthritis",
#    domains = "Condition",
#    searchInSynonyms = FALSE,
#    searchNonStandard = FALSE,
#    exclude = c(
#      "post-infection",
#      "post-traumatic"
#    ),
#    includeDescendants = TRUE,
#    includeAncestor = FALSE
#  )

## ----message=FALSE, warning=FALSE---------------------------------------------
newCodes1To2 <- compareCodelists(oaCodes1, oaCodes2) |>
  filter(codelist == "Only codelist 2") |>
  select(-"codelist")

newCodes1To2 |> 
  glimpse()

## ----message=FALSE, warning=FALSE,echo=FALSE----------------------------------
oaCodes3 <- readRDS(system.file("optionsData03.RData", 
                                            package = "CodelistGenerator"))

## ----eval=FALSE---------------------------------------------------------------
#  oaCodes3 <- getCandidateCodes(
#    cdm = cdm,
#    keywords = "osteoarthritis",
#    domains = c("Condition", "Observation"),
#    searchInSynonyms = FALSE,
#    searchNonStandard = FALSE,
#    exclude = c(
#      "post-infection",
#      "post-traumatic"
#    ),
#    includeDescendants = FALSE,
#    includeAncestor = FALSE
#  )

## ----message=FALSE, warning=FALSE---------------------------------------------
newCodes1To3 <- compareCodelists(oaCodes1, oaCodes3) |>
  filter(codelist == "Only codelist 2") |>
  select(-"codelist")

newCodes1To3 |> 
  glimpse()

## ----message=FALSE, warning=FALSE,echo=FALSE----------------------------------
oaCodes4 <- readRDS(system.file("optionsData04.RData", 
                                            package = "CodelistGenerator"))

## ----eval=FALSE---------------------------------------------------------------
#  oaCodes4 <- getCandidateCodes(
#    cdm = cdm,
#    keywords = "osteoarthritis",
#    domains = "Condition",
#    searchInSynonyms = TRUE,
#    searchNonStandard = FALSE,
#    exclude = c(
#      "post-infection",
#      "post-traumatic"
#    ),
#    includeDescendants = FALSE,
#    includeAncestor = FALSE
#  )

## ----message=FALSE, warning=FALSE---------------------------------------------
newCodes1To4 <- compareCodelists(oaCodes1, oaCodes4) |>
  filter(codelist == "Only codelist 2") |>
  select(-"codelist")

newCodes1To4 |> 
  glimpse()

## ----message=FALSE, warning=FALSE,echo=FALSE----------------------------------
oaCodes5 <- readRDS(system.file("optionsData05.RData", 
                                            package = "CodelistGenerator"))

## ----eval=FALSE---------------------------------------------------------------
#  oaCodes5 <- getCandidateCodes(
#    cdm = cdm,
#    keywords = "osteoarthritis",
#    domains = "Condition",
#    searchInSynonyms = FALSE,
#    searchNonStandard = TRUE,
#    exclude = c(
#      "post-infection",
#      "post-traumatic"
#    ),
#    includeDescendants = FALSE,
#    includeAncestor = FALSE
#  )

## ----message=FALSE, warning=FALSE---------------------------------------------
newCodes1To5 <- compareCodelists(oaCodes1, oaCodes5) |>
  filter(codelist == "Only codelist 2") |>
  select(-"codelist")

newCodes1To5 |> 
  glimpse()

## ----message=FALSE, warning=FALSE,echo=FALSE----------------------------------
oaCodes8 <- readRDS(system.file("optionsData07.RData", 
                                            package = "CodelistGenerator"))

## ----eval=FALSE---------------------------------------------------------------
#  oaCodes8 <- getCandidateCodes(
#    cdm = cdm,
#    keywords = "osteoarthritis",
#    domains = "Condition",
#    searchInSynonyms = FALSE,
#    searchNonStandard = FALSE,
#    exclude = c(
#      "post-infection",
#      "post-traumatic"
#    ),
#    includeDescendants = FALSE,
#    includeAncestor = TRUE
#  )

## ----message=FALSE, warning=FALSE---------------------------------------------
newCodes1To8 <- compareCodelists(oaCodes1, oaCodes8) |>
  filter(codelist == "Only codelist 2") |>
  select(-"codelist")

newCodes1To8 |> 
  glimpse()

