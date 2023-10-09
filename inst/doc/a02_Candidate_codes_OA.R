## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE, warning=FALSE,echo=FALSE----------------------------------
library(here)
library(readr)
library(DBI)
library(here)
library(dplyr)
library(dbplyr)
library(stringr)
library(DT)
library(kableExtra)
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
oaCodes1 <- readRDS(here("vignettes", "optionsData01.RData"))

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
datatable(oaCodes1,
  rownames = FALSE,
  options = list(
    pageLength = 10,
    lengthMenu = c(10, 20, 250)
  )
)

## ----message=FALSE, warning=FALSE,echo=FALSE----------------------------------
oaCodes2 <- readRDS(here("vignettes", "optionsData02.RData"))

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
newCodes1To2 <- compareCodelists(oaCodes1, oaCodes2) %>%
  filter(codelist == "Only codelist 2") %>%
  select(-"codelist")

datatable(newCodes1To2,
  rownames = FALSE,
  options = list(
    pageLength = 10,
    lengthMenu = c(10, 20, 50)
  )
)

## ----message=FALSE, warning=FALSE,echo=FALSE----------------------------------
oaCodes3 <- readRDS(here("vignettes", "optionsData03.RData"))

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
newCodes1To3 <- compareCodelists(oaCodes1, oaCodes3) %>%
  filter(codelist == "Only codelist 2") %>%
  select(-"codelist")

datatable(newCodes1To3,
  rownames = FALSE,
  options = list(
    pageLength = 10,
    lengthMenu = c(10, 20, 50)
  )
)

## ----message=FALSE, warning=FALSE,echo=FALSE----------------------------------
oaCodes4 <- readRDS(here("vignettes", "optionsData04.RData"))

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
newCodes1To4 <- compareCodelists(oaCodes1, oaCodes4) %>%
  filter(codelist == "Only codelist 2") %>%
  select(-"codelist")

datatable(newCodes1To4,
  rownames = FALSE,
  options = list(
    pageLength = 10,
    lengthMenu = c(10, 20, 50)
  )
)

## ----message=FALSE, warning=FALSE,echo=FALSE----------------------------------
oaCodes5 <- readRDS(here("vignettes", "optionsData04.RData"))

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
newCodes1To5 <- compareCodelists(oaCodes1, oaCodes5) %>%
  filter(codelist == "Only codelist 2") %>%
  select(-"codelist")

datatable(newCodes1To5,
  rownames = FALSE,
  options = list(
    pageLength = 10,
    lengthMenu = c(10, 20, 50)
  )
)

## ----message=FALSE, warning=FALSE,echo=FALSE----------------------------------
oaCodes8 <- readRDS(here("vignettes", "optionsData07.RData"))

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
newCodes1To8 <- compareCodelists(oaCodes1, oaCodes8) %>%
  filter(codelist == "Only codelist 2") %>%
  select(-"codelist")

datatable(newCodes1To8,
  rownames = FALSE,
  options = list(
    pageLength = 10,
    lengthMenu = c(10, 20, 50)
  )
)

