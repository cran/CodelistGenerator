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

## ----eval=FALSE---------------------------------------------------------------
#  # postgres database connection details
#  serverDbi <- Sys.getenv("server")
#  user <- Sys.getenv("user")
#  password <- Sys.getenv("password")
#  port <- Sys.getenv("port")
#  host <- Sys.getenv("host")
#  
#  db <- DBI::dbConnect(RPostgres::Postgres(),
#    dbname = serverDbi,
#    port = port,
#    host = host,
#    user = user,
#    password = password
#  )
#  
#  # name of vocabulary schema
#  vocabularyDatabaseSchema <- "vocabulary"

## ----eval=FALSE---------------------------------------------------------------
#  library(dplyr)
#  library(CodelistGenerator)
#  library(stringr)
#  library(DT)
#  library(kableExtra)

## ----eval=FALSE---------------------------------------------------------------
#  acetaminophen1 <- getCandidateCodes(
#    cdm = cdm,
#    keywords = "acetaminophen",
#    domains = "drug",
#    standardConcept = "standard",
#    includeDescendants = TRUE
#  )

## ----message=FALSE, warning=FALSE, echo=FALSE---------------------------------
acetaminophen1 <- readRDS(here("vignettes", "medData01.RData"))

## ----message=FALSE, warning=FALSE---------------------------------------------
acetaminophen1 %>% dplyr::glimpse()

