## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include = FALSE----------------------------------------------------------
CDMConnector::requireEunomia("synpuf-1k", "5.3")

## ----message=FALSE, warning=FALSE---------------------------------------------
library(DBI)
library(duckdb)
library(dplyr)
library(CDMConnector)
library(CodelistGenerator)
library(visOmopResults)

# Connect to the database and create the cdm object
con <- dbConnect(duckdb(), 
                      eunomiaDir("synpuf-1k", "5.3"))
cdm <- cdmFromCon(con = con, 
                  cdmName = "Eunomia Synpuf",
                  cdmSchema   = "main",
                  writeSchema = "main", 
                  achillesSchema = "main")

timings <- benchmarkCodelistGenerator(cdm)

visOmopTable(timings,
             hide = c("variable_name", "variable_level", "strata_name", "strata_level"),
             groupColumn = "task")

