## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE, warning=FALSE,echo=FALSE----------------------------------
library(DBI)
library(dplyr)
library(dbplyr)
library(here)
library(kableExtra)
library(CodelistGenerator)
library(CDMConnector)

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("mock_db_fig1.png")

## ----message=FALSE, warning=FALSE,echo=FALSE, results='hide'------------------
cdm <- mockVocabRef()

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("mock_db_fig2.png")

## -----------------------------------------------------------------------------
codes <- getCandidateCodes(
  cdm = cdm,
  keywords = "Musculoskeletal disorder",
  domains = "Condition",
  includeDescendants = FALSE,
)

kable(codes)

## -----------------------------------------------------------------------------
codes <- getCandidateCodes(
  cdm = cdm,
  keywords = "Musculoskeletal",
  domains = "Condition",
  includeDescendants = FALSE
)

kable(codes)

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("mock_db_fig7.png")

## -----------------------------------------------------------------------------
kable(getCandidateCodes(
  cdm = cdm,
  keywords = "Musculoskeletal disorder",
  domains = "Condition",
  includeDescendants = TRUE
))

## -----------------------------------------------------------------------------
codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c(
    "Musculoskeletal disorder",
    "arthritis",
    "arthrosis"
  ),
  domains = "Condition",
  includeDescendants = FALSE
)

kable(codes)

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("mock_db_fig5.png")

## -----------------------------------------------------------------------------
codes <- getCandidateCodes(
  cdm = cdm,
  keywords = "Osteoarthritis of knee",
  includeAncestor = TRUE,
  domains = "Condition"
)

kable(codes)

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("mock_db_fig4.png")

## -----------------------------------------------------------------------------
codes <- getCandidateCodes(
  cdm = cdm,
  keywords = "Knee osteoarthritis",
  domains = "Condition",
  includeDescendants = TRUE
)

kable(codes)

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("mock_db_fig5.png")

## -----------------------------------------------------------------------------
codes <- getCandidateCodes(
  cdm = cdm,
  keywords = "arthritis",
  exclude = "Hip osteoarthritis",
  domains = "Condition"
)

kable(codes)

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("mock_db_fig6.png")

## -----------------------------------------------------------------------------
codes <- getCandidateCodes(
  cdm = cdm,
  keywords = "osteoarthrosis",
  domains = "Condition",
  searchInSynonyms = TRUE
)

kable(codes)

## -----------------------------------------------------------------------------
codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("arthritis", "arthropathy"),
  domains = "Condition",
  searchNonStandard = TRUE
)

kable(codes)

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("mock_db_fig8.png")

## -----------------------------------------------------------------------------
codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c(
    "Musculoskeletal disorder",
    "arthritis",
    "arthropathy",
    "arthrosis"
  ),
  domains = "Condition",
  standardConcept = c("Standard", "Non-standard")
)

kable(codes)

## ----echo=FALSE---------------------------------------------------------------
DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

