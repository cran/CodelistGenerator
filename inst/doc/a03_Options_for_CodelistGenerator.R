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

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("mock_db_fig1.png")

## ----message=FALSE, warning=FALSE,echo=FALSE, results='hide'------------------
cdm <- mockVocabRef(backend = "data_frame")

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("mock_db_fig2.png")

## -----------------------------------------------------------------------------
codes <- getCandidateCodes(
  cdm = cdm,
  keywords = "Musculoskeletal disorder",
  domains = "Condition",
  includeDescendants = FALSE,
)

codes |> 
  glimpse()

## -----------------------------------------------------------------------------
codes <- getCandidateCodes(
  cdm = cdm,
  keywords = "Musculoskeletal",
  domains = "Condition",
  includeDescendants = FALSE
)

codes |> 
  glimpse()

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("mock_db_fig7.png")

## -----------------------------------------------------------------------------
getCandidateCodes(
  cdm = cdm,
  keywords = "Musculoskeletal disorder",
  domains = "Condition",
  includeDescendants = TRUE
) |> 
  glimpse()

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

codes |> 
  glimpse()

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("mock_db_fig5.png")

## -----------------------------------------------------------------------------
codes <- getCandidateCodes(
  cdm = cdm,
  keywords = "Osteoarthritis of knee",
  includeAncestor = TRUE,
  domains = "Condition"
)

codes |> 
  glimpse()

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("mock_db_fig4.png")

## -----------------------------------------------------------------------------
codes <- getCandidateCodes(
  cdm = cdm,
  keywords = "Knee osteoarthritis",
  domains = "Condition",
  includeDescendants = TRUE
)

codes |> 
  glimpse()

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("mock_db_fig5.png")

## -----------------------------------------------------------------------------
codes <- getCandidateCodes(
  cdm = cdm,
  keywords = "arthritis",
  exclude = "Hip osteoarthritis",
  domains = "Condition"
)

codes |> 
  glimpse()

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("mock_db_fig6.png")

## -----------------------------------------------------------------------------
codes <- getCandidateCodes(
  cdm = cdm,
  keywords = "osteoarthrosis",
  domains = "Condition",
  searchInSynonyms = TRUE
)

codes |> 
  glimpse()

## -----------------------------------------------------------------------------
codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("arthritis", "arthropathy"),
  domains = "Condition",
  searchNonStandard = TRUE
)

codes |> 
  glimpse()

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

codes |> 
  glimpse()

