## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(dplyr)
library(CodelistGenerator)

cdm <- mockVocabRef()

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("Figures/1.png")

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("Figures/2.png")

## ----message=FALSE------------------------------------------------------------
getCandidateCodes(
  cdm = cdm,
  keywords = "Musculoskeletal disorder",
  domains = "Condition", 
  standardConcept = "Standard",
  includeDescendants = FALSE,
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeAncestor = FALSE
)

## ----message=FALSE------------------------------------------------------------
getCandidateCodes(
  cdm = cdm,
  keywords = "Musculoskeletal",
  domains = "Condition",
  standardConcept = "Standard",
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeDescendants = FALSE,
  includeAncestor = FALSE
)

getCandidateCodes(
  cdm = cdm,
  keywords = "Disorder musculoskeletal",
  domains = "Condition",
  standardConcept = "Standard",
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeDescendants = FALSE,
  includeAncestor = FALSE
)

## ----message=FALSE------------------------------------------------------------
candidate_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = "Musculoskeletal",
  domains = "Condition",
  standardConcept = "Standard",
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeDescendants = FALSE,
  includeAncestor = FALSE
)

searchStrategy(candidate_codes)

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("Figures/3.png")

## ----message=FALSE------------------------------------------------------------
getCandidateCodes(
  cdm = cdm,
  keywords = "Musculoskeletal disorder",
  domains = "Condition",
  standardConcept = c("Non-standard", "Standard"),
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeDescendants = FALSE,
  includeAncestor = FALSE
)

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("Figures/4.png")

## ----message=FALSE------------------------------------------------------------
getCandidateCodes(
  cdm = cdm,
  keywords = c(
    "Musculoskeletal disorder",
    "arthritis"
  ),
  domains = "Condition",
  standardConcept = c("Standard"),
  includeDescendants = FALSE,
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeAncestor = FALSE
)

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("Figures/5.png")

## ----message=FALSE------------------------------------------------------------
getCandidateCodes(
  cdm = cdm,
  keywords = "Musculoskeletal disorder",
  domains = "Condition",
  standardConcept = "Standard",
  includeDescendants = TRUE,
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeAncestor = FALSE
)

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("Figures/6.png")

## ----message=FALSE------------------------------------------------------------
getCandidateCodes(
  cdm = cdm,
  keywords = "Musculoskeletal disorder",
  domains = "Condition",
  exclude = c("Osteoarthrosis", "knee"),
  standardConcept = "Standard",
  includeDescendants = TRUE,
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeAncestor = FALSE
)

## ----message=FALSE------------------------------------------------------------
getCandidateCodes(
  cdm = cdm,
  keywords = "Musculoskeletal disorder",
  domains = "Condition",
  exclude = c("knee osteoarthritis"),
  standardConcept = "Standard",
  includeDescendants = TRUE,
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeAncestor = FALSE
)

## ----message=FALSE------------------------------------------------------------
# No exclusion:
getCandidateCodes(
    cdm = cdm,
    keywords = "Knee",
    domains = "Condition",
    exclude = NULL,
    standardConcept = c("Standard", "Non-standard"),
    includeDescendants = TRUE,
    searchInSynonyms = FALSE,
    searchNonStandard = FALSE,
    includeAncestor = FALSE
)

# Exclusion looking for terms:
getCandidateCodes(
  cdm = cdm,
  keywords = "Knee",
  domains = "Condition",
  exclude = c("knee osteoarthritis"),
  standardConcept = c("Standard", "Non-standard"),
  includeDescendants = TRUE,
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeAncestor = FALSE
)

# Exclusion looking for partial matching terms (without word boundaries)
getCandidateCodes(
  cdm = cdm,
  keywords = "Knee",
  domains = "Condition",
  exclude = c("/knee osteoarthritis/"),
  standardConcept = c("Standard", "Non-standard"),
  includeDescendants = TRUE,
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeAncestor = FALSE
)

# Exclusion looking for partial matching terms (without word boundaries)
getCandidateCodes(
  cdm = cdm,
  keywords = "Knee",
  domains = "Condition",
  exclude = c("/e osteoarthritis/"),
  standardConcept = c("Standard", "Non-standard"),
  includeDescendants = TRUE,
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeAncestor = FALSE
)

## ----message=FALSE------------------------------------------------------------
getCandidateCodes(
  cdm = cdm,
  keywords = "Knee",
  domains = "Condition",
  exclude = c("/\bKnee osteoarthritis/\b"),
  standardConcept = c("Standard", "Non-standard"),
  includeDescendants = TRUE,
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeAncestor = FALSE
)

# We will now only search for "ee osteoarthritis" to show that 
# "knee osteoarthritis" won't be excluded:
getCandidateCodes(
  cdm = cdm,
  keywords = "Knee",
  domains = "Condition",
  exclude = c("/\bee osteoarthritis/\b"),
  standardConcept = c("Standard", "Non-standard"),
  includeDescendants = TRUE,
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeAncestor = FALSE
)

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("Figures/7.png")

## ----message=FALSE------------------------------------------------------------
codes <- getCandidateCodes(
  cdm = cdm,
  keywords = "Osteoarthritis of knee",
  includeAncestor = TRUE,
  domains = "Condition",
  standardConcept = "Standard",
  includeDescendants = TRUE,
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
)

codes

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("Figures/8.png")

## ----message=FALSE------------------------------------------------------------
getCandidateCodes(
  cdm = cdm,
  keywords = "osteoarthrosis",
  domains = "Condition",
  searchInSynonyms = TRUE,
  standardConcept = "Standard",
  includeDescendants = FALSE,
  searchNonStandard = FALSE,
  includeAncestor = FALSE
)

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("Figures/9.png")

## ----message=FALSE------------------------------------------------------------
getCandidateCodes(
  cdm = cdm,
  keywords = "osteoarthrosis",
  domains = "Condition",
  searchInSynonyms = TRUE,
  standardConcept = "Standard",
  includeDescendants = TRUE,
  searchNonStandard = FALSE,
  includeAncestor = FALSE
)

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("Figures/10.png")

## ----message=FALSE------------------------------------------------------------
codes1 <- getCandidateCodes(
  cdm = cdm,
  keywords = "Degenerative",
  domains = "Condition",
  standardConcept = "Standard",
  searchNonStandard = TRUE,
  includeDescendants = FALSE,
  searchInSynonyms = FALSE,
  includeAncestor = FALSE
)
codes1

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("Figures/11.png")

## ----message=FALSE------------------------------------------------------------
codes2 <- getCandidateCodes(
  cdm = cdm,
  keywords = "Degenerative",
  domains = "Condition",
  standardConcept = c("Non-standard", "Standard"),
  searchNonStandard = FALSE,
  includeDescendants = FALSE,
  searchInSynonyms = FALSE,
  includeAncestor = FALSE
)
codes2

