## ----include = FALSE----------------------------------------------------------
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = NOT_CRAN
)

## -----------------------------------------------------------------------------
# library(omopgenerics, warn.conflicts = FALSE)
# library(dplyr, warn.conflicts = FALSE)
# library(CodelistGenerator)

## -----------------------------------------------------------------------------
# codelist <- list("codes1" = c(1L, 2L, 3L),
#                  "codes2" = c(4L, 5L, 10L))
# codelist <- newCodelist(codelist)
# 
# codelist

## -----------------------------------------------------------------------------
# attributes(codelist)

## -----------------------------------------------------------------------------
# empty_codelist  <- emptyCodelist()
# 
# empty_codelist

## -----------------------------------------------------------------------------
# codelist_with_details <- list("codes1" = tibble("concept_id" = c(1L, 2L, 3L)),
#                               "codes2" = tibble("concept_id" = c(4L, 5L, 10L)))
# codelist_with_details <- newCodelistWithDetails(codelist_with_details)
# 
# codelist_with_details

## -----------------------------------------------------------------------------
# codelist_with_details <- list(
#   "codes1" = tibble("concept_id" = c(1L, 2L, 3L),
#                     "concept_name" = c("Musculoskeletal disorder", "Osteoarthrosis", "Arthritis"),
#                     "domain_id" = c("Condition", "Condition", "Condition"),
#                     "vocabulary_id" = c("SNOMED","SNOMED","SNOMED"),
#                     "standard_concept" = c("S","S","S")),
#   "codes2" = tibble("concept_id" = c(4L, 5L, 10L),
#                     "concept_name" = c("Osteoarthritis of knee", "Osteoarthritis of hip", "Adalimumab"),
#                     "domain_id" = c("Condition", "Condition", "Drug"),
#                     "vocabulary_id" = c("SNOMED","SNOMED","RxNorm"),
#                     "standard_concept" = c("S","S","S")))
# codelist_with_details <- newCodelistWithDetails(codelist_with_details)
# 
# codelist_with_details

## -----------------------------------------------------------------------------
# attributes(codelist_with_details)

## -----------------------------------------------------------------------------
# empty_codelist_with_details <- emptyCodelistWithDetails()
# 
# empty_codelist_with_details

## -----------------------------------------------------------------------------
# concept_set_expression <- list(
#   "codes1" = tibble("concept_id" = c(1L, 2L, 3L),
#                     "excluded" = c(FALSE, FALSE, FALSE),
#                     "descendants" = c(TRUE, FALSE, FALSE),
#                     "mapped" = c(TRUE, TRUE, TRUE)),
#   "codes2" = tibble("concept_id" = c(4L, 5L, 10L),
#                     "excluded" = c(FALSE, FALSE, FALSE),
#                     "descendants" = c(FALSE, FALSE, FALSE),
#                     "mapped" = c(TRUE, TRUE, TRUE))
# )
# 
# concept_set_expression <- newConceptSetExpression(concept_set_expression)
# 
# concept_set_expression

## -----------------------------------------------------------------------------
# attributes(concept_set_expression)

## -----------------------------------------------------------------------------
# empty_concept_set_expression <- emptyConceptSetExpression()
# 
# empty_concept_set_expression

## -----------------------------------------------------------------------------
# cdm <- mockVocabRef()
# codelist_to_codelist_with_details <- asCodelistWithDetails(codelist, cdm)
# codelist_to_codelist_with_details
# 
# codelist_to_concept_set_expression <- asCodelistWithDetails(codelist, cdm)
# codelist_to_concept_set_expression
# 

## -----------------------------------------------------------------------------
# 
# codelist_to_concept_set_expression <- asConceptSetExpression(codelist)
# codelist_to_concept_set_expression
# 
# codelist_with_details_to_concept_set_expression <- asConceptSetExpression(codelist_with_details)
# codelist_with_details_to_concept_set_expression

## -----------------------------------------------------------------------------
# codelist_with_details_to_codelist <- asCodelist(codelist_with_details)
# codelist_with_details_to_codelist
# 

