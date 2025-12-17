## ----include = FALSE----------------------------------------------------------
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%",
  eval = NOT_CRAN
)

## -----------------------------------------------------------------------------
# library(CodelistGenerator)

## -----------------------------------------------------------------------------
# dir_codes <- file.path(tempdir(), "codelists")
# dir.create(dir_codes)
# list.files(dir_codes)

## -----------------------------------------------------------------------------
# codelist <- list("codes1" = c(1L, 2L, 3L),
#                  "codes2" = c(4L, 5L, 10L))
# codelist <- newCodelist(codelist)
# 
# codelist

## -----------------------------------------------------------------------------
# exportCodelist(codelist, dir_codes, type = "csv")
# list.files(dir_codes)

## -----------------------------------------------------------------------------
# importCodelist(dir_codes, type = "csv")

## ----message=FALSE, warning=FALSE---------------------------------------------
# library(jsonlite)
# concept_set_path <- system.file("concepts_for_mock/arthritis_with_excluded.json",
#                                 package = "CodelistGenerator")
# fromJSON(concept_set_path) |> toJSON(pretty = TRUE, auto_unbox = TRUE)

## ----message=FALSE, warning=FALSE---------------------------------------------
# cse <- importConceptSetExpression(concept_set_path)
# cse

## ----message=FALSE, warning=FALSE---------------------------------------------
# cdm <- mockVocabRef()
# importConceptSetExpression(concept_set_path) |>
#   asCodelist(cdm)

