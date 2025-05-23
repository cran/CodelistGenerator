% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summariseUnmappedCodes.R
\name{summariseUnmappedCodes}
\alias{summariseUnmappedCodes}
\title{Find unmapped concepts related to codelist}
\usage{
summariseUnmappedCodes(
  x,
  cdm,
  table = c("condition_occurrence", "device_exposure", "drug_exposure", "measurement",
    "observation", "procedure_occurrence")
)
}
\arguments{
\item{x}{A codelist.}

\item{cdm}{A cdm reference via CDMConnector.}

\item{table}{Names of clinical tables in which to search for unmapped codes.
Can be one or more of "condition_occurrence", "device_exposure",
"drug_exposure", "measurement", "observation", and "procedure_occurrence".}
}
\value{
A summarised result of unmapped concepts related to given codelist.
}
\description{
Find unmapped concepts related to codelist
}
\examples{
\donttest{
cdm <- mockVocabRef("database")
codes <- list("Musculoskeletal disorder" = 1)
cdm <- omopgenerics::insertTable(cdm, "condition_occurrence",
dplyr::tibble(person_id = 1,
              condition_occurrence_id = 1,
              condition_concept_id = 0,
              condition_start_date  = as.Date("2000-01-01"),
              condition_type_concept_id  = NA,
              condition_source_concept_id = 7))
summariseUnmappedCodes(x = list("osteoarthritis" = 2), cdm = cdm,
table = "condition_occurrence")

CDMConnector::cdmDisconnect(cdm)
}
}
