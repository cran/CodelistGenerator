% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/codesFromConceptSet.R
\name{codesFromCohort}
\alias{codesFromCohort}
\title{Get concept ids from JSON files containing cohort definitions}
\usage{
codesFromCohort(path, cdm, type = c("codelist"))
}
\arguments{
\item{path}{Path to a file or folder containing JSONs of cohort definitions.}

\item{cdm}{A cdm reference via CDMConnector.}

\item{type}{Can be "codelist", "codelist_with_details" or
"concept_set_expression".}
}
\value{
Named list with concept_ids for each concept set.
}
\description{
Get concept ids from JSON files containing cohort definitions
}
\examples{
\donttest{
cdm <- mockVocabRef("database")
x <- codesFromCohort(cdm = cdm,
                     path =  system.file(package = "CodelistGenerator",
                     "cohorts_for_mock"))
x
CDMConnector::cdmDisconnect(cdm)
}

}
