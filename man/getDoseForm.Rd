% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vocabUtilities.R
\name{getDoseForm}
\alias{getDoseForm}
\title{getDoseForm}
\usage{
getDoseForm(cdm)
}
\arguments{
\item{cdm}{cdm_reference via CDMConnector}
}
\value{
The dose forms available for drug concepts
}
\description{
getDoseForm
}
\examples{
cdm <- mockVocabRef()
getDoseForm(cdm = cdm)
DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
}
