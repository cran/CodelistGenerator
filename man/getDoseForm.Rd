% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vocabUtilities.R
\name{getDoseForm}
\alias{getDoseForm}
\title{Get the dose forms available for drug concepts}
\usage{
getDoseForm(cdm)
}
\arguments{
\item{cdm}{A cdm reference via CDMConnector.}
}
\value{
The dose forms available for drug concepts.
}
\description{
Get the dose forms available for drug concepts
}
\examples{
\donttest{
cdm <- mockVocabRef()
getDoseForm(cdm = cdm)
}
}
