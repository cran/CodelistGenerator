% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mockVocabRef.R
\name{mockVocabRef}
\alias{mockVocabRef}
\title{Generate example vocabulary database}
\usage{
mockVocabRef(backend = "database")
}
\arguments{
\item{backend}{'database' (duckdb), 'arrow' (parquet files), or 'data_frame'}
}
\value{
cdm reference with mock vocabulary
}
\description{
Generate example vocabulary database
}
\examples{
cdm <- mockVocabRef()
cdm
DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
}
