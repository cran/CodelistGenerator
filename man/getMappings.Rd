% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getMappings.R
\name{getMappings}
\alias{getMappings}
\title{Show mappings from non-standard vocabularies to standard}
\usage{
getMappings(
  candidateCodelist,
  cdm = NULL,
  nonStandardVocabularies = c("ATC", "ICD10CM", "ICD10PCS", "ICD9CM", "ICD9Proc",
    "LOINC", "OPCS4", "Read", "RxNorm", "RxNorm Extension", "SNOMED")
)
}
\arguments{
\item{candidateCodelist}{Dataframe}

\item{cdm}{cdm_reference via CDMConnector::cdm_from_con()}

\item{nonStandardVocabularies}{Character vector}
}
\value{
tibble
}
\description{
Show mappings from non-standard vocabularies to standard
}
\examples{
\dontrun{
cdm <- CodelistGenerator::mockVocabRef()
codes <- CodelistGenerator::getCandidateCodes(
  cdm = cdm,
  keywords = "osteoarthritis"
)
CodelistGenerator::getMappings(
  cdm = cdm,
  candidateCodelist = codes,
  nonStandardVocabularies = "READ"
)
CDMConnector::cdmDisconnect(cdm)
}
}
