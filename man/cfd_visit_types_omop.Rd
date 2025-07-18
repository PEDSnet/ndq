% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{cfd_visit_types_omop}
\alias{cfd_visit_types_omop}
\title{Clinical Fact Documentation Visit Type Input File (OMOP)}
\format{
\subsection{\code{cfd_visit_types_omop}}{

A dataframe or CSV file with 2 columns
\describe{
\item{visit_type}{A string to identify the visit type of interest (ex: inpatient)}
\item{visit_concept_id}{The visit_concept_id associated with the visit type of interest (ex: 9202)}
}
}
}
\usage{
cfd_visit_types_omop
}
\description{
A sample version of an OMOP compliant input file for
defining visit types for the Clinical Fact Documentation data quality module.
Multiple rows should be included where multiple concepts are associated
with a single visit type. The name of the second column will control
where the visit filtering takes place, so you can filter on other
columns in the visit_tbl if need be (i.e. visit_source_concept_id)
}
\keyword{datasets}
