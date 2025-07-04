% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{cfd_input_omop}
\alias{cfd_input_omop}
\title{Clinical Fact Documentation Input File (OMOP)}
\format{
\subsection{\code{cfd_input_omop}}{

A dataframe or CSV file with 5 columns
\describe{
\item{check_id}{A short string "code" used to identify the specific check (ex: de, de_rx)}
\item{check_description}{A longer description of the check (ex: full drug_exposure table, prescribed drugs based on drug_type_concept_id)}
\item{schema}{The schema where the data is kept. Use \code{cdm} to use the pre-configured \code{cdm_schema}, \code{result} to use the preconfigured \code{results_schema}, or input the exact name of the schema.}
\item{table}{The name of the CDM or pre-computed results table where the relevant data is kept}
\item{filter_logic}{\strong{OPTIONAL} The logic that should be applied to the provided table in order to tailor the tables to the desired check assessment (ex: if you only want to assess prescription drugs)}
}
}
}
\usage{
cfd_input_omop
}
\description{
A sample version of an OMOP compliant input file for
the Clinical Fact Documentation data quality module. This exact
file is also included as a CSV in the package if the user wishes
to use it, or the structure can be copied to produce a custom
list of checks.
}
\keyword{datasets}
