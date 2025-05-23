% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unmapped_concepts.R
\name{process_uc}
\alias{process_uc}
\title{Unmapped Concepts -- Processing}
\usage{
process_uc(uc_results, rslt_source = "remote", csv_rslt_path = NULL)
}
\arguments{
\item{uc_results}{the output of check_uc}

\item{rslt_source}{the location of the results. acceptable values are \code{local} (stored as a dataframe in the R environment),
\code{csv} (stored as CSV files), or \code{remote} (stored on a remote DBMS); defaults to remote}

\item{csv_rslt_path}{if the results have been stored as CSV files, the path to the location
of these files. If the results are local or remote, leave NULL}
}
\value{
a table with either an additional column with unmapped proportions (by_year) or
with additional rows that include total unmapped counts/proportions across all
sites (not by_year)
}
\description{
Intakes the output of check_uc in order to apply additional processing. This
includes either adding proportions (for by_year output) or computing overall
totals across all sites included in the input (for not by_year output)
}
