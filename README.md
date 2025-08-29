# Network Data Quality (NDQ) # ndq <a href="https://pedsnet.github.io/ndq/"><img src="man/figures/logo.png" align="right" height="139" alt="ndq website" /></a>

<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/ndq)](https://CRAN.R-project.org/package=ndq) [![R-CMD-check](https://github.com/PEDSnet/ndq/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PEDSnet/ndq/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The Network Data Quality (NDQ) package contains several data quality modules intended to evaluate the overall condition of the data in a clinical research network. These modules, which cover a broad range of data quality domains from conformance to plausibility, are flexible and can be configured to execute checks specific to desired use cases in both the OMOP and PCORnet common data models (CDMs).

## Installation

You can install the development version of this package from GitHub:

``` r
remotes::install_github('PEDSnet/ndq')
```

## Current Functionality

The package currently (as of 09/2025) contains 10 distinct analysis types that can be configured to run innumerable data quality checks. See the table below for a list of the current offerings.

| Analysis Type | Description | Functions |
|:----------------|:---------------------|:--------------------------------|
| Data Cycle Changes | Computes row & patient counts in the specified tables for both the current data model version and a previous data model version in order to assess changes across data extractions. | [check_dc](https://pedsnet.github.io/ndq/reference/check_dc.html) <br> [process_dc](https://pedsnet.github.io/ndq/reference/process_dc.html) |
| Domain Concordance | Given the details of a pair of clinical events, this function will determine the count of patients OR visits that meet criteria for the first event, the second event, and both events. | [check_dcon](https://pedsnet.github.io/ndq/reference/check_dcon.html) <br> [process_dcon](https://pedsnet.github.io/ndq/reference/process_dcon.html) |
| Date Plausibility | Identifies the proportion of rows in each fact type that have an implausible date, where implausibility is defined as a date that falls before the associated visit start date, after the associated visit end date, or before the patient's birth date. | [check_dp](https://pedsnet.github.io/ndq/reference/check_dp.html) <br> [process_dp](https://pedsnet.github.io/ndq/reference/process_dp.html) |
| Best Mapped Concepts | Identifies the existing concepts within the specified field so the user can assess which of these concepts are acceptable ("best") or should not be used in that field ("not best"). | [check_bmc](https://pedsnet.github.io/ndq/reference/check_bmc.html) <br> [process_bmc](https://pedsnet.github.io/ndq/reference/process_bmc.html) |
| Expected Concepts Present | Identifies the count of patients who have at least one occurrence of the concept defined in the associated concept set and the proportion of patients who have the concept based on the user-provided denominator cohort. | [check_ecp](https://pedsnet.github.io/ndq/reference/check_ecp.html) <br> [process_ecp](https://pedsnet.github.io/ndq/reference/process_ecp.html) |
| Clinical Fact Documentation | Identifies visits that do and do not link to at least one of each user-specified clinical fact type. Will also compute the counts of patients who have at least one visit that does / does not link to the specified fact type. | [check_cfd](https://pedsnet.github.io/ndq/reference/check_cfd.html) <br> [process_cfd](https://pedsnet.github.io/ndq/reference/process_cfd.html) |
| Missing Field: Visit ID | Checks to see if the `visit_occurrence_id`/`encounterid` in a given fact table also exists in the `visit_occurrence`/`encounter` table and identify cases where the ID is missing entirely (NULL). | [check_mf_visitid](https://pedsnet.github.io/ndq/reference/check_mf_visitid.html) <br> [process_mf_visitid](https://pedsnet.github.io/ndq/reference/process_mf_visitid.html) |
| Unmapped Concepts | Evaluates the count and proportion of unmapped concepts associated with the fact type of interest. Can also be executed longitudinally by year. | [check_uc](https://pedsnet.github.io/ndq/reference/check_uc.html) <br> [process_uc](https://pedsnet.github.io/ndq/reference/process_uc.html) |
| Valueset Conformance | Intakes a limited valueset that is expected to make up the entire contents of a field (minus the specified `null_values`) and identifies if any non-permitted values exist in the field (and how often). | [check_vs](https://pedsnet.github.io/ndq/reference/check_vs.html) <br> [process_vs](https://pedsnet.github.io/ndq/reference/process_vs.html) |
| Vocabulary Conformance | Use a provided vocabulary definition table to identify the vocabulary of each concept and determine how many rows comply with the standard vocabularies expected for that field. | [check_vc](https://pedsnet.github.io/ndq/reference/check_vc.html) <br> [process_vc](https://pedsnet.github.io/ndq/reference/process_vc.html) |
| Facts Over Time | Computes the number of rows, patients, and (optionally) visits associated with the fact of interest within a specified time period. | [check_fot](https://pedsnet.github.io/ndq/reference/check_fot.html) <br> [process_fot](https://pedsnet.github.io/ndq/reference/process_fot.html) |

## Example Usage

To see a sample repository where these functions have been executed, see [PEDSnet NDQ](https://github.com/PEDSnet/pedsnet_ndq).
