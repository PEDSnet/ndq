# Network Data Quality (NDQ)

The Network Data Quality (NDQ) package contains several data quality
modules intended to evaluate the overall condition of the data in a
clinical research network. These modules, which cover a broad range of
data quality domains from conformance to plausibility, are flexible and
can be configured to execute checks specific to desired use cases in
both the OMOP and PCORnet common data models (CDMs).

## Installation

You can install the development version of this package from GitHub:

``` r
remotes::install_github('PEDSnet/ndq')
```

## Current Functionality

The package currently (as of 09/2025) contains 10 distinct analysis
types that can be configured to run innumerable data quality checks. See
the table below for a list of the current offerings.

[TABLE]

## Example Usage

To see a sample repository where these functions have been executed, see
[PEDSnet NDQ](https://github.com/PEDSnet/pedsnet_ndq).
