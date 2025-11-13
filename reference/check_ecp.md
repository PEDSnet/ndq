# Expected Concepts Present

This function will iterate through the provided input table to identify
the count of patients who have the at least one occurrence of the
concept defined in the user-provided concept set and the proportion of
patients who have the concept based on the user-defined denominator
cohort.

## Usage

``` r
check_ecp(ecp_tbl, omop_or_pcornet = "omop", check_string = "ecp")
```

## Arguments

- ecp_tbl:

  *tabular input* \|\| **required**

  The primary input table that contains descriptive information about
  the checks to be executed by the function. It should include
  definitions for the concepts that should be identified and the
  denominator cohort to be used for the computation. see
  [`?ecp_input_omop`](https://pedsnet.github.io/ndq/reference/ecp_input_omop.md)
  or
  [`?ecp_input_pcornet`](https://pedsnet.github.io/ndq/reference/ecp_input_pcornet.md)
  for examples of the input structure

- omop_or_pcornet:

  *string* \|\| defaults to `omop`

  A string, either `omop` or `pcornet`, indicating the CDM format of the
  data

- check_string:

  *string* \|\| defaults to `ecp`

  An abbreviated identifier that will be used to label all output from
  this module

## Value

This function will return a table with the total patient count, the
count of patients with a particular concept, the proportion of total
patients with the concept, plus some additional descriptive metadata

## Examples

``` r
# First create input file with desired checks to be executed
# You can access examples for both OMOP & PCORnet here:
ndq::ecp_input_omop
#> # A tibble: 5 × 9
#>   check_id   cohort_definition           cohort_schema cohort_table schema table
#>   <chr>      <chr>                       <chr>         <chr>        <chr>  <chr>
#> 1 hemoglobin All patients in CDM person… cdm           person       cdm    meas…
#> 2 sodium     All patients in CDM person… cdm           person       cdm    meas…
#> 3 flu        All patients in CDM person… cdm           person       cdm    meas…
#> 4 height     All patients in CDM person… cdm           person       cdm    meas…
#> 5 weight     All patients in CDM person… cdm           person       cdm    meas…
#> # ℹ 3 more variables: concept_field <chr>, conceptset_name <chr>,
#> #   filter_logic <lgl>
ndq::ecp_input_pcornet
#> # A tibble: 5 × 10
#>   check_id   cohort_definition           cohort_schema cohort_table schema table
#>   <chr>      <chr>                       <chr>         <chr>        <chr>  <chr>
#> 1 hemoglobin All patients in CDM person… cdm           demographic  cdm    lab_…
#> 2 sodium     All patients in CDM person… cdm           demographic  cdm    lab_…
#> 3 flu        All patients in CDM person… cdm           demographic  cdm    lab_…
#> 4 height     All patients in CDM person… cdm           demographic  cdm    lab_…
#> 5 weight     All patients in CDM person… cdm           demographic  cdm    lab_…
#> # ℹ 4 more variables: concept_field <chr>, vocabulary_field <lgl>,
#> #   conceptset_name <chr>, filter_logic <lgl>

# Use this as your input to the ECP function
## To execute the check at the patient level:
if (FALSE) { # \dontrun{
my_ecp_rslt <- check_ecp(ecp_tbl = ndq::ecp_input_omop,
                         omop_or_pcornet = 'omop',
                         check_string = 'ecp')
} # }
```
