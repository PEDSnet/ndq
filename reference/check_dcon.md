# Domain Concordance

Given the details of a pair of clinical events, this function will
determine the count of patients OR visits that meet criteria for the
first event, the second event, and both events. Users can optionally
define a time limitation for the combined cohort, so patients/visits
will only count towards that cohort when the two events occur within a
specified number of days of each other.

## Usage

``` r
check_dcon(
  dcon_tbl,
  compute_level = "patient",
  omop_or_pcornet = "omop",
  check_string = "dcon"
)
```

## Arguments

- dcon_tbl:

  *tabular* \|\| **required**

  The primary input table that contains descriptive information about
  the checks to be executed by the function. It should include
  definitions for each pair of cohorts, meaning there should be **2 rows
  per check**, one fo each cohort, with the same `check_id`. see
  [`?dcon_input_omop`](https://pedsnet.github.io/ndq/reference/dcon_input_omop.md)
  or
  [`?dcon_input_pcornet`](https://pedsnet.github.io/ndq/reference/dcon_input_pcornet.md)
  for examples of the input structure

- compute_level:

  *string* \|\| defaults to `patient`

  A string indicating whether the analysis should be conducted at the
  `patient` or `visit` level

- omop_or_pcornet:

  *string* \|\| defaults to `omop`

  A string, either `omop` or `pcornet`, indicating the CDM format of the
  data

- check_string:

  *string* \|\| defaults to `dcon`

  An abbreviated identifier that will be used to label all output from
  this module

## Value

This function will return a list of two dataframes:

- `dcon_output`: A table with counts of patients/visits in the first
  cohort, patients/visits in the second cohort, and patients/visits in
  both cohorts (who also meet the time limit criteria)

- `dcon_meta`: A table with descriptive metadata about each of the
  cohorts for which counts were computed

## Examples

``` r
# First create input file with desired checks to be executed
# You can access examples for both OMOP & PCORnet here:
ndq::dcon_input_omop
#> # A tibble: 4 × 10
#>   cohort_id  check_id   cohort_description schema table date_field concept_field
#>   <chr>      <chr>      <chr>              <chr>  <chr> <chr>      <chr>        
#> 1 asthma_dx  asthma_dx… asthma diagnosis   cdm    cond… condition… condition_co…
#> 2 broncho_rx asthma_dx… bronchodilator (a… cdm    drug… drug_expo… drug_concept…
#> 3 IP_visits  IP_visits… IP visits          cdm    visi… visit_sta… NA           
#> 4 IP_conds   IP_visits… IP condition head… cdm    cond… condition… NA           
#> # ℹ 3 more variables: conceptset_name <chr>, filter_logic <chr>,
#> #   time_between_events <dbl>
ndq::dcon_input_pcornet
#> # A tibble: 4 × 11
#>   cohort_id check_id cohort_description schema table date_field vocabulary_field
#>   <chr>     <chr>    <chr>              <chr>  <chr> <chr>      <chr>           
#> 1 asthma_dx asthma_… asthma diagnosis   cdm    diag… admit_date dx_type         
#> 2 broncho_… asthma_… bronchodilator (a… cdm    pres… rx_start_… NA              
#> 3 IP_visits IP_visi… IP visits          cdm    enco… admit_date NA              
#> 4 IP_conds  IP_visi… IP condition head… cdm    diag… admit_date dx_type         
#> # ℹ 4 more variables: concept_field <chr>, conceptset_name <chr>,
#> #   filter_logic <chr>, time_between_events <dbl>

# Use this as your input to the DCON function
## To execute the check at the patient level:
if (FALSE) { # \dontrun{
my_dcon_rslt <- check_dcon(dcon_tbl = ndq::dcon_input_omop,
                           compute_level = 'patient',
                           omop_or_pcornet = 'omop',
                           check_string = 'dcon')
} # }

## To execute the check at the visit level:
if (FALSE) { # \dontrun{
my_dcon_rslt <- check_dcon(dcon_tbl = ndq::dcon_input_omop,
                           compute_level = 'visit',
                           omop_or_pcornet = 'omop',
                           check_string = 'dcon')
} # }
```
