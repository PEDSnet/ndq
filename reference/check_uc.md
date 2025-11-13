# Unmapped Concepts

This function will evaluate the count and proportion of unmapped
concepts associated with the fact type of interest. If
`produce_mapped_list` is set to TRUE, a summary of the source values
associated with unmapped concepts will also be produced to help identify
areas where mappings could potentially be improved. This analysis can be
also be executed longitudinally by year.

## Usage

``` r
check_uc(
  uc_tbl,
  omop_or_pcornet = "omop",
  by_year = FALSE,
  produce_mapped_list = TRUE,
  unmapped_values = c(44814650L, 0L, 44814653L, 44814649L),
  check_string = "uc"
)
```

## Arguments

- uc_tbl:

  *tabular input* \|\| **required**

  The primary input table that contains descriptive information about
  the checks to be executed by the function. It should include
  definitions for the fields that should be evaluated for missingness
  and other relevant metadata. see
  [`?uc_input_omop`](https://pedsnet.github.io/ndq/reference/uc_input_omop.md)
  or
  [`?uc_input_pcornet`](https://pedsnet.github.io/ndq/reference/uc_input_pcornet.md)
  for examples of the input structure

- omop_or_pcornet:

  *string* \|\| defaults to `omop`

  A string, either `omop` or `pcornet`, indicating the CDM format of the
  data

- by_year:

  *boolean* \|\| defaults to `FALSE`

  A boolean indicating whether the analysis should be conducted
  longitudinally by year

  **Note:** the mapped list functionality is NOT available for the
  longitudinal analysis

- produce_mapped_list:

  *boolean* \|\| defaults to `TRUE`

  A boolean indicating whether a list of source values associated with
  unmapped concepts should be produced in addition to the primary
  output. This table will be iteratively output to the database backend
  of choice and will NOT be stored locally. Only source values with \>
  10 occurrences are included.

- unmapped_values:

  *string / vector* \|\| defaults to 44814650, 0, 44814653, & 44814649

  A string or vector listing the concept(s) that indicate an unmapped
  value. The function will check for NULL values by default, so that
  does not need to be defined here.

- check_string:

  *string* \|\| defaults to `uc`

  An abbreviated identifier that will be used to label all output from
  this module

## Value

If `by_year` is `FALSE`, this function will produce a dataframe with the
total row count, the unmapped row count, the proportion of unmapped
values, and some additional descriptive metadata for each user-defined
check

If `by_year` is `TRUE`, this function will produce a dataframe with the
total row count, the unmapped row count, the proportion of unmapped
values, and some additional descriptive metadata for each user-defined
check stratified by each year present in the fact table

If `produce_mapped_list` is `TRUE`, then a table with name `uc_grpd`
that includes the source values (with \> 10 appearances) and counts of
those values associated with unmapped concepts will be produced in
addition to the primary tabular output

## Examples

``` r
# First create input file with desired checks to be executed
# You can access examples for both OMOP & PCORnet here:
ndq::uc_input_omop
#> # A tibble: 9 × 8
#>   check_id check_description       schema table concept_field source_value_field
#>   <chr>    <chr>                   <chr>  <chr> <chr>         <chr>             
#> 1 dr       all drugs               cdm    drug… drug_concept… drug_source_value 
#> 2 di       inpatient administrati… cdm    drug… drug_concept… drug_source_value 
#> 3 dp       prescription drugs      cdm    drug… drug_concept… drug_source_value 
#> 4 du       drug dose unit          cdm    drug… dose_unit_co… dose_unit_source_…
#> 5 drt      drug route              cdm    drug… route_concep… route_source_value
#> 6 co       conditions              cdm    cond… condition_co… condition_source_…
#> 7 co_scid  condition source        cdm    cond… condition_so… condition_source_…
#> 8 po       ordered procedures      cdm    proc… procedure_co… procedure_source_…
#> 9 pb       billed procedures       cdm    proc… procedure_co… procedure_source_…
#> # ℹ 2 more variables: date_field <chr>, filter_logic <chr>
ndq::uc_input_pcornet
#> # A tibble: 7 × 8
#>   check_id check_description       schema table concept_field source_value_field
#>   <chr>    <chr>                   <chr>  <chr> <chr>         <chr>             
#> 1 di       inpatient administrati… cdm    med_… medadmin_code raw_medadmin_med_…
#> 2 dp       prescription drugs      cdm    pres… rxnorm_cui    raw_rx_med_name   
#> 3 du       rx dose unit            cdm    pres… rx_dose_orde… raw_rx_dose_order…
#> 4 drt      rx route                cdm    pres… rx_route      raw_rx_route      
#> 5 co       diagnoses               cdm    diag… dx            raw_dx            
#> 6 po       ordered procedures      cdm    proc… px            raw_px            
#> 7 pb       billed procedures       cdm    proc… px            raw_px            
#> # ℹ 2 more variables: date_field <chr>, filter_logic <chr>

# Use this as your input to the UC function
## Overall
if (FALSE) { # \dontrun{
my_uc_rslt <- check_uc(uc_tbl = ndq::uc_input_omop,
                       by_year = FALSE,
                       produce_mapped_list = TRUE,
                       unmapped_values = c(44814650L,0L,
                                           44814653L, 44814649L),
                       omop_or_pcornet = 'omop',
                       check_string = 'uc')
} # }

## By Year
if (FALSE) { # \dontrun{
my_uc_rslt <- check_uc(uc_tbl = ndq::uc_input_omop,
                       by_year = TRUE,
                       produce_mapped_list = FALSE,
                       unmapped_values = c(44814650L,0L,
                                           44814653L, 44814649L),
                       omop_or_pcornet = 'omop',
                       check_string = 'uc')
} # }
```
