# Best Mapped Concepts

This function will identify the existing concepts within the specified
field so the user can assess which of these concepts are acceptable
("best") or should not be used in that field ("not best")

## Usage

``` r
check_bmc(
  bmc_tbl,
  omop_or_pcornet = "omop",
  concept_tbl = NULL,
  check_string = "bmc"
)
```

## Arguments

- bmc_tbl:

  *tabular input* \|\| *required*

  The primary input table that contains descriptive information about
  the checks to be executed by the function. It should include
  definitions the fields that should be evaluated to determine if they
  only include "best" concepts. see
  [`?bmc_input_omop`](https://pedsnet.github.io/ndq/reference/bmc_input_omop.md)
  or
  [`?bmc_input_pcornet`](https://pedsnet.github.io/ndq/reference/bmc_input_pcornet.md)
  for examples of the input structure

- omop_or_pcornet:

  *string* \|\| defaults to `omop`

  A string, either `omop` or `pcornet`, indicating the CDM format of the
  data

- concept_tbl:

  *tabular input* \|\| defaults to `NULL`

  An optional parameter used to define a vocabulary table with concept
  definitions (for example, the OHDSI concept table). If left NULL, the
  concepts as they exist in the fact table will be returned to the user.

- check_string:

  *string* \|\| defaults to `bmc`

  An abbreviated identifier that will be used to label all output from
  this module

## Value

This function will return a list of two dataframes:

- `bmc_counts`: A table with one row for each concept present in each
  user-defined field and the associated row and patient
  counts/proportions

- `bmc_concepts`: A table with just the concepts from `bmc_counts`. This
  output is should be labelled with "best" (1) vs "not best" (0)
  indicators in a column called `include` for use in the processing step

## Examples

``` r
# First create input file with desired checks to be executed
# You can access examples for both OMOP & PCORnet here:
ndq::bmc_input_omop
#> # A tibble: 4 × 7
#>   check_id  check_description schema table     concept_field concept_table_field
#>   <chr>     <chr>             <chr>  <chr>     <chr>         <chr>              
#> 1 rxnorm_di inpatient admin   cdm    drug_exp… drug_concept… concept_class_id   
#> 2 rxnorm_dp prescriptions     cdm    drug_exp… drug_concept… concept_class_id   
#> 3 race      race              cdm    person    race_concept… concept_name       
#> 4 ethnicity ethnicity         cdm    person    ethnicity_co… concept_name       
#> # ℹ 1 more variable: filter_logic <chr>
ndq::bmc_input_pcornet
#> # A tibble: 4 × 7
#>   check_id  check_description schema table     concept_field concept_table_field
#>   <chr>     <chr>             <chr>  <chr>     <chr>         <chr>              
#> 1 rxnorm_di inpatient admin   cdm    med_admin medadmin_code concept_class_id   
#> 2 rxnorm_dp prescriptions     cdm    prescrib… rxnorm_cui    concept_class_id   
#> 3 race      race              cdm    demograp… race          concept_name       
#> 4 ethnicity ethnicity         cdm    demograp… hispanic      concept_name       
#> # ℹ 1 more variable: filter_logic <lgl>

# Use this as your input to the BMC function
if (FALSE) { # \dontrun{
my_bmc_rslt <- check_bmc(bmc_tbl = ndq::bmc_input_omop,
                         omop_or_pcornet = 'omop',
                         concept_tbl = vocabulary_tbl("concept"), ## points to OHDSI concept table
                         check_string = 'bmc')
} # }

```
