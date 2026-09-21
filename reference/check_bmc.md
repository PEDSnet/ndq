# Best Mapped Concepts

This function will identify the existing concepts within the specified
field so the user can assess which of these concepts are acceptable
("best") or should not be used in that field ("not best")

## Usage

``` r
check_bmc(
  bmc_tbl,
  best_notbest_tbl,
  omop_or_pcornet = "omop",
  concept_tbl = NULL,
  check_string = "bmc"
)
```

## Arguments

- bmc_tbl:

  *tabular input* \|\| **required**

  The primary input table that contains descriptive information about
  the checks to be executed by the function. It should include
  definitions the fields that should be evaluated to determine if they
  only include "best" concepts. see
  [`?bmc_input_omop`](https://pedsnet.github.io/ndq/reference/bmc_input_omop.md)
  or
  [`?bmc_input_pcornet`](https://pedsnet.github.io/ndq/reference/bmc_input_pcornet.md)
  for examples of the input structure

- best_notbest_tbl:

  *tabular input* \|\| **required**

  A table indicating the best/not best concept designations for each
  check. See
  [`?bmc_best_notbest`](https://pedsnet.github.io/ndq/reference/bmc_best_notbest.md)
  for an example of this input structure.

  If it is easier to list the concepts that are best, like if there are
  a limited number of acceptable concepts but many unacceptable
  concepts, the `best_notbest` column should be set to `1` and the
  `default_to` column should be set to `notbest`. If the inverse is true
  and it is easier to list the unacceptable concepts, the `best_notbest`
  column should be set to `0` and the `default_to` column should be set
  to `best`.

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

This function will return a table with one row for each concept present
in each user-defined field, the associated row and patient
counts/proportions, and a label indicating whether the concept is
considered "best" or "not best"

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

# Then, create a table that indicates specific concepts that you
# want to consider "best" or "not best," and indicate your preference
# for whether other concepts should default to one or the other.
ndq::bmc_best_notbest
#> # A tibble: 18 × 5
#>    check_name concept             best_notbest default_to  ...5
#>    <chr>      <chr>                      <dbl> <chr>      <dbl>
#>  1 rxnorm_di  Quant Branded Drug             1 notbest       NA
#>  2 rxnorm_di  Branded Drug                   1 notbest        1
#>  3 rxnorm_di  Branded Pack                   1 notbest        1
#>  4 rxnorm_di  Clinical Drug                  1 notbest        1
#>  5 rxnorm_di  Clinical Pack                  1 notbest        1
#>  6 rxnorm_di  Quant Clinical Drug            1 notbest       NA
#>  7 rxnorm_di  Quant Branded Drug             1 notbest       NA
#>  8 rxnorm_di  Branded Drug                   1 notbest       NA
#>  9 rxnorm_di  Branded Pack                   1 notbest       NA
#> 10 rxnorm_di  Clinical Drug                  1 notbest       NA
#> 11 rxnorm_di  Clinical Pack                  1 notbest       NA
#> 12 rxnorm_di  Quant Clinical Drug            1 notbest       NA
#> 13 race       Refuse to answer               0 best          NA
#> 14 race       No information                 0 best          NA
#> 15 race       Unknown                        0 best          NA
#> 16 ethnicity  Refuse to answer               0 best          NA
#> 17 ethnicity  No information                 0 best          NA
#> 18 ethnicity  Unknown                        0 best          NA

# Use this as your input to the BMC function
if (FALSE) { # \dontrun{
my_bmc_rslt <- check_bmc(bmc_tbl = ndq::bmc_input_omop,
                         best_notbest_tbl = ndq::bmc_best_notbest,
                         omop_or_pcornet = 'omop',
                         concept_tbl = vocabulary_tbl("concept"), ## points to OHDSI concept table
                         check_string = 'bmc')
} # }

```
