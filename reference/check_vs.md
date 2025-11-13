# Valueset Conformance

This function will intake a limited valueset that is expected to make up
the entire contents of a field (minus the specified `null_values`) and
identify if any non-permitted values exist in the field (and how often).

## Usage

``` r
check_vs(
  vs_tbl,
  omop_or_pcornet = "omop",
  concept_tbl = NULL,
  null_values = c(44814650L, 0L, 44814653L, 44814649L),
  check_string = "vs"
)
```

## Arguments

- vs_tbl:

  *tabular input* \|\| **required**

  The primary input table that contains descriptive information about
  the checks to be executed by the function. It should include
  definitions for the fields and associated valuesets of interest. see
  [`?vs_input_omop`](https://pedsnet.github.io/ndq/reference/vs_input_omop.md)
  or
  [`?vs_input_pcornet`](https://pedsnet.github.io/ndq/reference/vs_input_pcornet.md)
  for examples of the input structure

- omop_or_pcornet:

  omop_or_pcornet *string* \|\| defaults to `omop`

  A string, either `omop` or `pcornet`, indicating the CDM format of the
  data

- concept_tbl:

  *tabular input* \|\| defaults to `NULL`

  An optional parameter used to define a vocabulary table with concept
  definitions (for example, the OHDSI concept table). If left NULL, the
  concepts as they exist in the fact table will be returned to the user.

- null_values:

  *string / vector* \|\| defaults to 44814650, 0, 44814653, & 44814649

  A string or vector listing the concept(s) that indicate a NULL value,
  which will be excluding when assessing for the presence of values not
  present in the valueset.

- check_string:

  *string* \|\| defaults to `vs`

  An abbreviated identifier that will be used to label all output from
  this module

## Value

This function will return a dataframe with summary information about
each value that appears in the data but does not comply with the
valueset definition. If no violations are identified for a particular
check, a placeholder row will dummy information will be inserted
instead.

## Examples

``` r
# First create input file with desired checks to be executed
# You can access examples for both OMOP & PCORnet here:
ndq::vs_input_omop
#> # A tibble: 2 × 6
#>   check_id    schema table  valueset_name      concept_field        filter_logic
#>   <chr>       <chr>  <chr>  <chr>              <chr>                <lgl>       
#> 1 pd_race_cid cdm    person valueset_race      race_concept_id      NA          
#> 2 pd_eth_cid  cdm    person valueset_ethnicity ethnicity_concept_id NA          
ndq::vs_input_pcornet
#> # A tibble: 2 × 7
#>   check_id    schema table       valueset_name    concept_field vocabulary_field
#>   <chr>       <chr>  <chr>       <chr>            <chr>         <lgl>           
#> 1 pd_race_cid cdm    demographic valueset_race    race          NA              
#> 2 pd_eth_cid  cdm    demographic valueset_ethnic… hispanic      NA              
#> # ℹ 1 more variable: filter_logic <lgl>

# Use this as your input to the VS function
if (FALSE) { # \dontrun{
my_vs_rslt <- check_vs(vs_tbl = ndq::vs_input_omop,
                       omop_or_pcornet = 'omop',
                       concept_tbl = vocabulary_tbl("concept"), ## points to OHDSI concept table
                       null_values = c(44814650L,0L,
                                       44814653L,44814649L), # ignored illegal values
                       check_string = 'vs')
} # }
```
