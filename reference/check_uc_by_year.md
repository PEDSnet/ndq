# Unmapped Concepts by Year

This function will evaluate the count and proportion of unmapped
concepts associated with the fact type of interest, stratified by year.

## Usage

``` r
check_uc_by_year(
  uc_tbl,
  omop_or_pcornet = "omop",
  unmapped_values = c(44814650L, 0L, 44814653L, 44814649L),
  check_string = "uc"
)
```

## Arguments

- uc_tbl:

  dataframe with metadata describing the tables/columns for which
  unmapped concepts should be identified

- omop_or_pcornet:

  string identifying the cdm format of the underlying data

- unmapped_values:

  concepts / other values that indicate an unmapped value

- check_string:

  an abbreviated identifier to identify all output from this module
  defaults to `uc`

## Value

a dataframe with the total row count, the unmapped row count, the
proportion of unmapped values, and some additional descriptive metadata
for each check stratified by each year present in the fact table
