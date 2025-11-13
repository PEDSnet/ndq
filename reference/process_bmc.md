# Best Mapped Concepts – Processing

Intakes the output of `check_bmc` in order to apply additional
processing. This includes applying the user-specified best/not best
labels that were added to the `bmc_concepts` table, then using those
labels to compute proportions of best vs not best concept representation
in each check.

## Usage

``` r
process_bmc(
  bmc_results,
  bmc_concepts_labelled,
  rslt_source = "remote",
  csv_rslt_path = NULL
)
```

## Arguments

- bmc_results:

  *tabular input* \|\| **required**

  The `bmc_counts` output of `check_bmc`. This table should include
  results for all institutions that should be included in the
  computation of overall / "network level" statistics.

- bmc_concepts_labelled:

  *tabular input* \|\| **required**

  The `bmc_concepts` output of `check_bmc`, with an additional column
  called `include` added with "not best" or non-ideal concepts marked
  with a 0 (optionally, "best" concepts can also be marked with a 1)

- rslt_source:

  *string* \|\| defaults to `remote`

  A string that identifies the location of the `bmc_results` table.
  Acceptable values are

  - `local` - table is stored as a dataframe in the local R environment

  - `csv` - table is stored as a CSV file

  - `remote` - table is stored on a remote database

- csv_rslt_path:

  *string* \|\| defaults to `NULL`

  If `rslt_source` has been set to `csv`, this parameter should indicate
  the path to the result file(s). Otherwise, this parameter can be left
  as `NULL`

## Value

This function will return a list of two dataframes:

- `bmc_output_pp`: A table summarizing the proportion of best vs not
  best concepts for a given check, indicated by the user designation in
  the `bmc_concepts_labelled` table

- `bmc_concepts_pp`: The `bmc_results` input table with the best / not
  best designations added

## Examples

``` r
# This function should be run after check_bmc has been executed for all
# network institutions and results have been combined into a common table

# All returned concepts should also be labelled to indicate whether they
# should be included as a "best" concept or not:

readr::read_csv(system.file('extdata', 'bmc_concept_examples.csv', package = 'ndq'))
#> Rows: 6 Columns: 3
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (2): check_name, concept
#> dbl (1): include
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> # A tibble: 6 × 3
#>   check_name    concept                   include
#>   <chr>         <chr>                       <dbl>
#> 1 bmc_race      White                          NA
#> 2 bmc_race      Black or African American      NA
#> 3 bmc_race      Unknown                         0
#> 4 bmc_ethnicity Hispanic                       NA
#> 5 bmc_ethnicity Non-Hispanic                   NA
#> 6 bmc_ethnicity Other                           0

# Once the labels have been applied, the function can be executed
## When results are kept locally:
if (FALSE) { # \dontrun{
my_bmc_process <- process_bmc(bmc_results = my_bmc_rslts,
                              bmc_concepts_labelled = my_bmc_concepts,
                              rslt_source = 'local')
} # }

## When results are kept in CSV files:
if (FALSE) { # \dontrun{
my_bmc_process <- process_bmc(bmc_results = 'my_bmc_rslts',
                              bmc_concepts_labelled = 'my_bmc_concepts',
                              rslt_source = 'csv',
                              csv_rslt_path = 'path/to/my/results')
} # }

## When results are kept on a remote database:
if (FALSE) { # \dontrun{
my_bmc_process <- process_bmc(bmc_results = 'my_bmc_rslts',
                              bmc_concepts_labelled = 'my_bmc_concepts',
                              rslt_source = 'remote')
} # }
```
