# Unmapped Concepts – Processing

Intakes the output of `check_uc` in order to apply additional
processing. This includes either adding proportions (for longitudinal
output) or computing overall totals across all sites included in the
input (for non-longitudinal output)

## Usage

``` r
process_uc(uc_results, rslt_source = "remote", csv_rslt_path = NULL)
```

## Arguments

- uc_results:

  *tabular input* \|\| **required**

  The tabular output of `check_uc`. This table should include results
  for all institutions that should be included in the computation of
  overall / "network level" statistics.

- rslt_source:

  *string* \|\| defaults to `remote`

  A string that identifies the location of the `uc_results` table.
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

This function will return all columns from the `uc_results` input with
either an additional column with unmapped proportions (by_year) or with
additional rows that include total unmapped counts/proportions across
all sites (not by_year)

## Examples

``` r
# This function should be run after check_uc has been executed for all
# network institutions and results have been combined into a common table

# Once the labels have been applied, the function can be executed
## When results are kept locally:
if (FALSE) { # \dontrun{
my_uc_process <- process_uc(uc_results = my_uc_rslts,
                            rslt_source = 'local')
} # }

## When results are kept in CSV files:
if (FALSE) { # \dontrun{
my_uc_process <- process_uc(uc_results = 'my_uc_rslts',
                            rslt_source = 'csv',
                            csv_rslt_path = 'path/to/my/results')
} # }

## When results are kept on a remote database:
if (FALSE) { # \dontrun{
my_uc_process <- process_uc(uc_results = 'my_uc_rslts',
                            rslt_source = 'remote')
} # }
```
