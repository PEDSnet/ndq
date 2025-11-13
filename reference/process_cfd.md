# Clinical Fact Documentation – Processing

Intakes the combined output of `check_cfd` from all relevant
institutions in order to apply additional processing. It will compute
overall counts/proportions across all sites included in the input and
tidy some of the descriptive metadata.

## Usage

``` r
process_cfd(cfd_results, rslt_source = "remote", csv_rslt_path = NULL)
```

## Arguments

- cfd_results:

  *tabular input* \|\| **required**

  The tabular output of `check_cfd`. This table should include results
  for all institutions that should be included in the computation of
  overall / "network level" statistics.

- rslt_source:

  *string* \|\| defaults to `remote`

  A string that identifies the location of the `cfd_results` table.
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

This function will return all columns from the `cfd_results` input with
additional rows where site = `total` that reflect the combined results
of all institutions provided in the input data.

## Examples

``` r
# This function should be run after check_cfd has been executed for all
# network institutions and results have been combined into a common table

## When results are kept locally:
if (FALSE) { # \dontrun{
my_cfd_process <- process_cfd(cfd_results = my_cfd_rslts,
                              rslt_source = 'local')
} # }

## When results are kept in CSV files:
if (FALSE) { # \dontrun{
my_cfd_process <- process_cfd(cfd_results = 'my_cfd_rslts',
                              rslt_source = 'csv',
                              csv_rslt_path = 'path/to/my/results')
} # }

## When results are kept on a remote database:
if (FALSE) { # \dontrun{
my_cfd_process <- process_cfd(cfd_results = 'my_cfd_rslts',
                              rslt_source = 'remote')
} # }

```
