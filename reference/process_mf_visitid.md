# Missing Field: Visit ID – Processing

Intakes the output of `check_mf_visitid` in order to apply additional
processing. This includes computing proportions of missing visits and
computing overall totals across all sites included in the input.

## Usage

``` r
process_mf_visitid(
  mf_visitid_results,
  rslt_source = "remote",
  csv_rslt_path = NULL
)
```

## Arguments

- mf_visitid_results:

  *tabular input* \|\| **required**

  The tabular output of `check_mf_visitid`. This table should include
  results for all institutions that should be included in the
  computation of overall / "network level" statistics.

- rslt_source:

  *string* \|\| defaults to `remote`

  A string that identifies the location of the `mf_visitid_results`
  table. Acceptable values are

  - `local` - table is stored as a dataframe in the local R environment

  - `csv` - table is stored as a CSV file

  - `remote` - table is stored on a remote database

- csv_rslt_path:

  *string* \|\| defaults to `NULL`

  If `rslt_source` has been set to `csv`, this parameter should indicate
  the path to the result file(s). Otherwise, this parameter can be left
  as `NULL`

## Value

This function will return the `mf_visitid_results` table with the
addition of a column that reflects proportion of missing visits

## Examples

``` r
# This function should be run after check_mf_visitid has been executed for all
# network institutions and results have been combined into a common table

# Once the labels have been applied, the function can be executed
## When results are kept locally:
if (FALSE) { # \dontrun{
my_mf_visitid_process <- process_mf_visitid(mf_visitid_results = my_mf_visitid_rslts,
                                            rslt_source = 'local')
} # }

## When results are kept in CSV files:
if (FALSE) { # \dontrun{
my_mf_visitid_process <- process_mf_visitid(mf_visitid_results = 'my_mf_visitid_rslts',
                                            rslt_source = 'csv',
                                            csv_rslt_path = 'path/to/my/results')
} # }

## When results are kept on a remote database:
if (FALSE) { # \dontrun{
my_mf_visitid_process <- process_mf_visitid(mf_visitid_results = 'my_mf_visitid_rslts',
                                            rslt_source = 'remote')
} # }
```
