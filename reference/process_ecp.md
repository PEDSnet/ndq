# Expected Concepts Present – Processing

Intakes the output of `check_ecp` in order to apply additional
processing. This includes creating a new `check_name_app` column to
specify that the check was computed at the person level.

## Usage

``` r
process_ecp(ecp_results, rslt_source = "remote", csv_rslt_path = NULL)
```

## Arguments

- ecp_results:

  *tabular input* \|\| **required**

  The tabular output of `check_ecp`. This table should include results
  for all institutions that should be included in the computation of
  overall / "network level" statistics.

- rslt_source:

  *string* \|\| defaults to `remote`

  A string that identifies the location of the `ecp_results` table.
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

This function will return the `ecp_results` table with and additional
`check_name_app` column to indicate application level

## Examples

``` r
# This function should be run after check_ecp has been executed for all
# network institutions and results have been combined into a common table

# Once the labels have been applied, the function can be executed
## When results are kept locally:
if (FALSE) { # \dontrun{
my_ecp_process <- process_ecp(ecp_results = my_ecp_rslts,
                              rslt_source = 'local')
} # }

## When results are kept in CSV files:
if (FALSE) { # \dontrun{
my_ecp_process <- process_ecp(ecp_results = 'my_ecp_rslts',
                              rslt_source = 'csv',
                              csv_rslt_path = 'path/to/my/results')
} # }

## When results are kept on a remote database:
if (FALSE) { # \dontrun{
my_ecp_process <- process_ecp(ecp_results = 'my_ecp_rslts',
                              rslt_source = 'remote')
} # }
```
