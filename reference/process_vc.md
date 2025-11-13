# Vocabulary Conformance – Processing

Intakes the output of `check_vc` in order to apply additional
processing. This includes computing row and patient proportions and
computing overall totals across all sites included in the input.

## Usage

``` r
process_vc(vc_results, rslt_source = "remote", csv_rslt_path = NULL)
```

## Arguments

- vc_results:

  *tabular input* \|\| **required**

  The tabular output of `check_vc`. This table should include results
  for all institutions that should be included in the computation of
  overall / "network level" statistics.

- rslt_source:

  *string* \|\| defaults to `remote`

  A string that identifies the location of the `vc_results` table.
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

This function will return a list that contains two dataframes:
-`vc_processed`: The `vc_results` table with additional rows reflecting
the overall / "network level" counts and a computed proportion of
violating vocabularies -`vc_violations`: A table listing all of the
vocabulary violations

## Examples

``` r
# This function should be run after check_vc has been executed for all
# network institutions and results have been combined into a common table

# Once the labels have been applied, the function can be executed
## When results are kept locally:
if (FALSE) { # \dontrun{
my_vc_process <- process_vc(vc_results = my_vc_rslts,
                            rslt_source = 'local')
} # }

## When results are kept in CSV files:
if (FALSE) { # \dontrun{
my_vc_process <- process_vc(vc_results = 'my_vc_rslts',
                            rslt_source = 'csv',
                            csv_rslt_path = 'path/to/my/results')
} # }

## When results are kept on a remote database:
if (FALSE) { # \dontrun{
my_vc_process <- process_vc(vc_results = 'my_vc_rslts',
                            rslt_source = 'remote')
} # }
```
