# Best Mapped Concepts – Processing

Intakes the output of `check_bmc` in order to apply additional
processing. This includes applying the user-specified best/not best
labels that were added to the `bmc_concepts` table, then using those
labels to compute proportions of best vs not best concept representation
in each check.

## Usage

``` r
process_bmc(bmc_results, rslt_source = "remote", csv_rslt_path = NULL)
```

## Arguments

- bmc_results:

  *tabular input* \|\| **required**

  The output of `check_bmc`. This table should include results for all
  institutions that should be included in the computation of overall /
  "network level" statistics.

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

A table summarizing the proportion of best vs not best concepts for a
given check, indicated by the user designation provided in the
`check_bmc` results

## Examples

``` r
# This function should be run after check_bmc has been executed for all
# network institutions and results have been combined into a common table

## When results are kept locally:
if (FALSE) { # \dontrun{
my_bmc_process <- process_bmc(bmc_results = my_bmc_rslts,
                              rslt_source = 'local')
} # }

## When results are kept in CSV files:
if (FALSE) { # \dontrun{
my_bmc_process <- process_bmc(bmc_results = 'my_bmc_rslts',
                              rslt_source = 'csv',
                              csv_rslt_path = 'path/to/my/results')
} # }

## When results are kept on a remote database:
if (FALSE) { # \dontrun{
my_bmc_process <- process_bmc(bmc_results = 'my_bmc_rslts',
                              rslt_source = 'remote')
} # }
```
