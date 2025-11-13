# Data Cycle Changes – Processing

Intakes the output of `check_dc` in order to apply additional
processing. This includes computing percent change across data model
versions and computing an overall set of counts/percent changes across
all sites included in the input.

## Usage

``` r
process_dc(
  dc_ct_results,
  dc_meta_results,
  rslt_source = "remote",
  csv_rslt_path = NULL
)
```

## Arguments

- dc_ct_results:

  *tabular input* \|\| **required**

  The `dc_cts` output of `check_dc`. This table should include results
  for all institutions that should be included in the computation of
  overall / "network level" statistics.

- dc_meta_results:

  *tabular input* \|\| **required**

  The `dc_meta` output of `check_dc`

- rslt_source:

  *string* \|\| defaults to `remote`

  A string that identifies the location of the `dc_results` table.
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

This function will return the `dc_ct_results` table with overall /
"network-wide" counts and the proportion change between the two data
models

The `dc_meta_results` table remains unchanged, so the original version
returned by `check_dc` can be used if needed

## Examples

``` r
# This function should be run after check_dc has been executed for all
# network institutions and results have been combined into a common table

## When results are kept locally:
if (FALSE) { # \dontrun{
my_dc_process <- process_dc(dc_ct_results = my_dc_cts,
                            dc_meta_results = my_dc_meta,
                            rslt_source = 'local')
} # }

## When results are kept in CSV files:
if (FALSE) { # \dontrun{
my_dc_process <- process_dc(dc_ct_results = 'my_dc_cts',
                            dc_meta_results = 'my_dc_meta',
                            rslt_source = 'csv',
                            csv_rslt_path = 'path/to/my/results')
} # }

## When results are kept on a remote database:
if (FALSE) { # \dontrun{
my_dc_process <- process_dc(dc_ct_results = 'my_dc_cts',
                            dc_meta_results = 'my_dc_meta',
                            rslt_source = 'remote')
} # }

```
