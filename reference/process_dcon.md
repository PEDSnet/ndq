# Domain Concordance – Processing

Intakes the output of `check_dcon` in order to apply additional
processing. This includes computing the following cohort overlap counts
and proportions:

- `cohort_1_only`: overall, patients in just cohort 1

- `cohort_2_only`: overall, patients in just cohort 2

- `combined`: overall, patients in both 1 and 2

- `cohort_1_denom`: patients in cohort 1 not cohort 2

- `cohort_2_denom`: patients in cohort 2 not cohort 1

- `cohort_1_in_2`: patients in cohort 2 who are also in 1 (use cohort 2
  as denominator)

- `cohort_2_in_1`: patients in cohort 1 who are also in 2 (use cohort 1
  as denominator)

## Usage

``` r
process_dcon(dcon_results, rslt_source = "remote", csv_rslt_path = NULL)
```

## Arguments

- dcon_results:

  *tabular input* \|\| **required**

  The `dcon_counts` output of `check_dcon`. This table should include
  results for all institutions that should be included in the
  computation of overall / "network level" statistics.

- rslt_source:

  *string* \|\| defaults to `remote`

  A string that identifies the location of the `dcon_results` table.
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

This function will return a dataframe that, for each cohort pair,
contains one row for each cohort overlap computation type listed in the
description, including the associated raw counts and proportions

## Details

Note that `cohort_1_in_2` and `cohort_2_in_1` will have the same raw
count, but different proportions since the denominator is different

## Examples

``` r
# This function should be run after check_dcon has been executed for all
# network institutions and results have been combined into a common table

# Once the labels have been applied, the function can be executed
## When results are kept locally:
if (FALSE) { # \dontrun{
my_dcon_process <- process_dcon(dcon_results = my_dcon_rslts,
                                rslt_source = 'local')
} # }

## When results are kept in CSV files:
if (FALSE) { # \dontrun{
my_dcon_process <- process_dcon(dcon_results = 'my_dcon_rslts',
                                rslt_source = 'csv',
                                csv_rslt_path = 'path/to/my/results')
} # }

## When results are kept on a remote database:
if (FALSE) { # \dontrun{
my_dcon_process <- process_dcon(dcon_results = 'my_dcon_rslts',
                                rslt_source = 'remote')
} # }
```
