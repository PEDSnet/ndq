# Facts Over Time – Processing

Intakes the output of check_fot in order to apply additional processing.
This includes applying a heuristic meant to compare the fact count in a
given time period to other time periods around it. For a monthly
computation, for example, the heuristic would be
`month / ((month-1 * .25) + (month+1 * .25) + (month-12 * .5))` which is
the value for a given month divided by the weighted average of the value
in the previous month, the next month, and the same month in the
previous year. The function can also optionally compute a ratio given a
`total_pt` column (added by the user) to be used as a denominator and a
`ratio_mult` value to be used as a multiplier.

## Usage

``` r
process_fot(
  fot_results,
  target_col = "row_cts",
  add_ratios = FALSE,
  ratio_mult = 10000,
  rslt_source = "remote",
  csv_rslt_path = NULL
)
```

## Arguments

- fot_results:

  *tabular input* \|\| **required**

  The tabular output of `check_fot`. This table should include results
  for all institutions that should be included in the computation of
  overall / "network level" statistics.

- target_col:

  *string* \|\| defaults to `row_cts`

  A string indicating the column in `fot_results` that should be used to
  compute the primary heuristic. For the ratio computation, `row_pts`
  will *always* be used regardless of what is specified in this
  parameter.

- add_ratios:

  *boolean* \|\| defaults to `FALSE`

  A boolean indicating whether incidence ratios / rates should be
  computed. if TRUE, the user should edit the `fot_results` table to
  include an additional `total_pt` column with counts for the
  denominator of the user's choosing. We recommend including the desired
  denominator population as a check passed into the `check_fot` function
  to facilitate the computation of patient counts for each time period.

- ratio_mult:

  *integer* \|\| defaults to `10,000`

  If `add_ratios` is set to `TRUE`, this parameter should represent the
  numerical multiplier that should be used to compute the incidence
  ratio. This will result in a ratio that represents the fact rate per
  (`ratio_mult`) patients.

- rslt_source:

  *string* \|\| defaults to `remote`

  A string that identifies the location of the `fot_results` table.
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

This function will return a list of up to three dataframes:

- `fot_heuristic`: a table with the summary heuristic for each fact type
  (month / ((month-1) \* .25 + (month+1) \* .25 + (month-12)\*.5))

- `fot_heuristic_summary`: a table with summary values (mean, med, sd,
  q1, q3) based on the heuristic

- `fot_ratios`: the `fot_results` table with an additional column with
  the incidence ratio (only returned if `row_ratio = TRUE`)

## Examples

``` r
# This function should be run after check_fot has been executed for all
# network institutions and results have been combined into a common table

# Once the labels have been applied, the function can be executed
## When results are kept locally:
if (FALSE) { # \dontrun{
my_fot_process <- process_fot(fot_results = my_fot_rslts,
                              target_col = 'row_cts',
                              rslt_source = 'local')
} # }

## When results are kept in CSV files:
if (FALSE) { # \dontrun{
my_fot_process <- process_fot(fot_results = 'my_fot_rslts',
                              target_col = 'row_cts',
                              rslt_source = 'csv',
                              csv_rslt_path = 'path/to/my/results')
} # }

## When results are kept on a remote database:
if (FALSE) { # \dontrun{
my_fot_process <- process_fot(fot_results = 'my_fot_rslts',
                              target_col = 'row_cts',
                              rslt_source = 'remote')
} # }

# You can also optionally compute patient incidence ratios. This computation will use
# the patient count, regardless of what is specified in the `target_col` argument.
# A column with the total patient count for that time period will need to be added to
# the results output. It can reflect any patient cohort, so long as the column is called
# `total_pt`. We recommend computing the denominator as part of the initial check_fot execution
# and extracting the counts from the resulting output.

if (FALSE) { # \dontrun{
my_fot_process <- process_fot(fot_results = 'my_fot_rslts',
                              target_col = 'row_cts',
                              add_ratios = TRUE,
                              ratio_mult = 10000,
                              rslt_source = 'remote')
} # }
```
