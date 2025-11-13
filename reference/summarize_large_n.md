# Add summary columns for a large N of sites

For analyses that have a high volume of sites in the output, the
standard, site-level results may be difficult to visualize on a plot.
This function will compute high level summary statistics across all
sites to provide another option for visualization. It will also allow
for users to compare the site level output to the overall /
"network-level" statistics.

## Usage

``` r
summarize_large_n(
  dq_output,
  num_col,
  grp_vars,
  check_string,
  time = FALSE,
  shape = "wide"
)
```

## Arguments

- dq_output:

  *tabular output* \|\| **required**

  Any of the output tables that has been created by one of the `check_*`
  functions and has also passed through the associated `process_*`
  function

- num_col:

  *string* \|\| **required**

  A string indicating the name of the numeric column in the `dq_output`
  table that should be the basis for summary statistic computation

- grp_vars:

  *string / vector* \|\| **required**

  A string or vector listing the variables in `dq_output` that should be
  used for grouping when comparing summary statistics.

- check_string:

  *string* \|\| **required**

  A string that matches the check string originally input into the
  analysis

- time:

  *boolean* \|\| defaults to `FALSE`

  A boolean indicating whether the check is time dependent. This should
  only apply for Facts Over Time output or Unmapped Concepts output in
  cases when it was computed longitudinally

- shape:

  *string* \|\| defaults to `wide`

  A string, either `long` or `wide`, indicating whether summary
  statistics should be separate columns in the table (wide) or assigned
  to separate rows with the name of the summary statistic in the site
  column (long)

## Value

This function will return the original `dq_output` table with the
addition of summary statistics based on the numeric column provided.
These will include:

- If `time = TRUE`, the mean and median are returned.

- If `time = FALSE`, the max, min, q1, q3, mean, and median are returned
