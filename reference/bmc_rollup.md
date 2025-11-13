# Function to compute proportion of "best" based on output from bmc check

Function to compute proportion of "best" based on output from bmc check

## Usage

``` r
bmc_rollup(bmc_output_pp)
```

## Arguments

- bmc_output_pp:

  table output from the bmc_assign function, which has all the columns
  output from the bmc check + an indicator column for whether the
  concept should be in the "best" category

## Value

table with the cols: site, check_type, database_version, check_name,
check_desc, count_best, include, total_rows, total_pts, best_row_prop,
best_pts_prop
