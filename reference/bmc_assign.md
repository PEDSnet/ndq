# assign "best" "not best" labels to concepts

assign "best" "not best" labels to concepts

## Usage

``` r
bmc_assign(bmc_output, conceptset)
```

## Arguments

- bmc_output:

  the bmc_counts output from check_bmc

- conceptset:

  the bmc_concepts output from check_bmc with an additional column
  called "include" added with "not best" or unideal concepts marked with
  a 0

## Value

a dataframe with the include designations attached to all concepts; also
checks to ensure no additional values are present
