# Best Mapped Concepts Best/Not Best File

A sample version of the best/not best designation file to classify
concepts in the result of this module. This exact file is also included
as a CSV in the package if the user wishes to use it, or the structure
can be copied to produce a custom list of checks.

## Usage

``` r
bmc_best_notbest
```

## Format

### `bmc_best_notbest`

A dataframe or CSV file with 4 columns

- check_name:

  The name of the check associated with the concepts of interest, which
  should match what is provided in the `bmc_tbl` input

- concept:

  The concept identifier or concept name (depending on if a vocabulary
  table is provided) that should be classified as best/not best

- best_notbest:

  A binary indicator where 0 indicates the concept is not best /
  unacceptable and 1 indicates the concept is best / acceptable

- default_to:

  An indicator, either `best` or `notbest`, to designate what any
  concepts not listed in this file should be defaulted to
