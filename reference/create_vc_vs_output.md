# Additional processing for VS & VC checks

Additional processing for VS & VC checks

## Usage

``` r
create_vc_vs_output(tbl_list, check_string = "vc")
```

## Arguments

- tbl_list:

  a list that contains all the vc or vs violations

- check_string:

  a string that contains the table name for the check output

## Value

a pivoted version of the input table with dummy rows added for checks
that did not return any violations
