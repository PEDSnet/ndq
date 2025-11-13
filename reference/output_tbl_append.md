# output table to database if it does not exist, or append it to an existing table with the same name if it does

output table to database if it does not exist, or append it to an
existing table with the same name if it does

## Usage

``` r
output_tbl_append(
  data,
  name = NA,
  local = FALSE,
  file = base::ifelse(config("results_target") == "file", TRUE, FALSE),
  db = if (!file) config("results_target") else NA,
  results_tag = TRUE,
  ...
)
```

## Arguments

- data:

  the data to output

- name:

  the name of the table to output

- local:

  description

- file:

  blah

- db:

  blah

- results_tag:

  blah

- ...:

  other arguments

  Parameters are the same as `output_tbl`

## Value

The table as it exists on the databse, with the new data appended, if
the table already existts.
