# Create informational metadata file

This function will intake a list of result tables and extract relevant
descriptive metadata. This is a convenience function to encourage robust
documentation of the checks that are executed as part of a data quality
assessment.

## Usage

``` r
create_check_metadata(
  check_tbls,
  metadata_file = NULL,
  rslt_source = "remote",
  csv_rslt_path = NULL
)
```

## Arguments

- check_tbls:

  *list* \|\| **required**

  A list of table names from which the complete list of executed checks
  should be extracted

- metadata_file:

  *tabular input* \|\| defaults to `NULL`

  This parameter allows the user to insert a previously generated
  metadata file to which any new analysis should be appended. The
  function will also print a message in the console to highlight new
  checks that will need to be described more fully by the user

- rslt_source:

  *string* \|\| defaults to `remote`

  A string that identifies the location of the tables listed in
  `check_tbls`. Acceptable values are

  - `local` - table is stored as a dataframe in the local R environment

  - `csv` - table is stored as a CSV file

  - `remote` - table is stored on a remote database

- csv_rslt_path:

  *string* \|\| defaults to `NULL`

  If `rslt_source` has been set to `csv`, this parameter should indicate
  the path to the result file(s). Otherwise, this parameter can be left
  as `NULL`

## Value

This function will return a dataframe with the names and other
information related to executed checks from each of the provided
`check_tbls`.

- If a metadata file is provided, this table will include all
  information from this file in addition to newly added checks that need
  additional metadata added

- If a metadata file is NOT provided, it will include all checks with
  some metadata pulled from the tables themselves and other fields left
  blank for the user to fill in
