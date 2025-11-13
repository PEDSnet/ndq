# Add an anonymized site identifier to all tables

This function will iterate through the provided list of result tables to
apply consistent anonymization of site names across all output.

## Usage

``` r
attach_anon_id(all_sites_tbl, tbls_to_anon)
```

## Arguments

- all_sites_tbl:

  *tabular input* \|\| **required**

  A reference table that contains a column called `site` with all of the
  sites that should be masked in each of the provided result tables

- tbls_to_anon:

  *named list* \|\| **required**

  A **NAMED** list of all of the tables that should be anonymized

## Value

This function will return each of the original tables in `tbls_to_anon`
with all the original columns plus a column called `site_anon` with a
masked identifier that is consistent across all of the tables in
`tbls_to_anon`
