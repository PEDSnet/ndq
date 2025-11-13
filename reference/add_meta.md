# add check name, db version, and site name to a given table

add check name, db version, and site name to a given table

## Usage

``` r
add_meta(
  tbl_meta,
  check_lib,
  version = config("current_version"),
  site_nm = config("qry_site")
)
```

## Arguments

- tbl_meta:

  the table to add meta information to

- check_lib:

  the name of the check

- version:

  the version of the database; defaults to `config('current_version')`;

- site_nm:

  the name of the site; defaults to `config('site')`
