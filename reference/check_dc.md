# Data Cycle Changes

This function will compute row & patient counts in the specified tables
for both the current data model version and a previous data model
version in order to assess changes across data extractions. If you have
previously executed this function, you have the option to point to a
previous result set instead of recomputing the counts from the CDM.

## Usage

``` r
check_dc(
  dc_tbl,
  omop_or_pcornet = "omop",
  prev_db_string = "v1",
  current_db_string = "v2",
  prev_ct_src = "cdm",
  prev_db = config("db_src"),
  prev_rslt_tbl = "dc_output",
  prev_rslt_schema = config("results_schema"),
  check_string = "dc"
)
```

## Arguments

- dc_tbl:

  *tabular input* \|\| **required**

  The primary input table that contains descriptive information about
  the checks to be executed by the function. It should include
  definitions for the CDM elements that should be compared between the
  current and previous data model versions. see
  [`?dc_input_omop`](https://pedsnet.github.io/ndq/reference/dc_input_omop.md)
  or
  [`?dc_input_pcornet`](https://pedsnet.github.io/ndq/reference/dc_input_pcornet.md)
  for examples of the input structure

- omop_or_pcornet:

  *string* \|\| defaults to `omop`

  A string, either `omop` or `pcornet`, indicating the CDM format of the
  data

- prev_db_string:

  *string* \|\| defaults to `v1`

  A string label indicating the previous CDM version

  If `prev_ct_src` is set to `result`, make sure this matches the
  appropriate database label in the last set of results

- current_db_string:

  *string* \|\| defaults to `v2`

  A string label indicating the current CDM version

- prev_ct_src:

  *string* \|\| defaults to `cdm`

  A string indicating the source from which the counts from the previous
  data model should be extracted:

  - `cdm` will compute the counts based on the previous CDM tables

  - `result` will pull existing counts from a previous instance of
    `check_dc` output

- prev_db:

  *database connection* \|\| defaults to `config('db_src')`

  A database connection object that will connect the function to the
  previous CDM instance or the defined result table

- prev_rslt_tbl:

  *string* \|\| defaults to `dc_output`

  If `prev_ct_src` is set to `result`, this string should reflect name
  of the table where previous results are stored on the database

- prev_rslt_schema:

  *string* \|\| defaults to `config('results_schema')`

  If `prev_ct_src` is set to `result`, this string should reflect name
  of the schema where previous results are stored on the database

- check_string:

  *string* \|\| defaults to `dc`

  An abbreviated identifier that will be used to label all output from
  this module

## Value

This function will return a list with two dataframes:

- `dc_cts`: A table containing the row and (where applicable) person
  counts for each CDM element specified by the user

- `dc_meta`: A table containing the metadata associated with each check
  that appears in `dc_cts`

## Examples

``` r
# First create input file with desired checks to be executed
# You can access examples for both OMOP & PCORnet here:
ndq::dc_input_omop
#> # A tibble: 26 × 8
#>    schema_current table_current schema_prev table_prev filter_logic check_domain
#>    <chr>          <chr>         <chr>       <chr>      <chr>        <chr>       
#>  1 cdm            person        cdm         person     NA           person      
#>  2 cdm            drug_exposure cdm         drug_expo… NA           drug_exposu…
#>  3 cdm            condition_oc… cdm         condition… NA           condition_o…
#>  4 cdm            device_expos… cdm         device_ex… NA           device_expo…
#>  5 cdm            visit_occurr… cdm         visit_occ… NA           visit_occur…
#>  6 cdm            condition_oc… cdm         condition… condition_t… condition_o…
#>  7 cdm            condition_oc… cdm         condition… condition_t… condition_i…
#>  8 cdm            condition_oc… cdm         condition… condition_t… condition_ed
#>  9 cdm            condition_oc… cdm         condition… condition_t… condition_o…
#> 10 cdm            condition_oc… cdm         condition… condition_t… condition_o…
#> # ℹ 16 more rows
#> # ℹ 2 more variables: check_id <chr>, check_description <chr>
ndq::dc_input_pcornet
#> # A tibble: 22 × 8
#>    schema_current table_current schema_prev table_prev filter_logic check_domain
#>    <chr>          <chr>         <chr>       <chr>      <chr>        <chr>       
#>  1 cdm            demographic   cdm         demograph… NA           person      
#>  2 cdm            diagnosis     cdm         diagnosis  NA           diagnosis   
#>  3 cdm            encounter     cdm         encounter  NA           encounter   
#>  4 cdm            diagnosis     cdm         diagnosis  enc_type %i… diagnosis_o…
#>  5 cdm            diagnosis     cdm         diagnosis  enc_type %i… diagnosis_i…
#>  6 cdm            diagnosis     cdm         diagnosis  enc_type %i… diagnosis_ed
#>  7 cdm            diagnosis     cdm         diagnosis  enc_type %i… diagnosis_o…
#>  8 cdm            diagnosis     cdm         diagnosis  enc_type %i… diagnosis_o…
#>  9 cdm            diagnosis     cdm         diagnosis  enc_type %i… diagnosis_i…
#> 10 cdm            diagnosis     cdm         diagnosis  enc_type %i… diagnosis_i…
#> # ℹ 12 more rows
#> # ℹ 2 more variables: check_id <chr>, check_description <chr>

# Use this as your input to the DC function

## If you would like to retrieve counts from a previous CDM instance,
## configure the function like so:
if (FALSE) { # \dontrun{
my_dc_rslt <- check_dc(dc_tbl = ndq::dc_input_omop,
                       omop_or_pcornet = 'omop',
                       prev_db_string = 'my_previous_data',
                       current_db_string = 'my_current_data',
                       prev_ct_src = 'cdm', ## looking in previous CDM instance
                       prev_db = my_prev_db_connection,
                       check_string = 'dc')
} # }

## If you would like to reference previously executed counts from a prior
## run of check_dc, configure the function like so:
if (FALSE) { # \dontrun{
my_dc_rslt <- check_dc(dc_tbl = ndq::dc_input_omop,
                       omop_or_pcornet = 'omop',
                       prev_db_string = 'my_previous_data',
                       current_db_string = 'my_current_data',
                       prev_ct_src = 'result', ## looking in previous results
                       prev_db = my_prev_db_connection,
                       prev_rslt_tbl = 'my_dc_rslt',
                       prev_rslt_schema = 'my_previous_schema',
                       check_string = 'dc')
} # }
```
