# Missing Field: Visit ID

This function will check to see if the
`visit_occurrence_id`/`encounterid` in a given fact table also exists in
the `visit_occurrence`/`encounter` table and identify cases where these
IDs are missing entirely (NULL). There may be cases where this is
expected (for example, immunizations imported from an external registry)
but generally the `visit_occurrence_id`/`encounterid` should be
populated and exist as a primary key in the
`visit_occurrence`/`encounter` table.

## Usage

``` r
check_mf_visitid(
  mf_tbl,
  omop_or_pcornet = "omop",
  visit_tbl = cdm_tbl("visit_occurrence"),
  check_string = "mf_visitid"
)
```

## Arguments

- mf_tbl:

  *tabular input* \|\| **required**

  The primary input table that contains descriptive information about
  the checks to be executed by the function. It should include
  definitions for the clinical fact types with visit IDs that should be
  evaluated against the visit_tbl. see `?mf_visitid_input_omop` or
  `?mf_visitid_input_pcornet` for examples of the input structure

- omop_or_pcornet:

  *string* \|\| defaults to `omop`

  A string, either `omop` or `pcornet`, indicating the CDM format of the
  data

- visit_tbl:

  *tabular input* \|\| defaults to `cdm_tbl('visit_occurrence')`

  The CDM table with the visit primary key IDs. Typically, this will be
  either the OMOP `visit_occurrence` or PCORnet `encounter` table

- check_string:

  *string* \|\| defaults to `mf_visitid`

  An abbreviated identifier that will be used to label all output from
  this module

## Value

This function will return a table summarizing the total number of visits
in the fact table, the number of NULL visit IDs, and the number of
visits that do not have an associated entry in the visit table

## Examples

``` r
# First create input file with desired checks to be executed
# You can access examples for both OMOP & PCORnet here:
ndq::mf_input_omop
#> # A tibble: 4 × 5
#>   check_id check_description                 schema table           filter_logic
#>   <chr>    <chr>                             <chr>  <chr>           <chr>       
#> 1 coied    conditions excluding problem list cdm    condition_occu… !condition_…
#> 2 dr       all drugs                         cdm    drug_exposure   NA          
#> 3 dp       prescription or inpatient drugs   cdm    drug_exposure   !drug_type_…
#> 4 pr       all procedures                    cdm    procedure_occu… NA          
ndq::mf_input_pcornet
#> # A tibble: 4 × 5
#>   check_id check_description                 schema table       filter_logic
#>   <chr>    <chr>                             <chr>  <chr>       <lgl>       
#> 1 coied    conditions excluding problem list cdm    diagnosis   NA          
#> 2 admin    inpatient administrations         cdm    med_admin   NA          
#> 3 rx       prescription                      cdm    prescribing NA          
#> 4 pr       all procedures                    cdm    procedures  NA          

# Use this as your input to the MF: Visit ID function
if (FALSE) { # \dontrun{
my_mf_rslt <- check_mf(mf_tbl = ndq::mf_input_omop,
                       omop_or_pcornet = 'omop',
                       visit_tbl = cdm_tbl('visit_occurrence'), ## table with visit ID primary keys
                       check_string = 'mf_visitid')
} # }
```
