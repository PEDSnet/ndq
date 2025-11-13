# Clinical Fact Documentation

This function will identify visits that both do and do not link to at
least one occurrence of the user-specified clinical facts. It will also
compute the counts of patients are associated with these visits.

## Usage

``` r
check_cfd(
  cfd_tbl,
  visit_type_filter,
  visit_type_tbl,
  omop_or_pcornet = "omop",
  visit_tbl = cdm_tbl("visit_occurrence"),
  check_string = "cfd"
)
```

## Arguments

- cfd_tbl:

  *tabular input* \|\| **required**

  The primary input table that contains descriptive information about
  the checks to be executed by the function. It should include
  definitions for the clinical fact types that should be evaluated
  against the visit_tbl. see
  [`?cfd_input_omop`](https://pedsnet.github.io/ndq/reference/cfd_input_omop.md)
  or
  [`?cfd_input_pcornet`](https://pedsnet.github.io/ndq/reference/cfd_input_pcornet.md)
  for examples of the input structure

- visit_type_filter:

  *string / vector* \|\| **required**

  A string or vector of strings that specifies the visit type(s) the
  function should limit to for the analysis (i.e. `inpatient`,
  `c(inpatient, outpatient)`). If `all` is included as a visit type, all
  available visit types will be pulled from the `visit_tbl` (not just
  what was defined in the `visit_type_tbl`) to capture the full array of
  visits.

- visit_type_tbl:

  *tabular input* \|\| **required**

  A table with mappings that link visit_concept_ids / enc_types to a
  broader string descriptor (i.e. inpatient). Multiple rows should be
  included if multiple IDs should be mapped to the same string label.

- omop_or_pcornet:

  *string* \|\| defaults to `omop`

  A string, either `omop` or `pcornet`, indicating the CDM format of the
  data

- visit_tbl:

  *tabular input* \|\| defaults to `cdm_tbl('visit_occurrence')`

  The CDM table with the visits to be used in the analysis. Typically,
  this will be either the OMOP `visit_occurrence` or PCORnet `encounter`
  table

- check_string:

  *string* \|\| defaults to `cfd`

  An abbreviated identifier that will be used to label all output from
  this module

## Value

A summary table that, for each provided clinical fact and visit type
combination, will include the count and proportion of visits that are
and are not linked to at least one occurrence of the fact type of
interest. It will also include the count and proportion of patients that
are associated with these visits.

## Examples

``` r
# First create input file with desired checks to be executed
# You can access examples for both OMOP & PCORnet here:
ndq::cfd_input_omop
#> # A tibble: 3 × 5
#>   check_id check_description schema table                filter_logic
#>   <chr>    <chr>             <chr>  <chr>                <lgl>       
#> 1 pr       procedures        cdm    procedure_occurrence NA          
#> 2 co       conditions        cdm    condition_occurrence NA          
#> 3 dr       drugs             cdm    drug_exposure        NA          
ndq::cfd_input_pcornet
#> # A tibble: 4 × 5
#>   check_id check_description         schema table       filter_logic
#>   <chr>    <chr>                     <chr>  <chr>       <lgl>       
#> 1 pr       procedures                cdm    procedures  NA          
#> 2 co       diagnoses                 cdm    diagnosis   NA          
#> 3 rx       prescriptions             cdm    prescribing NA          
#> 4 admin    inpatient administrations cdm    med_admin   NA          

# Next define the visit types to be examined
# You can access examples for both OMOP & PCORnet here:
ndq::cfd_visit_types_omop
#> # A tibble: 6 × 2
#>   visit_type visit_concept_id
#>   <chr>                 <dbl>
#> 1 inpatient              9201
#> 2 inpatient        2000000048
#> 3 outpatient             9202
#> 4 outpatient           581399
#> 5 emergency              9203
#> 6 emergency        2000000048
ndq::cfd_visit_types_pcornet
#> # A tibble: 6 × 2
#>   visit_type enc_type
#>   <chr>      <chr>   
#> 1 inpatient  IP      
#> 2 inpatient  EI      
#> 3 outpatient AV      
#> 4 outpatient TH      
#> 5 emergency  ED      
#> 6 emergency  EI      

# Use this as your input to the CFD function
if (FALSE) { # \dontrun{
my_cfd_rslt <- check_cfd(cfd_tbl = ndq::cfd_input_omop,
                         visit_type_tbl = ndq::cfd_visit_types_omop,
                         visit_type_filter = c('inpatient', 'outpatient'),
                         omop_or_pcornet = 'omop',
                         visit_tbl = cdm_tbl('visit_occurrence'),
                         check_string = 'cfd')
} # }
```
