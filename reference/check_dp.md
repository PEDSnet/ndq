# Date Plausibility

This function will iterate through the provided input table to identify
the proportion of rows in each fact type that have an implausible date.
Implausibility is defined as a date that falls before the associated
visit start date, after the associated visit end date, before the
patient's birth date, or after the patient's death date + the defined
buffer period.

## Usage

``` r
check_dp(
  dp_tbl,
  omop_or_pcornet = "omop",
  visit_tbl = cdm_tbl("visit_occurrence"),
  dob_tbl = cdm_tbl("person"),
  death_tbl = cdm_tbl("death"),
  post_death_buffer = 30L,
  check_string = "dp"
)
```

## Arguments

- dp_tbl:

  *tabular input* \|\| **required**

  The primary input table that contains descriptive information about
  the checks to be executed by the function. It should include
  definitions for the clinical fact types that should be evaluated for
  date plausibility. see
  [`?dp_input_omop`](https://pedsnet.github.io/ndq/reference/dp_input_omop.md)
  or
  [`?dp_input_pcornet`](https://pedsnet.github.io/ndq/reference/dp_input_pcornet.md)
  for examples of the input structure

- omop_or_pcornet:

  *string* \|\| defaults to `omop`

  A string, either `omop` or `pcornet`, indicating the CDM format of the
  data

- visit_tbl:

  *tabular input* \|\| defaults to `cdm_tbl(visit_occurrence)`

  A table with visit information, including identifiers that can link
  back to facts and start & end dates. This argument will most likely be
  the `visit_occurrence` table for OMOP or the `encounter` table for
  PCORnet. Custom tables with specific visit subsets (i.e. a table with
  only nephrology specialist visits) can also be provided.

- dob_tbl:

  *tabular input* \|\| defaults to `cdm_tbl('person')`

  A table with patient identifiers that can link back to facts and birth
  dates. This argument will most likely be the `person` table for OMOP
  or the `demographic` table for PCORnet.

- death_tbl:

  *tabular input* \|\| defaults to `cdm_tbl('death')`

  A table with patient identifiers that can link back to facts and death
  dates. This argument will most likely be the `death` table for OMOP or
  the `death` table for PCORnet.

- post_death_buffer:

  *integer* \|\| defaults to `30`

  An integer reflecting the number of days after death where the
  function should begin counting facts as implausible. This allows the
  user to capture autopsy results or other expected post-death facts to
  occur within the plausible window.

- check_string:

  *string* \|\| defaults to `dp`

  An abbreviated identifier that will be used to label all output from
  this module

## Value

This function will return a table with the count & proportion of rows
with an implausible date value, with one column for each of the 3
implausibility definitions. These will indicate the proportion of facts
falling before the associated visit start date, after the associated
visit end date, or before the patient birth date

## Examples

``` r
# First create input file with desired checks to be executed
# You can access examples for both OMOP & PCORnet here:
ndq::dp_input_omop
#> # A tibble: 3 × 6
#>   check_id check_description schema table                date_field filter_logic
#>   <chr>    <chr>             <chr>  <chr>                <chr>      <lgl>       
#> 1 pr       procedures        cdm    procedure_occurrence procedure… NA          
#> 2 co       conditions        cdm    condition_occurrence condition… NA          
#> 3 dr       drugs             cdm    drug_exposure        drug_expo… NA          
ndq::dp_input_pcornet
#> # A tibble: 4 × 6
#>   check_id check_description         schema table       date_field  filter_logic
#>   <chr>    <chr>                     <chr>  <chr>       <chr>       <lgl>       
#> 1 pr       procedures                cdm    procedures  px_date     NA          
#> 2 co       conditions                cdm    diagnosis   admit_date  NA          
#> 3 rx       prescribed drugs          cdm    prescribing rx_start_d… NA          
#> 4 admin    inpatient administrations cdm    med_admin   med_admin_… NA          

# Use this as your input to the DP function
if (FALSE) { # \dontrun{
my_dp_rslt <- check_dp(dp_tbl = ndq::dp_input_omop,
                       omop_or_pcornet = 'omop',
                       visit_tbl = cdm_tbl('visit_occurrence'), ## table with visit/encounter dates
                       dob_tbl = cdm_tbl('person'), ## table with dates of birth
                       check_string = 'dp')
} # }
```
