# Facts Over Time

This function will compute the number of rows, patients, and
(optionally) visits associated with the fact of interest within a
specified time period. The user will supply the end points of the time
span (i.e. January 2009 - January 2024) and the time period they wish to
divide it by (i.e. month, year).

## Usage

``` r
check_fot(
  fot_tbl,
  omop_or_pcornet = "omop",
  compute_method = "loop",
  time_span = list("2009-01-01", today()),
  time_period = "month",
  lookback_interval = 1,
  check_string = "fot",
  visits_only = FALSE,
  distinct_visits = TRUE
)
```

## Arguments

- fot_tbl:

  *tabular input* \|\| **required**

  The primary input table that contains descriptive information about
  the checks to be executed by the function. It should include
  definitions for the clinical fact types that should be evaluated
  across the user-specified time span per each time period. see
  [`?fot_input_omop`](https://pedsnet.github.io/ndq/reference/fot_input_omop.md)
  or
  [`?fot_input_pcornet`](https://pedsnet.github.io/ndq/reference/fot_input_pcornet.md)
  for examples of the input structure

- omop_or_pcornet:

  *string* \|\| defaults to `omop`

  A string, either `omop` or `pcornet`, indicating the CDM format of the
  data

- compute_method:

  *string* \|\| defaults to `loop`

  A string, either `loop` or `group`, that controls whether the check is
  executed by looping through each time period or grouping by a date
  field to obtain counts. `group` is recommended for high performing
  database systems.

- time_span:

  *list (length 2)* \|\| defaults to `list('2009-01-01', today())`

  A list that contains the start date and end date of the total time
  period of interest

- time_period:

  *string* \|\| defaults to `month`

  A string indicating the length of each time period that should be
  examined, like months or years

- lookback_interval:

  *integer* \|\| defaults to `1`

  An integer indicating the number of time periods (defined in
  `time_period`) to look back in each interval. For example, a
  `time_period` of `month` and a `lookback_interval` of `3` would
  produce quarterly counts.

- check_string:

  *string* \|\| defaults to `fot`

  An abbreviated identifier that will be used to label all output from
  this module

- visits_only:

  *boolean* \|\| defaults to FALSE

  If set to `TRUE`, then this function will only return visit counts and
  will not return patient or row counts. By default, visit, row, and
  patient counts are returned.

- distinct_visits:

  *boolean* \|\| defaults to TRUE

  If set to `FALSE`, then this function will only return patient and row
  counts and will not return visit counts. By default, visit, row, and
  patient counts are returned.

## Value

This function will return a dataframe with one row for each time period
within the specified time span for each check. Additionally:

- If `visits_only = TRUE`, only visit counts will be returned for the
  check + time period

- If `visits_only = FALSE` and `distinct_visits = TRUE`, patient, row,
  and visit counts will be returned for the check + time period

- If `visits_only = FALSE` and `distinct_visits = FALSE`, only patient
  and row counts will be returned for the check + time period

## Examples

``` r
# First create input file with desired checks to be executed
# You can access examples for both OMOP & PCORnet here:
ndq::fot_input_omop
#> # A tibble: 22 × 6
#>    check_id  check_description              schema table date_field filter_logic
#>    <chr>     <chr>                          <chr>  <chr> <chr>      <chr>       
#>  1 vi        all visits                     cdm    visi… visit_sta… NA          
#>  2 vo_office outpatient visits              cdm    visi… visit_sta… visit_conce…
#>  3 vo_labs   outpatient lab visits          cdm    visi… visit_sta… visit_conce…
#>  4 vo_th     telehealth visits              cdm    visi… visit_sta… visit_conce…
#>  5 vo_oa     other ambulatory visits        cdm    visi… visit_sta… visit_conce…
#>  6 vip       inpatient visits (9201 or 200… cdm    visi… visit_sta… visit_conce…
#>  7 ved       emergency department visits    cdm    visi… visit_sta… visit_conce…
#>  8 vob       observation visits             cdm    visi… visit_sta… visit_conce…
#>  9 co        all conditions                 cdm    cond… condition… NA          
#> 10 dp        prescription drugs             cdm    drug… drug_expo… drug_type_c…
#> # ℹ 12 more rows
ndq::fot_input_pcornet
#> # A tibble: 21 × 6
#>    check_id  check_description              schema table date_field filter_logic
#>    <chr>     <chr>                          <chr>  <chr> <chr>      <chr>       
#>  1 vi        all visits                     cdm    enco… admit_date NA          
#>  2 vo_office outpatient visits              cdm    enco… admit_date enc_type ==…
#>  3 vo_th     telehealth visits              cdm    enco… admit_date enc_type ==…
#>  4 vo_oa     other ambulatory visits        cdm    enco… admit_date enc_type ==…
#>  5 vip       inpatient visits (9201 or 200… cdm    enco… admit_date enc_type %i…
#>  6 ved       emergency department visits    cdm    enco… admit_date enc_type ==…
#>  7 vob       observation visits             cdm    enco… admit_date enc_type ==…
#>  8 co        all diagnoses                  cdm    diag… admit_date NA          
#>  9 dp        prescription drugs             cdm    pres… rx_start_… NA          
#> 10 ip        inpatient administration drugs cdm    med_… medadmin_… NA          
#> # ℹ 11 more rows

# Use this as your input to the FOT function
## This check can be executed for different time period lengths, like...
### Yearly
if (FALSE) { # \dontrun{
my_fot_rslt <- check_fot(fot_tbl = ndq::fot_input_omop,
                         omop_or_pcornet = 'omop',
                         compute_method = 'loop', # use 'group' for high performant DBMSs
                         time_span = list('2015-01-01', '2025-01-01'),
                         time_period = 'year',
                         lookback_interval = 1,
                         check_string = 'fot')
} # }

### Monthly
if (FALSE) { # \dontrun{
my_fot_rslt <- check_fot(fot_tbl = ndq::fot_input_omop,
                         omop_or_pcornet = 'omop',
                         compute_method = 'loop', # use 'group' for high performant DBMSs
                         time_span = list('2015-01-01', '2025-01-01'),
                         time_period = 'month',
                         lookback_interval = 1,
                         check_string = 'fot')
} # }

### Quarterly
if (FALSE) { # \dontrun{
my_fot_rslt <- check_fot(fot_tbl = ndq::fot_input_omop,
                         omop_or_pcornet = 'omop',
                         compute_method = 'loop', # use 'group' for high performant DBMSs
                         time_span = list('2015-01-01', '2025-01-01'),
                         time_period = 'month',
                         lookback_interval = 3,
                         check_string = 'fot')
} # }
```
