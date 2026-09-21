# Quantitative Variable Distribution

This function will, for a given cohort, compute summary statistics that
describe the numerical distribution of a given quantitative variable. If
compute_trimmed is set to TRUE, the function will additionally compute a
summary for a distribution that has the top and bottom 2.5% removed, to
help visualize the results without the most extreme outliers.

## Usage

``` r
check_qvd(
  qvd_tbl,
  qvd_cohort,
  compute_trimmed = TRUE,
  omop_or_pcornet = "omop",
  check_string = "qvd"
)
```

## Arguments

- qvd_tbl:

  *tabular input* \|\| **required**

  The primary input table that contains descriptive information about
  the variables to be examined by the function. It should define each
  variable and point the system to the appropriate column with
  quantitative information.

  see
  [`?qvd_input_omop`](https://pedsnet.github.io/ndq/reference/qvd_input_omop.md)
  or
  [`?qvd_input_pcornet`](https://pedsnet.github.io/ndq/reference/qvd_input_pcornet.md)
  for examples of the input structure

- qvd_cohort:

  *tabular input* \|\| **required**

  A table defining the specific subset of patients for which variable
  information should be summarised. A more specific cohort of patients
  is recommended to make it easier to assess whether certain values are
  plausible (i.e. height or lab results)

- compute_trimmed:

  *boolean* \|\| defaults to `TRUE`

  A boolean indicating whether the variable of interest should also be
  evaluated using a trimmed distribution, which removes the top and
  bottom 2.5% to lessen the impact of the most extreme outliers on the
  summary statistic. These results will be treated as an additional
  variable with its own row of results, rather than adding new columns
  to the output.

- omop_or_pcornet:

  *string* \|\| defaults to `omop`

  A string, either `omop` or `pcornet`, indicating the CDM format of the
  data

- check_string:

  *string* \|\| defaults to `dp`

  An abbreviated identifier that will be used to label all output from
  this module

## Value

This function will return a dataframe with summary statistics that
describe the distribution for each user-defined variable. This will
include mean, median, standard deviation, Q1, Q3, min, and max. Two
measures of skew will also be returned: skewness and Pearson's skew.

## Examples

``` r
# First create input file with desired checks to be executed
# You can access examples for both OMOP & PCORnet here:
ndq::qvd_input_omop
#> # A tibble: 2 × 8
#>   check_id check_description table       schema value_field     concept_field   
#>   <chr>    <chr>             <chr>       <chr>  <chr>           <chr>           
#> 1 ht       Height            measurement cdm    value_as_number NA              
#> 2 hgb      Hemoglobin        measurement cdm    value_as_number measurement_con…
#> # ℹ 2 more variables: conceptset_name <chr>, filter_logic <chr>
ndq::qvd_input_pcornet
#> # A tibble: 2 × 9
#>   check_id check_description table         schema value_field concept_field
#>   <chr>    <chr>             <chr>         <chr>  <chr>       <chr>        
#> 1 ht       Height            vital         cdm    ht          NA           
#> 2 hgb      Hemoglobin        lab_result_cm cdm    result_num  lab_loinc    
#> # ℹ 3 more variables: vocabulary_field <lgl>, conceptset_name <chr>,
#> #   filter_logic <lgl>

# Use this as your input to the QVD function
if (FALSE) { # \dontrun{
my_qvd_rslt <- check_qvd(qvd_tbl = ndq::dp_input_omop,
                         qvd_cohort = my_cohort,
                         compute_trimmed = TRUE,
                         omop_or_pcornet = 'omop',
                         check_string = 'qvd')
} # }
```
