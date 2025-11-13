# Clinical Fact Documentation Visit Type Input File (PCORnet)

A sample version of an PCORnet compliant input file for defining visit
types for the Clinical Fact Documentation data quality module. Multiple
rows should be included where multiple concepts are associated with a
single visit type. The name of the second column will control where the
visit filtering takes place, so you can filter on other columns in the
visit_tbl if need be

## Usage

``` r
cfd_visit_types_pcornet
```

## Format

### `cfd_visit_types_pcornet`

A dataframe or CSV file with 2 columns

- visit_type:

  A string to identify the visit type of interest (ex: inpatient)

- enc_type:

  The enc_type associated with the visit type of interest (ex: 9202)
