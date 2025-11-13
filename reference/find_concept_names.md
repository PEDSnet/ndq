# Find concept names for existing concept_ids

Find concept names for existing concept_ids

## Usage

``` r
find_concept_names(
  fact_tbl,
  omop_or_pcornet,
  fact_concept_id,
  concept_field,
  concept_tbl = vocabulary_tbl("concept")
)
```

## Arguments

- fact_tbl:

  the fact table associated with the field

- omop_or_pcornet:

  string indicating the CDM format of the data; defaults to `omop`

- fact_concept_id:

  the concept field in the fact table (i.e. condition_concept_id or dx)

- concept_field:

  the field in the concept table to be used in the analysis; will
  typically be concept_name, but concept_class_id is used for RxNorm
  class identifiers

- concept_tbl:

  defaults to `vocabulary_tbl('concept')`

## Value

the provided fact_tbl with additional information from the specified
concept_field
