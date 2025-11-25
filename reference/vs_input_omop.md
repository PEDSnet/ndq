# Valueset Conformance Input File (OMOP)

A sample version of an OMOP compliant input file for the Valueset
Conformance data quality module. This exact file is also included as a
CSV in the package if the user wishes to use it, or the structure can be
copied to produce a custom list of checks.

## Usage

``` r
vs_input_omop
```

## Format

### `vs_input_omop`

A dataframe or CSV file with 7 columns

- check_id:

  A short string "code" used to identify the specific check (ex: de,
  de_rx)

- check_description:

  A longer description of the check

- schema:

  The schema where the data is kept. Use `cdm` to use the pre-configured
  `cdm_schema`, `result` to use the preconfigured `results_schema`, or
  input the exact name of the schema.

- table:

  The name of the CDM or pre-computed results table where the relevant
  data is kept

- valueset_name:

  The name of the CSV file (stored in the pre-defined file subdirectory)
  defining the acceptable values for the valueset

- concept_field:

  The \*\_concept_id field with the concepts that make up the valueset
  (ex: race_concept_id, ethnicity_concept_id)

- filter_logic:

  **OPTIONAL** The logic that should be applied to the provided table in
  order to tailor the tables to the desired check assessment (ex: if you
  only want to evaluate the valueset for prescription drugs)
