# Vocabulary Conformance Input File (OMOP)

A sample version of an OMOP compliant input file for the Vocabulary
Conformance data quality module. This exact file is also included as a
CSV in the package if the user wishes to use it, or the structure can be
copied to produce a custom list of checks.

## Usage

``` r
vc_input_omop
```

## Format

### `vc_input_omop`

A dataframe or CSV file with 6 columns

- check_id:

  A short string "code" used to identify the specific check (ex: de,
  de_rx)

- schema:

  The schema where the data is kept. Use `cdm` to use the pre-configured
  `cdm_schema`, `result` to use the preconfigured `results_schema`, or
  input the exact name of the schema.

- table:

  The name of the CDM or pre-computed results table where the relevant
  data is kept

- acceptable_vocabularies:

  A comma-separated string of acceptable vocabulary values (ex: "RxNorm,
  RxNorm Extension")

- concept_field:

  The \*\_concept_id field with the concepts that should be evaluated
  (ex: drug_concept_id, drug_source_concept_id)

- filter_logic:

  **OPTIONAL** The logic that should be applied to the provided table in
  order to tailor the tables to the desired check assessment (ex: if
  prescription drugs had different vocabulary requirements than
  inpatient administrations)
