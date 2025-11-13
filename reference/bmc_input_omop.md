# Best Mapped Concepts Input File (OMOP)

A sample version of an OMOP compliant input file for the Best Mapped
Concepts data quality module. This exact file is also included as a CSV
in the package if the user wishes to use it, or the structure can be
copied to produce a custom list of checks.

## Usage

``` r
bmc_input_omop
```

## Format

### `bmc_input_omop`

A dataframe or CSV file with 7 columns

- check_id:

  A short string "code" used to identify the specific check (ex: de,
  de_rx)

- check_description:

  A longer description of the check (ex: full drug_exposure table,
  prescribed drugs based on drug_type_concept_id)

- schema:

  The schema where the data is kept. Use `cdm` to use the pre-configured
  `cdm_schema`, `result` to use the preconfigured `results_schema`, or
  input the exact name of the schema.

- table:

  The name of the CDM or pre-computed results table where the relevant
  data is kept

- concept_field:

  The \*\_concept_id field with the concepts that make up the valueset
  (ex: drug_concept_id, drug_source_concept_id)

- concept_table_field:

  **OPTIONAL** The field in OMOP's vocabulary concept table that should
  be used to identify the concept_ids in the concept_field (i.e.
  concept_name, concept_class_id); only needed if concept_tbl is not
  NULL

- filter_logic:

  **OPTIONAL** The logic that should be applied to the provided table in
  order to tailor the tables to the desired check assessment (ex: if you
  only want to assess prescription drugs)
