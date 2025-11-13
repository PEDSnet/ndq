# Best Mapped Concepts Input File (PCORnet)

A sample version of an PCORnet compliant input file for the Best Mapped
Concepts data quality module. This exact file is also included as a CSV
in the package if the user wishes to use it, or the structure can be
copied to produce a custom list of checks.

## Usage

``` r
bmc_input_pcornet
```

## Format

### `bmc_input_pcornet`

A dataframe or CSV file with 7 columns

- check_id:

  A short string "code" used to identify the specific check (ex:
  dr_admin, dr_rx)

- check_description:

  A longer description of the check (ex: full med_admin table, full
  prescribing table)

- schema:

  The schema where the data is kept. Use `cdm` to use the pre-configured
  `cdm_schema`, `result` to use the preconfigured `results_schema`, or
  input the exact name of the schema.

- table:

  The name of the CDM or pre-computed results table where the relevant
  data is kept

- concept_field:

  The field with the concept codes that make up the valueset (ex:
  rxnorm_cui, dx)

- concept_table_field:

  **OPTIONAL** The field in OHDSI's vocabulary concept table that should
  be used to identify the concept_ids in the concept_field (i.e.
  concept_name, concept_class_id); only needed if concept_tbl is not
  NULL

- filter_logic:

  **OPTIONAL** The logic that should be applied to the provided table in
  order to tailor the tables to the desired check assessment (ex: if you
  only want to assess billed procedures)
