# Unmapped Concepts Input File (PCORnet)

A sample version of an PCORnet compliant input file for the Unmapped
Concepts data quality module. This exact file is also included as a CSV
in the package if the user wishes to use it, or the structure can be
copied to produce a custom list of checks.

## Usage

``` r
uc_input_pcornet
```

## Format

### `uc_input_pcornet`

A dataframe or CSV file with 8 columns

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

  The \*\_concept_id field with the concepts that make up the valueset
  (ex: rxnorm_cui, raw_rxnorm_cui)

- source_value_field:

  For analyses where `produce_mapped_list == TRUE`, the \*\_source_value
  field linked to the concept_field of interest to be used to produce a
  list of source values associated with unmapped concepts

- date_field:

  For analyses where `by_year == TRUE`, the date field to be used to
  retrieve the year associted with the unmapped concepts

- filter_logic:

  **OPTIONAL** The logic that should be applied to the provided table in
  order to tailor the tables to the desired check assessment (ex: if you
  only want to identify billed procedures that are unmapped)
