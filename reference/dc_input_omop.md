# Data Cycle Changes Input File (OMOP)

A sample version of an OMOP compliant input file for the Data Cycle
Changes data quality module. This exact file is also included as a CSV
in the package if the user wishes to use it, or the structure can be
copied to produce a custom list of checks.

## Usage

``` r
dc_input_omop
```

## Format

### `dc_input_omop`

A dataframe or CSV file with 8 columns

- schema_current:

  The schema where the data reflecting the current data model version is
  kept. Use `cdm` to use the pre-configured `cdm_schema`, `result` to
  use the preconfigured `results_schema`, or input the exact name of the
  schema.

- table_current:

  The name of the CDM or pre-computed results table to be used to
  retrieve the counts associated with the current data model

- schema_prev:

  **ONLY REQUIRED IF PREV_CT_SRC = CDM**; The schema where the data
  reflecting the previous data model version is kept. Use `cdm` to use
  the pre-configured `cdm_schema`, `result` to use the preconfigured
  `results_schema`, or input the exact name of the schema.

- table_prev:

  **ONLY REQUIRED IF PREV_CT_SRC = CDM**; The name of the CDM or
  pre-computed results table to be used to retrieve the counts
  associated with the previous data model

- filter_logic:

  **OPTIONAL** The logic that should be applied to BOTH table_current
  and table_prev in order to tailor the tables to the desired check
  assessment

- check_domain:

  A string indicating the domain of the check (ex: drug_exposure,
  prescription_drugs)

- check_id:

  A short string "code" used to identify the specific check (ex: de,
  de_rx)

- check_description:

  A longer description of the check (ex: full drug_exposure table,
  prescribed drugs based on drug_type_concept_id)
