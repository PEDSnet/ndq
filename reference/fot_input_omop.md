# Facts Over Time Input File (OMOP)

A sample version of an OMOP compliant input file for the Facts Over Time
data quality module. This exact file is also included as a CSV in the
package if the user wishes to use it, or the structure can be copied to
produce a custom list of checks.

## Usage

``` r
fot_input_omop
```

## Format

### `fot_input_omop`

A dataframe or CSV file with 6 columns

- check_id:

  A short string "code" used to identify the specific check (ex: de,
  de_rx)

- check_description:

  A longer description of the check (ex: full drug_exposure table,
  prescribed drugs based on drug_type_concept_id)

- check_domain:

  A string indicating the domain of the check (ex: visits, drugs)

- schema:

  The schema where the data is kept. Use `cdm` to use the pre-configured
  `cdm_schema`, `result` to use the preconfigured `results_schema`, or
  input the exact name of the schema.

- table:

  The name of the CDM or pre-computed results table where the relevant
  data is kept

- date_field:

  The date field to be used to identify the date associated with the
  event

- filter_logic:

  **OPTIONAL** The logic that should be applied to the provided table in
  order to tailor the tables to the desired check assessment (ex: if you
  only want to assess prescription drugs)
