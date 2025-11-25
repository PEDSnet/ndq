# Expected Concepts Present Input File (PCORnet)

A sample version of an PCORnet compliant input file for the Unmapped
Concepts data quality module. This exact file is also included as a CSV
in the package if the user wishes to use it, or the structure can be
copied to produce a custom list of checks.

## Usage

``` r
ecp_input_pcornet
```

## Format

### `ecp_input_pcornet`

A dataframe or CSV file with 9 columns

- check_id:

  A short string "code" used to identify the specific check (ex: de,
  de_rx)

- check_description:

  A longer description of the check

- cohort_definition:

  The definition of the specific cohort used to identify patients with
  the concept of interest

- cohort_schema:

  The schema where the cohort_table is kept. Use `cdm` to use the
  pre-configured `cdm_schema`, `result` to use the preconfigured
  `results_schema`, or input the exact name of the schema.

- cohort_table:

  The name of the CDM or pre-computed results table where the cohort has
  been stored

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

- vocabulary_field:

  Typically only relevant for diagnoses and procedures, the field where
  the code vocabulary is kept (i.e. dx_type, px_type)

- conceptset_name:

  The string name of the concept set that will identify the concepts of
  interest, as it appears in the predefined file_subdirectory

- filter_logic:

  **OPTIONAL** The logic that should be applied to the provided table in
  order to tailor the tables to the desired check assessment (ex: if you
  only want to identify billed diagnoses for hypertension)

## Details

If you choose to use this sample file, please be sure to set your
`file_subdirectory` to
`system.file("sample_conceptsets", package = "ndq")` so the functions
know where to access the associated concept sets. You also have the
option to download the sample concept sets to a local directory and
point to that location.
