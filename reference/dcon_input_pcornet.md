# Domain Concordance Input File (PCORnet)

A sample version of an PCORnet compliant input file for the Domain
Concordance data quality module. This exact file is also included as a
CSV in the package if the user wishes to use it, or the structure can be
copied to produce a custom list of checks.

## Usage

``` r
dcon_input_pcornet
```

## Format

### `dcon_input_pcornet`

A dataframe or CSV file with 10 columns

- cohort_id:

  A short string describing the event in that row of the input file

- check_id:

  A short string "code" used to identify the specific check (ex:
  cancer_oncologists). There should always be **TWO rows with the SAME
  check_id**, with each row representing one of the two input cohorts.

- cohort_description:

  A longer description of the cohort (ex: cancer diagnoses, oncology
  specialist visits)

- schema:

  The schema where the data is kept. Use `cdm` to use the pre-configured
  `cdm_schema`, `result` to use the preconfigured `results_schema`, or
  input the exact name of the schema.

- table:

  The name of the CDM or pre-computed results table where the relevant
  data is kept

- date_field:

  The date field to be used when time_between_events is not null to
  determine the length of time between each event occurrence

- vocabulary_field:

  Typically only relevant for diagnoses and procedures, the field where
  the code vocabulary is kept (i.e. dx_type, px_type)

- concept_field:

  The field with the concept codes that make up the valueset (ex: dx,
  provider_specialty_primary)

- conceptset_name:

  **OPTIONAL** The string name of the concept set that will identify the
  concepts of interest, as it appears in the predefined
  file_subdirectory

- filter_logic:

  **OPTIONAL** The logic that should be applied to the provided table in
  order to tailor the tables to the desired check assessment (ex: if you
  only want to assess billed diagnoses)

- time_between_events:

  **OPTIONAL** An integer expressing the *maximum* number of days that
  should fall between the two events. Patients with events that occur
  too far apart will not be included in the combined cohort.

## Details

If you choose to use this sample file, please be sure to set your
`file_subdirectory` to
`system.file("sample_conceptsets", package = "ndq")` so the functions
know where to access the associated concept sets. You also have the
option to download the sample concept sets to a local directory and
point to that location.
