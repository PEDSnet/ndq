
#' Data Cycle Changes Input File (OMOP)
#'
#' A sample version of an OMOP compliant input file for
#' the Data Cycle Changes data quality module. This exact
#' file is also included as a CSV in the package if the user wishes
#' to use it, or the structure can be copied to produce a custom
#' list of checks.
#'
#' @format ## `dc_input_omop`
#' A dataframe or CSV file with 8 columns
#' \describe{
#'   \item{schema_current}{The schema where the data reflecting the current data model version is kept. Use `cdm` to use the pre-configured `cdm_schema`, `result` to use the preconfigured `results_schema`, or input the exact name of the schema.}
#'   \item{table_current}{The name of the CDM or pre-computed results table to be used to retrieve the counts associated with the current data model}
#'   \item{schema_prev}{**ONLY REQUIRED IF PREV_CT_SRC = CDM**; The schema where the data reflecting the previous data model version is kept. Use `cdm` to use the pre-configured `cdm_schema`, `result` to use the preconfigured `results_schema`, or input the exact name of the schema.}
#'   \item{table_prev}{**ONLY REQUIRED IF PREV_CT_SRC = CDM**; The name of the CDM or pre-computed results table to be used to retrieve the counts associated with the previous data model}
#'   \item{filter_logic}{**OPTIONAL** The logic that should be applied to BOTH table_current and table_prev in order to tailor the tables to the desired check assessment}
#'   \item{check_domain}{A string indicating the domain of the check (ex: drug_exposure, prescription_drugs)}
#'   \item{check_id}{A short string "code" used to identify the specific check (ex: de, de_rx)}
#'   \item{check_description}{A longer description of the check (ex: full drug_exposure table, prescribed drugs based on drug_type_concept_id)}
#' }
#'
"dc_input_omop"


#' Vocabulary Conformance Input File (OMOP)
#'
#' A sample version of an OMOP compliant input file for
#' the Vocabulary Conformance data quality module. This exact
#' file is also included as a CSV in the package if the user wishes
#' to use it, or the structure can be copied to produce a custom
#' list of checks.
#'
#' @format ## `vc_input_omop`
#' A dataframe or CSV file with 6 columns
#' \describe{
#'   \item{check_id}{A short string "code" used to identify the specific check (ex: de, de_rx)}
#'   \item{schema}{The schema where the data is kept. Use `cdm` to use the pre-configured `cdm_schema`, `result` to use the preconfigured `results_schema`, or input the exact name of the schema.}
#'   \item{table}{The name of the CDM or pre-computed results table where the relevant data is kept}
#'   \item{acceptable_vocabularies}{A comma-separated string of acceptable vocabulary values (ex: "RxNorm, RxNorm Extension")}
#'   \item{concept_field}{The *_concept_id field with the concepts that should be evaluated (ex: drug_concept_id, drug_source_concept_id)}
#'   \item{filter_logic}{**OPTIONAL** The logic that should be applied to the provided table in order to tailor the tables to the desired check assessment (ex: if prescription drugs had different vocabulary requirements than inpatient administrations)}
#' }
#'
"vc_input_omop"


#' Valueset Conformance Input File (OMOP)
#'
#' A sample version of an OMOP compliant input file for
#' the Valueset Conformance data quality module. This exact
#' file is also included as a CSV in the package if the user wishes
#' to use it, or the structure can be copied to produce a custom
#' list of checks.
#'
#' @format ## `vs_input_omop`
#' A dataframe or CSV file with 6 columns
#' \describe{
#'   \item{check_id}{A short string "code" used to identify the specific check (ex: de, de_rx)}
#'   \item{schema}{The schema where the data is kept. Use `cdm` to use the pre-configured `cdm_schema`, `result` to use the preconfigured `results_schema`, or input the exact name of the schema.}
#'   \item{table}{The name of the CDM or pre-computed results table where the relevant data is kept}
#'   \item{valueset_name}{The name of the CSV file (stored in the pre-defined file subdirectory) defining the acceptable values for the valueset}
#'   \item{concept_field}{The *_concept_id field with the concepts that make up the valueset (ex: race_concept_id, ethnicity_concept_id)}
#'   \item{filter_logic}{**OPTIONAL** The logic that should be applied to the provided table in order to tailor the tables to the desired check assessment (ex: if you only want to evaluate the valueset for prescription drugs)}
#' }
#'
"vs_input_omop"

#' Unmapped Concepts Input File (OMOP)
#'
#' A sample version of an OMOP compliant input file for
#' the Unmapped Concepts data quality module. This exact
#' file is also included as a CSV in the package if the user wishes
#' to use it, or the structure can be copied to produce a custom
#' list of checks.
#'
#' @format ## `uc_input_omop`
#' A dataframe or CSV file with 8 columns
#' \describe{
#'   \item{check_id}{A short string "code" used to identify the specific check (ex: de, de_rx)}
#'   \item{check_description}{A longer description of the check (ex: full drug_exposure table, prescribed drugs based on drug_type_concept_id)}
#'   \item{schema}{The schema where the data is kept. Use `cdm` to use the pre-configured `cdm_schema`, `result` to use the preconfigured `results_schema`, or input the exact name of the schema.}
#'   \item{table}{The name of the CDM or pre-computed results table where the relevant data is kept}
#'   \item{concept_field}{The *_concept_id field with the concepts that make up the valueset (ex: drug_concept_id, drug_source_concept_id)}
#'   \item{source_value_field}{For analyses where `produce_mapped_list == TRUE`, the *_source_value field linked to the concept_field of interest to be used to produce a list of source values associated with unmapped concepts}
#'   \item{date_field}{For analyses where `by_year == TRUE`, the date field to be used to retrieve the year associted with the unmapped concepts}
#'   \item{filter_logic}{**OPTIONAL** The logic that should be applied to the provided table in order to tailor the tables to the desired check assessment (ex: if you only want to identify prescription drugs that are unmapped)}
#' }
#'
"uc_input_omop"


#' Expected Concepts Present Input File (OMOP)
#'
#' A sample version of an OMOP compliant input file for
#' the Unmapped Concepts data quality module. This exact
#' file is also included as a CSV in the package if the user wishes
#' to use it, or the structure can be copied to produce a custom
#' list of checks.
#'
#' If you choose to use this sample file, please be sure to
#' set your `file_subdirectory`
#' to `system.file("sample_conceptsets", package = "ndq")` so
#' the functions know where to access the associated concept sets. You
#' also have the option to download the sample concept sets to a local
#' directory and point to that location.
#'
#' @format ## `ecp_input_omop`
#' A dataframe or CSV file with 9 columns
#' \describe{
#'   \item{check_id}{A short string "code" used to identify the specific check (ex: de, de_rx)}
#'   \item{cohort_definition}{The definition of the specific cohort used to identify patients with the concept of interest}
#'   \item{cohort_schema}{The schema where the cohort_table is kept. Use `cdm` to use the pre-configured `cdm_schema`, `result` to use the preconfigured `results_schema`, or input the exact name of the schema.}
#'   \item{cohort_table}{The name of the CDM or pre-computed results table where the cohort has been stored}
#'   \item{schema}{The schema where the data is kept. Use `cdm` to use the pre-configured `cdm_schema`, `result` to use the preconfigured `results_schema`, or input the exact name of the schema.}
#'   \item{table}{The name of the CDM or pre-computed results table where the relevant data is kept}
#'   \item{concept_field}{The *_concept_id field with the concepts that make up the valueset (ex: drug_concept_id, drug_source_concept_id)}
#'   \item{conceptset_name}{The string name of the concept set that will identify the concepts of interest, as it appears in the predefined file_subdirectory}
#'   \item{filter_logic}{**OPTIONAL** The logic that should be applied to the provided table in order to tailor the tables to the desired check assessment (ex: if you only want to identify prescriptions for antihypertensives)}
#' }
#'
"ecp_input_omop"


#' Patient Facts Input File (OMOP)
#'
#' A sample version of an OMOP compliant input file for
#' the Patient Facts data quality module. This exact
#' file is also included as a CSV in the package if the user wishes
#' to use it, or the structure can be copied to produce a custom
#' list of checks.
#'
#' @format ## `pf_input_omop`
#' A dataframe or CSV file with 5 columns
#' \describe{
#'   \item{check_id}{A short string "code" used to identify the specific check (ex: de, de_rx)}
#'   \item{check_description}{A longer description of the check (ex: full drug_exposure table, prescribed drugs based on drug_type_concept_id)}
#'   \item{schema}{The schema where the data is kept. Use `cdm` to use the pre-configured `cdm_schema`, `result` to use the preconfigured `results_schema`, or input the exact name of the schema.}
#'   \item{table}{The name of the CDM or pre-computed results table where the relevant data is kept}
#'   \item{filter_logic}{**OPTIONAL** The logic that should be applied to the provided table in order to tailor the tables to the desired check assessment (ex: if you only want to assess prescription drugs)}
#' }
#'
"pf_input_omop"


#' Best Mapped Concepts Input File (OMOP)
#'
#' A sample version of an OMOP compliant input file for
#' the Best Mapped Concepts data quality module. This exact
#' file is also included as a CSV in the package if the user wishes
#' to use it, or the structure can be copied to produce a custom
#' list of checks.
#'
#' @format ## `bmc_input_omop`
#' A dataframe or CSV file with 7 columns
#' \describe{
#'   \item{check_id}{A short string "code" used to identify the specific check (ex: de, de_rx)}
#'   \item{check_description}{A longer description of the check (ex: full drug_exposure table, prescribed drugs based on drug_type_concept_id)}
#'   \item{schema}{The schema where the data is kept. Use `cdm` to use the pre-configured `cdm_schema`, `result` to use the preconfigured `results_schema`, or input the exact name of the schema.}
#'   \item{table}{The name of the CDM or pre-computed results table where the relevant data is kept}
#'   \item{concept_field}{The *_concept_id field with the concepts that make up the valueset (ex: drug_concept_id, drug_source_concept_id)}
#'   \item{concept_table_field}{The field in OMOP's vocabulary concept table that should be used to identify the concept_ids in the concept_field (i.e. concept_name, concept_class_id)}
#'   \item{filter_logic}{**OPTIONAL** The logic that should be applied to the provided table in order to tailor the tables to the desired check assessment (ex: if you only want to assess prescription drugs)}
#' }
#'
"bmc_input_omop"


#' Missing Field: Visit ID Input File (OMOP)
#'
#' A sample version of an OMOP compliant input file for
#' the Missing Field: Visit ID data quality module. This exact
#' file is also included as a CSV in the package if the user wishes
#' to use it, or the structure can be copied to produce a custom
#' list of checks.
#'
#' @format ## `mf_input_omop`
#' A dataframe or CSV file with 5 columns
#' \describe{
#'   \item{check_id}{A short string "code" used to identify the specific check (ex: de, de_rx)}
#'   \item{check_description}{A longer description of the check (ex: full drug_exposure table, prescribed drugs based on drug_type_concept_id)}
#'   \item{schema}{The schema where the data is kept. Use `cdm` to use the pre-configured `cdm_schema`, `result` to use the preconfigured `results_schema`, or input the exact name of the schema.}
#'   \item{table}{The name of the CDM or pre-computed results table where the relevant data is kept}
#'   \item{filter_logic}{**OPTIONAL** The logic that should be applied to the provided table in order to tailor the tables to the desired check assessment (ex: if you only want to assess prescription drugs)}
#' }
#'
"mf_input_omop"


#' Domain Concordance Input File (OMOP)
#'
#' A sample version of an OMOP compliant input file for
#' the Domain Concordance data quality module. This exact
#' file is also included as a CSV in the package if the user wishes
#' to use it, or the structure can be copied to produce a custom
#' list of checks.
#'
#' If you choose to use this sample file, please be sure to
#' set your `file_subdirectory`
#' to `system.file("sample_conceptsets", package = "ndq")` so
#' the functions know where to access the associated concept sets. You
#' also have the option to download the sample concept sets to a local
#' directory and point to that location.
#'
#' @format ## `dcon_input_omop`
#' A dataframe or CSV file with 10 columns
#' \describe{
#'   \item{cohort_id}{A short string describing the event in that row of the input file}
#'   \item{check_id}{A short string "code" used to identify the specific check (ex: cancer_oncologists, htn_dx_antihtn_rx). There should always be **TWO rows with the SAME check_id**, with each row representing one of the two input cohorts.}
#'   \item{check_description}{A longer description of the check (ex: cancer diagnoses and oncology specialist visits, hypertension diagnoses and antihypertensive prescriptions)}
#'   \item{schema}{The schema where the data is kept. Use `cdm` to use the pre-configured `cdm_schema`, `result` to use the preconfigured `results_schema`, or input the exact name of the schema.}
#'   \item{table}{The name of the CDM or pre-computed results table where the relevant data is kept}
#'   \item{date_field}{The date field to be used when time_between_events is not null to determine the length of time between each event occurrence}
#'   \item{concept_field}{The *_concept_id field with the concepts that make up the valueset (ex: condition_concept_id, drug_concept_id)}
#'   \item{conceptset_name}{**OPTIONAL** The string name of the concept set that will identify the concepts of interest, as it appears in the predefined file_subdirectory}
#'   \item{filter_logic}{**OPTIONAL** The logic that should be applied to the provided table in order to tailor the tables to the desired check assessment (ex: if you only want to assess prescription drugs)}
#'   \item{time_between_events}{**OPTIONAL** An integer expressing the *maximum* number of days that should fall between the two events. Patients with events that occur too far apart will not be included in the combined cohort.}
#' }
#'
"dcon_input_omop"


#' Facts Over Time Input File (OMOP)
#'
#' A sample version of an OMOP compliant input file for
#' the Facts Over Time data quality module. This exact
#' file is also included as a CSV in the package if the user wishes
#' to use it, or the structure can be copied to produce a custom
#' list of checks.
#'
#' @format ## `fot_input_omop`
#' A dataframe or CSV file with 6 columns
#' \describe{
#'   \item{check_id}{A short string "code" used to identify the specific check (ex: de, de_rx)}
#'   \item{check_description}{A longer description of the check (ex: full drug_exposure table, prescribed drugs based on drug_type_concept_id)}
#'   \item{schema}{The schema where the data is kept. Use `cdm` to use the pre-configured `cdm_schema`, `result` to use the preconfigured `results_schema`, or input the exact name of the schema.}
#'   \item{table}{The name of the CDM or pre-computed results table where the relevant data is kept}
#'   \item{date_field}{The date field to be used to identify the date associated with the event}
#'   \item{filter_logic}{**OPTIONAL** The logic that should be applied to the provided table in order to tailor the tables to the desired check assessment (ex: if you only want to assess prescription drugs)}
#' }
#'
"fot_input_omop"



