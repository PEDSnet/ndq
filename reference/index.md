# Package index

## Data Quality Checks

The primary functions used to execute each data quality analysis

- [`check_bmc()`](https://pedsnet.github.io/ndq/reference/check_bmc.md)
  : Best Mapped Concepts
- [`check_cfd()`](https://pedsnet.github.io/ndq/reference/check_cfd.md)
  : Clinical Fact Documentation
- [`check_dc()`](https://pedsnet.github.io/ndq/reference/check_dc.md) :
  Data Cycle Changes
- [`check_dcon()`](https://pedsnet.github.io/ndq/reference/check_dcon.md)
  : Domain Concordance
- [`check_dp()`](https://pedsnet.github.io/ndq/reference/check_dp.md) :
  Date Plausibility
- [`check_ecp()`](https://pedsnet.github.io/ndq/reference/check_ecp.md)
  : Expected Concepts Present
- [`check_fot()`](https://pedsnet.github.io/ndq/reference/check_fot.md)
  : Facts Over Time
- [`check_mf_visitid()`](https://pedsnet.github.io/ndq/reference/check_mf_visitid.md)
  : Missing Field: Visit ID
- [`check_uc()`](https://pedsnet.github.io/ndq/reference/check_uc.md) :
  Unmapped Concepts
- [`check_vc()`](https://pedsnet.github.io/ndq/reference/check_vc.md) :
  Vocabulary Conformance
- [`check_vs()`](https://pedsnet.github.io/ndq/reference/check_vs.md) :
  Valueset Conformance

## Data Quality Processing

Post-processing functions to be applied to output after the primary
analyses have been executed on all network institutions.

- [`process_bmc()`](https://pedsnet.github.io/ndq/reference/process_bmc.md)
  : Best Mapped Concepts – Processing
- [`process_cfd()`](https://pedsnet.github.io/ndq/reference/process_cfd.md)
  : Clinical Fact Documentation – Processing
- [`process_dc()`](https://pedsnet.github.io/ndq/reference/process_dc.md)
  : Data Cycle Changes – Processing
- [`process_dcon()`](https://pedsnet.github.io/ndq/reference/process_dcon.md)
  : Domain Concordance – Processing
- [`process_dp()`](https://pedsnet.github.io/ndq/reference/process_dp.md)
  : Date Plausibility – Processing
- [`process_ecp()`](https://pedsnet.github.io/ndq/reference/process_ecp.md)
  : Expected Concepts Present – Processing
- [`process_fot()`](https://pedsnet.github.io/ndq/reference/process_fot.md)
  : Facts Over Time – Processing
- [`process_mf_visitid()`](https://pedsnet.github.io/ndq/reference/process_mf_visitid.md)
  : Missing Field: Visit ID – Processing
- [`process_uc()`](https://pedsnet.github.io/ndq/reference/process_uc.md)
  : Unmapped Concepts – Processing
- [`process_vc()`](https://pedsnet.github.io/ndq/reference/process_vc.md)
  : Vocabulary Conformance – Processing
- [`process_vs()`](https://pedsnet.github.io/ndq/reference/process_vs.md)
  : Valueset Conformance – Processing

## Helper Functions

Miscellaneous helper functions to supplement the analyses

- [`attach_anon_id()`](https://pedsnet.github.io/ndq/reference/attach_anon_id.md)
  : Add an anonymized site identifier to all tables
- [`create_check_metadata()`](https://pedsnet.github.io/ndq/reference/create_check_metadata.md)
  : Create informational metadata file
- [`summarize_large_n()`](https://pedsnet.github.io/ndq/reference/summarize_large_n.md)
  : Add summary columns for a large N of sites

## Example Function Inputs

Examples of all the tables that should be used as input to each of the
`check_*` functions. There are specific examples for both OMOP and
PCORnet configurations.

- [`bmc_input_omop`](https://pedsnet.github.io/ndq/reference/bmc_input_omop.md)
  : Best Mapped Concepts Input File (OMOP)
- [`bmc_input_pcornet`](https://pedsnet.github.io/ndq/reference/bmc_input_pcornet.md)
  : Best Mapped Concepts Input File (PCORnet)
- [`bmc_best_notbest`](https://pedsnet.github.io/ndq/reference/bmc_best_notbest.md)
  : Best Mapped Concepts Best/Not Best File
- [`cfd_input_omop`](https://pedsnet.github.io/ndq/reference/cfd_input_omop.md)
  : Clinical Fact Documentation Input File (OMOP)
- [`cfd_input_pcornet`](https://pedsnet.github.io/ndq/reference/cfd_input_pcornet.md)
  : Clinical Fact Documentation Input File (PCORnet)
- [`cfd_visit_types_omop`](https://pedsnet.github.io/ndq/reference/cfd_visit_types_omop.md)
  : Clinical Fact Documentation Visit Type Input File (OMOP)
- [`cfd_visit_types_pcornet`](https://pedsnet.github.io/ndq/reference/cfd_visit_types_pcornet.md)
  : Clinical Fact Documentation Visit Type Input File (PCORnet)
- [`dc_input_omop`](https://pedsnet.github.io/ndq/reference/dc_input_omop.md)
  : Data Cycle Changes Input File (OMOP)
- [`dc_input_pcornet`](https://pedsnet.github.io/ndq/reference/dc_input_pcornet.md)
  : Data Cycle Changes Input File (PCORnet)
- [`dcon_input_omop`](https://pedsnet.github.io/ndq/reference/dcon_input_omop.md)
  : Domain Concordance Input File (OMOP)
- [`dcon_input_pcornet`](https://pedsnet.github.io/ndq/reference/dcon_input_pcornet.md)
  : Domain Concordance Input File (PCORnet)
- [`dp_input_omop`](https://pedsnet.github.io/ndq/reference/dp_input_omop.md)
  : Date Plausibility Input File (OMOP)
- [`dp_input_pcornet`](https://pedsnet.github.io/ndq/reference/dp_input_pcornet.md)
  : Date Plausibility Input File (PCORnet)
- [`ecp_input_omop`](https://pedsnet.github.io/ndq/reference/ecp_input_omop.md)
  : Expected Concepts Present Input File (OMOP)
- [`ecp_input_pcornet`](https://pedsnet.github.io/ndq/reference/ecp_input_pcornet.md)
  : Expected Concepts Present Input File (PCORnet)
- [`fot_input_omop`](https://pedsnet.github.io/ndq/reference/fot_input_omop.md)
  : Facts Over Time Input File (OMOP)
- [`fot_input_pcornet`](https://pedsnet.github.io/ndq/reference/fot_input_pcornet.md)
  : Facts Over Time Input File (PCORnet)
- [`mf_input_omop`](https://pedsnet.github.io/ndq/reference/mf_input_omop.md)
  : Missing Field: Visit ID Input File (OMOP)
- [`mf_input_pcornet`](https://pedsnet.github.io/ndq/reference/mf_input_pcornet.md)
  : Missing Field: Visit ID Input File (PCORnet)
- [`uc_input_omop`](https://pedsnet.github.io/ndq/reference/uc_input_omop.md)
  : Unmapped Concepts Input File (OMOP)
- [`uc_input_pcornet`](https://pedsnet.github.io/ndq/reference/uc_input_pcornet.md)
  : Unmapped Concepts Input File (PCORnet)
- [`vc_input_omop`](https://pedsnet.github.io/ndq/reference/vc_input_omop.md)
  : Vocabulary Conformance Input File (OMOP)
- [`vc_input_pcornet`](https://pedsnet.github.io/ndq/reference/vc_input_pcornet.md)
  : Vocabulary Conformance Input File (PCORnet)
- [`vs_input_omop`](https://pedsnet.github.io/ndq/reference/vs_input_omop.md)
  : Valueset Conformance Input File (OMOP)
- [`vs_input_pcornet`](https://pedsnet.github.io/ndq/reference/vs_input_pcornet.md)
  : Valueset Conformance Input File (PCORnet)
