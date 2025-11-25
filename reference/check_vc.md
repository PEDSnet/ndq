# Vocabulary Conformance

This function will use a provided table with concept to vocabulary
mappings to identify the vocabulary of each concept and determine how
many rows comply with the standard vocabularies expected for that field
and how many rows violate these expectations.

## Usage

``` r
check_vc(
  vc_tbl,
  omop_or_pcornet = "omop",
  check_string = "vc",
  concept_tbl = vocabulary_tbl("concept"),
  null_values = c(44814650L, 0L, 44814653L, 44814649L)
)
```

## Arguments

- vc_tbl:

  *tabular input* \|\| **required**

  The primary input table that contains descriptive information about
  the checks to be executed by the function. It should include
  definitions for database table fields and the expected vocabularies
  for that field. see
  [`?vc_input_omop`](https://pedsnet.github.io/ndq/reference/vc_input_omop.md)
  or
  [`?vc_input_pcornet`](https://pedsnet.github.io/ndq/reference/vc_input_pcornet.md)
  for examples of the input structure

- omop_or_pcornet:

  somop_or_pcornet *string* \|\| defaults to `omop`

  A string, either `omop` or `pcornet`, indicating the CDM format of the
  data

- check_string:

  *string* \|\| defaults to `vc`

  An abbreviated identifier that will be used to label all output from
  this module

- concept_tbl:

  *tabular input* \|\| defaults to `vocabulary_tbl('concept')`

  A vocabulary table with concept definitions (for example, the OHDSI
  concept table) that includes the vocabulary to which the concept
  belongs. This table should at a minimum have the columns:
  `concept_id`, `concept_name`, `vocabulary_id`

- null_values:

  *string / vector* \|\| defaults to 44814650, 0, 44814653, & 44814649

  A string or vector listing the concept(s) that indicate a NULL value,
  which will be excluding when assessing for the presence of
  vocabularies not accepted in the field.

## Value

This function will return a table with summary information about each
vocabulary that appears in the field, with violations marked in a T/F
field. These summaries are computed at both the row and concept levels.
This is to account for any cases where a large quantity of rows are in
violation of the acceptable vocabularies, but it is made up of only 1-2
distinct concepts.

Note that vocabularies associated with the indicated `null_values` are
ignored, so proportions may not add up to 1 as a result.

## Examples

``` r
# First create input file with desired checks to be executed
# You can access examples for both OMOP & PCORnet here:
ndq::vc_input_omop
#> # A tibble: 9 × 7
#>   check_id   check_description schema table acceptable_vocabular…¹ concept_field
#>   <chr>      <chr>             <chr>  <chr> <chr>                  <chr>        
#> 1 pr_cid     Procedures        cdm    proc… ICD10CM,CPT4,ICD9CM,I… procedure_co…
#> 2 co_cscid   Conditions (sour… cdm    cond… ICD9,ICD9CM,ICD10,ICD… condition_so…
#> 3 im_dose    Immunization, do… cdm    immu… UCUM                   imm_dose_uni…
#> 4 dt_cause_… Cause of death    cdm    death SNOMED, OMOP Extension cause_concep…
#> 5 co_cid     Conditions        cdm    cond… SNOMED,OMOP Extension  condition_co…
#> 6 dr_cid     Drugs             cdm    drug… RxNorm, RxNorm Extens… drug_concept…
#> 7 im_cid     Immunizations     cdm    immu… CVX                    immunization…
#> 8 dr_dose    Drugs, dosage un… cdm    drug… UCUM                   dose_unit_co…
#> 9 pr_pscid   Procedures (sour… cdm    proc… ICD10CM,CPT4,ICD9CM,I… procedure_so…
#> # ℹ abbreviated name: ¹​acceptable_vocabularies
#> # ℹ 1 more variable: filter_logic <lgl>
ndq::vc_input_pcornet
#> # A tibble: 7 × 7
#>   check_id   check_description schema table acceptable_vocabular…¹ concept_field
#>   <chr>      <chr>             <chr>  <chr> <chr>                  <chr>        
#> 1 pr_cid     Procedures        cdm    proc… ICD10CM,CPT4,ICD9CM,I… px           
#> 2 im_dose    Immunization, do… cdm    immu… UCUM                   vx_dose_unit 
#> 3 dt_cause_… Cause of death    cdm    death SNOMED, OMOP Extension cause_concep…
#> 4 co_cid     Conditions        cdm    diag… SNOMED,ICD9,ICD9CM,IC… dx           
#> 5 dr_cid     Drugs             cdm    pres… RxNorm, RxNorm Extens… rxnorm_cui   
#> 6 im_cid     Immunizations     cdm    immu… CVX,NDC,CPT4,HCPCS,Rx… vx_code      
#> 7 dr_dose    Drugs, dosage un… cdm    pres… UCUM                   rx_dose_orde…
#> # ℹ abbreviated name: ¹​acceptable_vocabularies
#> # ℹ 1 more variable: filter_logic <lgl>

# Use this as your input to the vc function
if (FALSE) { # \dontrun{
my_vc_rslt <- check_vc(vc_tbl = ndq::vc_input_omop,
                       omop_or_pcornet = 'omop',
                       concept_tbl = vocabulary_tbl("concept"), ## points to OHDSI concept table
                       null_values = c(44814650L,0L,
                                       44814653L,44814649L), ## ignored illegal vocabs
                       check_string = 'vc')
} # }

```
