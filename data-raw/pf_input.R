## code to prepare `pf_input_omop` dataset goes here

pf_input_omop <- readr::read_csv(paste0(system.file("extdata", package = 'ndq'), '/pf_input_omop.csv'))

usethis::use_data(pf_input_omop, overwrite = TRUE)

## code to prepare `pf_visit_types_omop` dataset goes here
pf_visit_types_omop <- tibble('visit_type' = c('inpatient', 'inpatient',
                                               'outpatient', 'outpatient',
                                               'emergency', 'emergency'),
                              'visit_concept_id' = c(9201, 2000000048,
                                                     9202, 581399,
                                                     9203, 2000000048))

usethis::use_data(pf_visit_types_omop, overwrite = TRUE)
