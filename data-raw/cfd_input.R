## code to prepare `cfd_input_omop` dataset goes here

cfd_input_omop <- readr::read_csv(paste0(system.file("extdata", package = 'ndq'), '/cfd_input_omop.csv'))

usethis::use_data(cfd_input_omop, overwrite = TRUE)

## code to prepare `cfd_visit_types_omop` dataset goes here
cfd_visit_types_omop <- tibble('visit_type' = c('inpatient', 'inpatient',
                                               'outpatient', 'outpatient',
                                               'emergency', 'emergency'),
                              'visit_concept_id' = c(9201, 2000000048,
                                                     9202, 581399,
                                                     9203, 2000000048))

usethis::use_data(cfd_visit_types_omop, overwrite = TRUE)
