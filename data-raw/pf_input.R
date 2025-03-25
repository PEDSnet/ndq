## code to prepare `pf_input_omop` dataset goes here

pf_input_omop <- readr::read_csv(paste0(system.file("extdata", package = 'ndq'), '/pf_input_omop.csv'))

usethis::use_data(pf_input_omop, overwrite = TRUE)
