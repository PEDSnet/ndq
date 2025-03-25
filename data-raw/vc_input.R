## code to prepare `vc_input_omop` dataset goes here

vc_input_omop <- readr::read_csv(paste0(system.file("extdata", package = 'ndq'), '/vc_input_omop.csv'))


usethis::use_data(vc_input_omop, overwrite = TRUE)
