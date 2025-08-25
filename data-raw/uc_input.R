## code to prepare `uc_input_omop` dataset goes here

uc_input_omop <- readr::read_csv(paste0(system.file("extdata", package = 'ndq'), '/uc_input_omop.csv'))

usethis::use_data(uc_input_omop, overwrite = TRUE)

## code to prepare `uc_input_pcornet` dataset goes here

uc_input_pcornet <- readr::read_csv(paste0(system.file("extdata", package = 'ndq'), '/uc_input_pcornet.csv'))

usethis::use_data(uc_input_pcornet, overwrite = TRUE)
