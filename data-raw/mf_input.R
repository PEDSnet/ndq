## code to prepare `mf_input_omop` dataset goes here

mf_input_omop <- readr::read_csv(paste0(system.file("extdata", package = 'ndq'), '/mf_input_omop.csv'))

usethis::use_data(mf_input_omop, overwrite = TRUE)

## code to prepare `mf_input_pcornet` dataset goes here

mf_input_pcornet <- readr::read_csv(paste0(system.file("extdata", package = 'ndq'), '/mf_input_pcornet.csv'))

usethis::use_data(mf_input_pcornet, overwrite = TRUE)
