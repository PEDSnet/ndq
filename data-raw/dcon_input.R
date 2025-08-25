## code to prepare `dcon_input_omop` dataset goes here

dcon_input_omop <- readr::read_csv(paste0(system.file("extdata", package = 'ndq'), '/dcon_input_omop.csv'))


usethis::use_data(dcon_input_omop, overwrite = TRUE)

## code to prepare `dcon_input_pcornet` dataset goes here

dcon_input_pcornet <- readr::read_csv(paste0(system.file("extdata", package = 'ndq'), '/dcon_input_pcornet.csv'))


usethis::use_data(dcon_input_pcornet, overwrite = TRUE)
