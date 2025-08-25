## code to prepare `bmc_input_omop` dataset goes here

bmc_input_omop <- readr::read_csv(paste0(system.file("extdata", package = 'ndq'), '/bmc_input_omop.csv'))

usethis::use_data(bmc_input_omop, overwrite = TRUE)

## code to prepare `bmc_input_pcornet` dataset goes here

bmc_input_pcornet <- readr::read_csv(paste0(system.file("extdata", package = 'ndq'), '/bmc_input_pcornet.csv'))

usethis::use_data(bmc_input_pcornet, overwrite = TRUE)
