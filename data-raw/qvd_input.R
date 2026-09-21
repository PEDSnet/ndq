
## code to prepare `qvd_input_omop` dataset goes here

qvd_input_omop <- readr::read_csv(paste0(system.file("extdata", package = 'ndq'), '/qvd_input_omop.csv'))

usethis::use_data(qvd_input_omop, overwrite = TRUE)

## code to prepare `bmc_input_pcornet` dataset goes here

qvd_input_pcornet <- readr::read_csv(paste0(system.file("extdata", package = 'ndq'), '/qvd_input_pcornet.csv'))

usethis::use_data(qvd_input_pcornet, overwrite = TRUE)
