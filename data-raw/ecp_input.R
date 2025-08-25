## code to prepare `ecp_input_omop` dataset goes here

ecp_input_omop <- readr::read_csv(paste0(system.file("extdata", package = 'ndq'), '/ecp_input_omop.csv'))

usethis::use_data(ecp_input_omop, overwrite = TRUE)

## code to prepare `ecp_input_pcornet` dataset goes here

ecp_input_pcornet <- readr::read_csv(paste0(system.file("extdata", package = 'ndq'), '/ecp_input_pcornet.csv'))

usethis::use_data(ecp_input_pcornet, overwrite = TRUE)
