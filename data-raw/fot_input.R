## code to prepare `fot_input_omop` dataset goes here

fot_input_omop <- readr::read_csv(paste0(system.file("extdata", package = 'ndq'), '/fot_input_omop.csv'))


usethis::use_data(fot_input_omop, overwrite = TRUE)


## code to prepare `fot_input_pcornet` dataset goes here

fot_input_pcornet <- readr::read_csv(paste0(system.file("extdata", package = 'ndq'), '/fot_input_pcornet.csv'))


usethis::use_data(fot_input_pcornet, overwrite = TRUE)
