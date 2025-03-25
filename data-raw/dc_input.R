## code to prepare `dc_input_omop` dataset goes here

dc_input_omop <- readr::read_csv(paste0(system.file("extdata", package = 'ndq'), '/dc_input_omop.csv'))


usethis::use_data(dc_input_omop, overwrite = TRUE)
