## code to prepare `dp_input_omop` dataset goes here

dp_input_omop <- readr::read_csv(paste0(system.file("extdata", package = 'ndq'), '/dp_input_omop.csv'))

usethis::use_data(dp_input_omop, overwrite = TRUE)
