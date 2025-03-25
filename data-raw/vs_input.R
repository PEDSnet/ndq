## code to prepare `vs_input_omop` dataset goes here

vs_input_omop <- readr::read_csv(paste0(system.file("extdata", package = 'ndq'), '/vs_input_omop.csv'))

usethis::use_data(vs_input_omop, overwrite = TRUE)
