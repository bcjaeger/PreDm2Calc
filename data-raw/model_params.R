## code to prepare `DATASET` dataset goes here

model_params <- readRDS('data-raw/model_params.rds')

usethis::use_data(model_params, internal = TRUE, overwrite = TRUE)
