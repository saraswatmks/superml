# Regression csv data to .RData format
reg_train <- data.table::fread('data-raw/reg_train.csv')
reg_test <- data.table::fread('data-raw/reg_test.csv')

devtools::use_data(reg_train, overwrite = TRUE)
devtools::use_data(reg_test, overwrite = TRUE)

# Classification csv data to .RData format
cla_train <- data.table::fread('data-raw/cla_train.csv')
cla_test <- data.table::fread('data-raw/cla_test.csv')

devtools::use_data(cla_train, overwrite = TRUE)
devtools::use_data(cla_test, overwrite = TRUE)



