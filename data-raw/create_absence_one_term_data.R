# Code to move raw data csv into a nice RDA object

absence_one_term_data <- read.csv("./data-raw/absence_one_term_national_1718.csv")

devtools::use_data(absence_one_term_data)

rm(absence_one_term_data)
