# R/data-raw.R

library(readxl)

pizza_prices <- read_excel("inst/extdata/raw-data/Pizza_Prices.xlsx")
exam_data    <- read_excel("inst/extdata/raw-data/Exam_Data.xlsx")
infection_treatments <- read_excel("inst/extdata/raw-data/Infection_Treatments.xlsx")

usethis::use_data(pizza_prices, exam_data, infection_treatments, overwrite = TRUE)
