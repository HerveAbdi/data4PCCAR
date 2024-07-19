## Libraries
library(usethis)
library(readxl)

## Load data
FlashExample <- read_excel("FlashExample.xlsx")
colnames(FlashExample) <- gsub(" ", "_", colnames(FlashExample))

## use this dataset
use_data(FlashExample, overwrite = TRUE)


