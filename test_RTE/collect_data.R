# scrap and clean data
library(httr)
library(stringr)
library(plyr)
library(dplyr)
library(readxl)
options(digits = 4)

data_dir <- '/Users/hyan/DevOff/MixAPI/test_RTE/data/RTE/'

target <- 'https://eco2mix.rte-france.com/download/eco2mix/eCO2mix_RTE_En-cours-TR.zip'
dest <- str_c(data_dir, 'eco2mix_RTE_En-cours-TR.zip')

download.file(url = target, destfile = dest, mode = 'wb')
file_extracted <- unzip(dest, exdir = data_dir, list = TRUE)

library(xlsx)
real_time <- read.table(str_c(data_dir, file_extracted$Name))
