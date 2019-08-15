# Title: read_census.r
# By: Josh Macabuag
# Date: 15-Aug-2019
# Description:
# - Read Myanmar (MMR) census data


## 1.0 SETUP -----------------------------------------------------------
## User-Defined Values --

## Input Files --
iFile <- list()

iFile$censusFolder <- file.path("..","..","1-data", "Exposure", "Census Data")

## Packages --
if(!require(data.table)) {
  install.packages("data.table")
  library(data.table)
}

if(!require(readxl)) {
  install.packages("readxl")
  library(readxl)
}

## 2.0 READ SPREADSHEETS ---------------------------------------------------

a <- data.table(read_excel(path = file.path(iFile$censusFolder, "BaselineData_Census_Ayeyarwady_with_Pcode_MIMU_05Jun2015_Eng.xlsx"),
                sheet = "Table I-1", skip = 3, n_max = 100))
b <- a[!is.na(`Township Pcode`)]
if (anyDuplicated(b$`Township Pcode`) != 0) warning("'Township Pcode not unique")
