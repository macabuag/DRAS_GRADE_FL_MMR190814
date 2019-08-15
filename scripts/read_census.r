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

if(!require(stringr)) {
  install.packages("stringr")
  library(stringr)
}

## 2.0 READ SPREADSHEETS ---------------------------------------------------

iFile$censusList <- list.files(path = iFile$censusFolder, pattern = "*_Eng*")

Census <- list()


for(iCensus in iFile$censusList) {
  # a <- data.table(read_excel(path = file.path(iFile$censusFolder, "BaselineData_Census_Ayeyarwady_with_Pcode_MIMU_05Jun2015_Eng.xlsx"),
  #                 sheet = "Table I-1", skip = 3, n_max = 200))
  iState <- word(string = iCensus, start = 3, sep = "_")
  
  if (iState %in% c("Union")) next #don't read Union (it isn't a state, but an amalgamated table of all states)
  
  a <- data.table(read_excel(path = file.path(iFile$censusFolder, iCensus),
                             sheet = "Table I-1", skip = 3, n_max = 200))

  if (!"Township Pcode" %in% names(a)) {
    a <- data.table(read_excel(path = file.path(iFile$censusFolder, iCensus),
                               sheet = "Table I-1", skip = 4, n_max = 200))
  } #sometimes the table starts from the 4th sometimes the 5th row
    
  b <- a[!is.na(`Township Pcode`)]
  if (anyDuplicated(b$`Township Pcode`) != 0) warning("'Township Pcode not unique")
  Census$state$Tab_I[[iState]] <- b
  rm(a)
}
rm(b)

c <- rbindlist(Census$state$Tab_I, use.names = T, fill = T)
if (anyDuplicated(c$`Township Pcode`) != 0) warning("'Township Pcode not unique")


Census$Combined <- c
rm(c)

## 3.0 WRITE OUTPUTS ----------------------------------------------------------------------------------
dir.create("outputs", showWarnings = F)
fwrite(Census$Combined, file = file.path("outputs", "BaselineData_Census_COMBINED_with_Pcode_MIMU_05Jun2015_Eng.csv"))

