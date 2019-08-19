# Title: read_damageData.r
# By: Josh Macabuag
# Date: 19-Aug-2019
# Description:
# - Read Myanmar (MMR) Damage Observations Data from Department of Disaster Management


## 1.0 SETUP -----------------------------------------------------------
## User-Defined Values --

## Input Files --
iFile <- list()

iFile$damageData <- file.path("..","Spreadsheets","Damage Observations", "Damage Observations190819.xlsx")
iFile$gauge <- file.path("inputs","Stations+ River Water Levels.xlsx")

sheetNames <- c("10-8-2019ed", "12-8-2019ed", "14-8-2019ed", "16-8-2019ed")

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

if(!require(tidyr)) {
  install.packages("tidyr")
  library(tidyr)
}

# if(!require(dplyr)) {
#   install.packages("dplyr")
#   library(dplyr)
# }

if(!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(ggrepel)) {
  install.packages("ggrepel")
  library(ggrepel)
}

if(!require(scales)) {
  install.packages("scales")
  library(scales)
}


## 2.0 DAMAGE DATA -----------------------------------------------------------
## 2.1 READ SPREADSHEETS ----
damageData <- list()

for (i in 1:length(sheetNames)){
  a <- data.table(read_excel(path = iFile$damageData, sheet = sheetNames[i], 
                             range = "AD3:AI16", trim_ws = T))
  b <- na.omit(a)
  b[,sheet := sheetNames[i]]
  if ("row labels" %in% tolower(names(b))) names(b)[tolower(names(b))=="row labels"] <- "State"

  damageData$daily[[sheetNames[i]]] <- b
}
rm(a,b)

damageData$comb <- rbindlist(damageData$daily, use.names = TRUE, fill = T)


## 2.2 TREAT THE DATA ----
  ## Tidy Titles ##
c <- names(damageData$comb)
c <- str_remove_all(string = c, pattern = "Sum of ")
c <- str_remove_all(string = c, pattern = " ")
names(damageData$comb) <- c ; rm(c)

  ## Add date ##
d <- substr(x = damageData$comb$sheet, start = 1, nchar(damageData$comb$sheet)-2)
damageData$comb$date <- as.Date(d, format = "%d-%m-%Y") ; rm(d)

  ## Extract the 'Grand Totals' rows ##
damageData$totals <- damageData$comb[State %like% "Grand Total"]
damageData$comb <- damageData$comb[!State %like% "Grand Total"]



  ## Prepare Long Table ##
damageData$gg$state <- melt(data = damageData$comb, id.vars = c("State", "sheet", "date"),
                            variable.name = "metric", value.name = "value")
damageData$gg$totals <- melt(data = damageData$totals, id.vars = c("State", "sheet", "date"),
                             variable.name = "metric", value.name = "value")

  ## Calculate Differences Over the Time Period ##
e <- damageData$gg$state[,.(min=min(value), max=max(value)), by=.(State, metric)]
e[,diff:=max-min]
damageData$diff <- e ; rm(e)


## 2.3 PLOT TRENDS ----
damageData$plots$trends <- ggplot(data=damageData$gg$state, aes(x=date, y=value, colour=State)) +
  geom_line() +
  facet_wrap(~metric, scales = "free") +
  scale_y_continuous(labels = comma) +
  theme_classic()
damageData$plots$trends


damageData$plots$totals <- ggplot(data=damageData$gg$totals, aes(x=date, y=value)) +
  geom_line() +
#  geom_label_repel(data=damageData$diff, aes(x=max(damageData$gg$totals$date), y = max, label=State)) +
  facet_wrap(~metric, scales = "free") +
  scale_y_continuous(labels = comma) +
  theme_classic()
damageData$plots$totals


## 3.0 FLOW DATA -----------------------------------------------------------
## 3.1 READ SPREADSHEETS ----
gaugeData <- list()


gaugeData$raw <- data.table(read_excel(path = iFile$gauge, sheet = 1))

gaugeData$plots$all <- ggplot(data=gaugeData$raw) +
  geom_line(aes(x=Date1, y = `Distance to Danger Level (cm)`,
                colour=Lat, group=factor(Sr))) +
  coord_cartesian(expand = F) +
  labs(title = "Flow Gauge Data", subtitle = iFile$gauge) +
  theme_classic()
gaugeData$plots$all

