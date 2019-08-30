# Title: read_damageData.r
# By: Josh Macabuag
# Date: 19-Aug-2019
# Description:
# - Read Myanmar (MMR) Damage Observations Data from Department of Disaster Management


## 1.0 SETUP -----------------------------------------------------------
## User-Defined Values --
UCC <- list()
UCC$rural <- 75 #$USD/m^2. Tony: $57 in 2015 PDNA, increased to reflect GDP growth
UCC$urban <- 270

PPD <- 4.2 #Tony: 4.6 in 2014 census, reduced for population trends

GDP <- list()
GDP$`2017` <- 69.32e9 #$bn

popCorrection <- 54.29/51.48

houseSize <- list()
houseSize$rural <- 45 #m^2. Tony: 2015 PDNA
houseSize$urban <- 60

MDR <- list()
MDR$bldgs$general <- 0.08
MDR$bldgs$manual$State <- c("Mon", "Mandalay")
MDR$bldgs$manual$MDR <- 0.14

MDR$agri <- 0.4

MDR$other$general <- 0.07
MDR$other$manual$MDR <- 0.11


MDR$stilts$rural <- 0 #% rural buildings unaffected because on stilts
MDR$stilts$urban <- 0



## Input Files --
iFile <- list()

iFile$damageData <- file.path("..","Spreadsheets","Damage Observations", "Damage Observations190819.xlsx")
iFile$gauge <- file.path("inputs","Stations+ River Water Levels.xlsx")

sheetNames <- c("10-8-2019ed", "12-8-2019ed", "14-8-2019ed", "16-8-2019ed", 
                "19-8-2019ed", "20-8-2019ed", "21-08-2019ed", "22-8-2019ed", "23-8-2019ed")

resultsRange <- list()
resultsRange$ST <- "AD3:AI18"
#resultsRange$TS <- "AR6:AY71"
resultsRange$TS <- "BH1:BO65"

latestSheet <- list()
latestSheet$ST <- sheetNames[length(sheetNames)] #change this if you want to consider a date other than the last specified in sheetNames
latestSheet$TS <- "14-8-2019ed"

iFile$`Myanmar PCodes` <- file.path("..","..","1-data","Exposure","Myanmar PCodes Release-VIII.i_Sep2017_Countrywide.xlsx")

#iFile$GAD_MIMU_Census_CatDat <- file.path("..","Spreadsheets", "Township Exp Loss Data", "GAD_MIMU_Census_CATDAT_townshipData_calc.xlsx")
#iFile$GAD_MIMU_Census_CatDat <- file.path("..","Spreadsheets", "Township Exp Loss Data", "GAD_MIMU_Census_CATDAT_townshipData_calcv3.xlsx")
#iFile$GAD_MIMU_Census_CatDat <- file.path("..","Spreadsheets", "Township Exp Loss Data", "GAD_MIMU_Census_CATDAT_townshipData_calcv3_x.xlsx")
#iFile$GAD_MIMU_Census_CatDat <- file.path("..","Spreadsheets", "Township Exp Loss Data", "GAD_MIMU_Census_CATDAT_townshipData_calcv4_x.xlsx")
iFile$GAD_MIMU_Census_CatDat <- file.path("..","Spreadsheets", "Township Exp Loss Data", "GAD_MIMU_Census_CATDAT_townshipData_calcv5_x.xlsx")


iFile$censusFolder <- file.path("..","..","1-data", "Exposure", "Census Data")


## GIS ##
iFile$GIS$inp$admn3_mimu$dsn <- file.path("..","..","1-data","Exposure", "Admin Boundaries")
iFile$GIS$inp$admn3_mimu$layer <- "mmr_polbnda_adm3_250k_mimu"

iFile$GIS$inp$adm2_mimu <- file.path("..","..","1-data","Exposure", 
                                     "Admin Boundaries", "mmr_polbnda_adm2_250k_mimu")

iFile$GIS$inp$adm1_mimu <- file.path("..","..","1-data","Exposure", 
                                     "Admin Boundaries", "mmr_polbnda2_adm1_250k_mimu")

iFile$GIS$inp$SEADRIF_folder <- file.path(file.path("..","..","1-data","Hazard","SEADRIF"))


oFolder <- paste0(gsub("[[:punct:]]", " ", Sys.time()), "-readDamDat")  #create a unique folder with the run date


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

if(!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

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


## GIS ##
if(!require(sf)) {
  install.packages("sf")
  library(sf)
}

if(!require(raster)) {
  install.packages("raster")
  library(raster)
}

if(!require(leaflet)) {
  install.packages("leaflet")
  library(leaflet)
}

if(!require(leaflet.minicharts)) {
  install.packages("leaflet.minicharts")
  library(leaflet.minicharts)
}

if(!require(htmlwidgets)) {
  install.packages("htmlwidgets")
  library(htmlwidgets)
}

## Plot Functions ##
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


## 2.0 DAMAGE DATA -----------------------------------------------------------
## 2.1 READ SPREADSHEETS ----

Myanmar_PCodes <- list()
Myanmar_PCodes$States <- data.table(read_excel(iFile$`Myanmar PCodes`, sheet = "_01_States"))
Myanmar_PCodes$Townships <- data.table(read_excel(iFile$`Myanmar PCodes`, sheet = "_03_Townships"))

tidy_DamDat <- function(damDat) {
  ## Check State names ##
  if ("row labels" %in% tolower(names(damDat))) names(damDat)[tolower(names(damDat))=="row labels"] <- "State"
  damDat[State=="Karen", State:="Kayin"]
  damDat[State=="Rakkhine", State:="Rakhine"]
  damDat[State=="Tanintari", State:="Tanintharyi"]
  damDat[State=="Ayeyarwaddy", State:="Ayeyarwady"]
  
  if(!any(damDat$State %in% Myanmar_PCodes$States$State_Region)) warning(paste0("States present in ", iFile$damageData, " which are not in ", iFile$`Myanmar PCodes`, "! Please check that state names have been spelled correctly."))
  
  ## Tidy Titles ##
  c <- names(damDat)
  c <- str_remove_all(string = c, pattern = "Sum of ")
  c <- str_remove_all(string = c, pattern = " ")
  names(damDat) <- c ; rm(c)
  
  return(damDat)
} #check heading names and State names



damageData <- list()

for (i in 1:length(sheetNames)){
  a <- data.table(read_excel(path = iFile$damageData, sheet = sheetNames[i], 
                             range = resultsRange$ST, trim_ws = T))
  b <- na.omit(a)
  b[,sheet := sheetNames[i]]

  damageData$daily[[sheetNames[i]]] <- tidy_DamDat(b) #check heading names and State names
}
rm(a,b)

damageData$comb <- rbindlist(damageData$daily, use.names = TRUE, fill = T)


## 2.2 TREAT THE DATA ----
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
damageData$plots$trends <- ggplot(data=damageData$gg$state[metric=="Tot_ppl"], aes(x=date, y=value, colour=State)) +
  geom_line() +
  facet_wrap(~metric, scales = "free") +
  scale_y_continuous(labels = comma) +
  theme_classic()
#damageData$plots$trends

damageData$plots$totals <- ggplot(data=damageData$gg$totals, aes(x=date, y=value)) +
  geom_line() +
  facet_wrap(~metric, scales = "free") +
  scale_y_continuous(labels = comma) +
  theme_classic()
#damageData$plots$totals


damageData$plots$base <-   ggplot() +
  scale_x_date(breaks = as.Date(c("2019/8/10", "2019/8/14", "2019/8/19", "2019/8/23")),
               labels = date_format("%d %b"),
               name = "Date of DDM Report", expand = expand_scale(mult = c(0, 0.05))) +
  scale_y_continuous(labels = comma, name = "Number of Affected People") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        legend.box.background = element_rect(colour = "black"))

damageData$plots$totals_ppl <- damageData$plots$base +
  geom_line(data=damageData$gg$totals[metric=="Tot_ppl"],
            aes(x=date, y=value)) +
  labs(title = "Affected People (National)", subtitle = "Figures reported by the DDM")
damageData$plots$totals_ppl


impactedStates <- c("Mon", "Bago", "Mandalay", "Kachin", "Sagaing", "Kayin", "Tanintharyi")
a <- damageData$gg$state[State %in% impactedStates,
                         .(State, date, metric, value)]
b <- damageData$gg$state[!State %in% impactedStates &
                           !State %in% c("Bago (East)", "Bago (West)"),
                         .(State="Other States", value=sum(value)), by=.(date, metric)]
c <- rbindlist(l = list(a,b), use.names = T)
c$State <- factor(c$State, 
                  levels = c("Mon", "Bago", "Mandalay", "Kachin",
                             "Sagaing", "Kayin", "Tanintharyi", "Other States"))

damageData$plots$trends_ppl <- damageData$plots$base +
  geom_line(data=c[metric=="Tot_ppl"],
            aes(x=date, y=value, colour=State))  +
  labs(title = "Affected People (by State)", subtitle = "Figures reported by the DDM") +
  theme(axis.title.y = element_blank())
damageData$plots$trends_ppl

rm(a,b,c)

## 3.0 FLOW DATA -----------------------------------------------------------
## 3.1 READ SPREADSHEETS ----
gaugeData <- list()
gaugeData$long <- data.table(read_excel(path = iFile$gauge, sheet = 1))
gaugeData$wide <- data.table(read_excel(path = iFile$gauge, sheet = "Sheet2"))

a <- gaugeData$wide
a[,`Distance to Danger Level (cm)`:=NULL]
b <- gather(data = a, key = Date, value = `Distance to Danger Level (cm)`, names(a)[12:length(names(a))])
b$Date <- as.Date(as.integer(b$Date), 
                  origin = "1899-12-30") #https://www.r-bloggers.com/date-formats-in-r/

gaugeData$long <- b
rm(a,b)

## 3.2 ADD STATE NAME TO GAUGE POINTS ----
## read in shapefiles ##
GIS <- list()
GIS$inp$adm1_mimu <- st_read(dsn=iFile$GIS$inp$adm1_mimu)

## plot gauge locations ##
GIS$m$gaugePoints <- leaflet(data = ) %>%
  addTiles() %>%
  addPolygons(data=GIS$inp$adm1_mimu) %>%
  addCircles(data = gaugeData$long, lng = ~Lon, lat = ~Lat)
#GIS$m$gaugePoints

## identify which stations in which admin 1 (spatial join on state) ##
b <- st_as_sf(x = gaugeData$long, coords = c("Lon", "Lat"),  #convert data.table with lat-long to spatial object
              crs=st_crs(GIS$inp$adm1_mimu))                #obtain the CRS of the admin layer to set other layers as the same
nrow(gaugeData$long)
nrow(b)
#st_is(b, "POINT")
#View(b)

c <- st_join(x = b, y = GIS$inp$adm1_mimu, left=TRUE)       #
st_is(c, "POINT")
nrow(c)
#View(c)
gaugeData$shp <- c
gaugeData$shp$Lat <- st_coordinates(gaugeData$shp)[,2]
gaugeData$shp$Lon <- st_coordinates(gaugeData$shp)[,1]
gaugeData$long <- data.table(gaugeData$shp)

rm(b,c)


## 3.3 PLOT ----


gaugeData$plots$all <- ggplot(data=gaugeData$long[!is.na(ST)]) +
  geom_line(aes(x=Date, y = `Distance to Danger Level (cm)`,
                colour=Lat, group=factor(StationName))) +
  geom_hline(yintercept = 0, linetype="dashed", alpha=0.2) +
  coord_cartesian(expand = F) +
  scale_x_date(breaks = as.Date(c("2019/7/5", "2019/7/19", "2019/8/2", "2019/8/16")),
               labels = date_format("%d %b")) +
  labs(title = "Flow Gauge Data", 
       subtitle = "Data from Myanmar’s Department of Meteorology and Hydrology") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        legend.background = element_rect(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
#gaugeData$plots$all

gaugeData$plots$states <- gaugeData$plots$all +
  facet_wrap(.~ST)
#gaugeData$plots$states

jpeg(filename = "flowGauges.jpeg", width = 7, height = 6.5, units = "in", res = 300)
  gaugeData$plots$states
dev.off()

## 4.0 LOSSES ----------------------------------------------



## 4.1 FACTOR DAMAGE DATA ----
## Calc Factor 14th - 19th ##
  #join state-level data from 14th & 19th
c <- merge(x = damageData$daily[[latestSheet$ST]], y = damageData$daily[[latestSheet$TS]], by="State", all.x=T, all.y=T) #Naypyidaw doesn't appear

  #divide 19th / 14th to get factor
d <- c[,.(State,
          Total_affected_houses = Total_affected_houses.x/Total_affected_houses.y,
          Tot_household = Tot_household.x/Tot_household.y,
          Tot_ppl = Tot_ppl.x/Tot_ppl.y,
          Materials_for_house_school_kyat = Materials_for_house_school_kyat.x/Materials_for_house_school_kyat.y,
          Total_kyat = Total_kyat.x/Total_kyat.y)]

  #replace NAs, NANs, & INFs
e <- !do.call(cbind, lapply(d, is.finite)) #https://stackoverflow.com/questions/12188509/cleaning-inf-values-from-an-r-dataframe
d[e] <- 1
d$State <- c$State

damageData$stateFactors$`19_14` <- d
rm(c,d,e)


## Read 14th TS-level data ##
a <- read_excel(iFile$damageData, sheet = latestSheet$TS, range = resultsRange$TS) #65 affected townships (reported on 14th)
b <- tidy_DamDat(data.table(a)) #check heading names and State names


## Estimate 19th TS-level data ##
  #add the factors by state
c <- merge(x = b, y = damageData$stateFactors$`19_14`, by="State", all.x=T, all.y=F) #Naypyidaw doesn't appear

  #multiply TS-level observations by 
d <- c[,.(TS_Pcode, TS_Name = Corrent_Name,  
          State,
          Total_affected_houses = Total_affected_houses.x*Total_affected_houses.y,
          Tot_household = Tot_household.x*Tot_household.y,
          Tot_ppl = Tot_ppl.x*Tot_ppl.y,
          Materials_for_house_school_kyat = Materials_for_house_school_kyat.x*Materials_for_house_school_kyat.y,
          Total_kyat = Total_kyat.x*Total_kyat.y)]

  ##MANUAL: Add Nay Pyi Taw township##
x <- cbind(data.table(TS_Pcode="MMR018007", 
                        TS_Name="Lewe"),damageData$daily$`21-08-2019ed`[State %like% "Naypyidaw"])
x$sheet <- NULL
#y <- rbindlist(d,x, use.names = T, fill = T)
y <- rbind(d,x)


  #check that the sums add up
e <- colSums(y[,`Total_affected_houses`:`Total_kyat`], na.rm = T)
f <- colSums(damageData$daily[[latestSheet$ST]][State!="Grand Total",`Total_affected_houses`:`Total_kyat`], na.rm = T)
if (abs(sum(e-f))<1) {
  cat(paste0("CHECK OK: factored TS-level totals and original state-level totals match."))
} else {
  warning("CHECK NG: factored TS-level totals and original state-level totals DO NOT match.\nCompare the following:")
  print(damageData$daily[[latestSheet$ST]][,State:Total_kyat])
  print(y[,.(Total_affected_houses=sum(Total_affected_houses),
             Tot_ppl=sum(Tot_ppl)), by=State])
  #colSums(damageData$daily$`14-8-2019ed`[State!="Grand Total",`Total_affected_houses`:`Total_kyat`], na.rm = T)
}


## Add Township name ##
damageData$daily$latest_factored_TS <- merge(Myanmar_PCodes$Townships[,.(TS_Pcode, Township, District)], 
                                             y,
                                             by="TS_Pcode")
damageData$daily$latest_factored_TS$date <- as.Date(substr(latestSheet$ST, start = 1, stop = nchar(latestSheet$ST)-2), 
                                                    format = "%d-%m-%Y")

rm(a,b,c,d,e,f,g, x,y)


## 4.2 READ CENSUS DATA ----
  ## Read in MIMU_GAD ##
TS_census <- list()
a <- read_excel(iFile$GAD_MIMU_Census_CatDat, sheet = "Join_MMR")
if ("State/Region Pcode" %in% names(a)) {
  TS_census$MIMU$raw <- a ; rm(a)
} else {
  TS_census$MIMU$raw <- read_excel(iFile$GAD_MIMU_Census_CatDat, sheet = "Join_MMR", skip = 1)
}

b <- data.table(TS_census$MIMU$raw)

TS_census$MIMU$cropped <- cbind(b[,1:15],
                                b[,.(`Total Pop Both sexes- All`,
                                     `Pop Conventional HH Both sexes- All`,
                                     `Pop in Institutions Both sexes- All`,
                                     `Pop in Conventional households`,
                                     `Pop in Institutions`,
                                     `Pop Urban Both sexes- All`,
                                     `Pop Rural Both sexes- All`,
                                     `Urban Population %-`,
                                     `Conventional HH Total Number (n)-`,
                                     `% of Population in Institutions`,
                                     `Mean household size`,
                                     `Population/Housing Units`,
                                     `Population Density- All`,
                                     `Land Area Km2 - MIMU-`,
                                     `Disasters Impacted by Nargis 2008`,
                                     `Disasters Impacted by Giri 2010`,
                                     `Disasters Impacted by Pakkoku Floods 2011`,
                                     `Disasters Impacted by Seasonal Floods 2012`,
                                     `Disasters Impacted by Seasonal Floods 2013`,
                                     `Disasters Impacted by Floods 2015`)])

  ## Check Column meanings ##
sum(TS_census$MIMU$cropped[,`Total Pop Both sexes- All`-
                         (`Pop Conventional HH Both sexes- All`+`Pop in Institutions Both sexes- All`)], na.rm = T)
sum(TS_census$MIMU$cropped[,`Pop Conventional HH Both sexes- All`-`Pop in Conventional households`], na.rm = T)
sum(TS_census$MIMU$cropped[,`Pop in Institutions Both sexes- All`-`Pop in Institutions`], na.rm = T)
sum(TS_census$MIMU$cropped[,`Mean household size`-`Population/Housing Units`], na.rm = T)
sum(TS_census$MIMU$cropped[,(`Pop Urban Both sexes- All`/(`Pop Urban Both sexes- All` + `Pop Rural Both sexes- All`))
                           -`Urban Population %-`], na.rm = T)

  ## READ BaselineData_Census SPREADSHEETS ----
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
  TS_census$baseline$state$Tab_I[[iState]] <- b
  rm(a)
}

c <- rbindlist(TS_census$baseline$state$Tab_I, use.names = T, fill = T)
if (anyDuplicated(c$`Township Pcode`) != 0) warning("'Township Pcode not unique")

TS_census$baseline$Combined <- c
rm(b,c)

  ## COMPARE BASELINE_CENSUS & MIMU ----
nrow(TS_census$baseline$Combined) ; length(unique(TS_census$baseline$Combined$`Township Pcode`))
nrow(TS_census$MIMU$cropped) ; length(unique(TS_census$MIMU$cropped$`Township Pcode`))

#check totals
TS_census$MIMU$sums <- colSums(TS_census$MIMU$cropped[,`Number of Village Tracts`:`Population Density- All`], na.rm = T)
if (TS_census$MIMU$sums["Conventional HH Total Number (n)-"] - sum(TS_census$baseline$Combined$Total) > 1) {
  cat(paste0("CHECK OK: #households in '", iFile$GAD_MIMU_Census_CatDat, "' matches the sum of the total households in '", iFile$censusFolder, "'"))
} else {
  warning(paste0("CHECK NG! #households in '", iFile$GAD_MIMU_Census_CatDat, "' does NOT matches the sum of the total households in '", iFile$censusFolder, "'"))
}

## 4.3 CALC LOSSES ----
  ## COMBINE CENSUS WITH DAMAGE OBSERVATIONS --
a <- merge(damageData$daily$latest_factored_TS, TS_census$MIMU$cropped,
           by.x="TS_Pcode", by.y="Township Pcode",
           all.x=T, all.y=T)

  ## MANUAL: Add Updated Ppl count ##
e <- data.table(read_excel(iFile$GAD_MIMU_Census_CatDat, sheet = "TS_JD"))

e[,.(`Old Hshlds`=sum(`Old Hshlds`), `Old Ppl`=sum(`Old Ppl`), 
     `New Hshlds`=sum(`New Hshlds`), `New PPl`=sum(`New PPl`),
     `Max Hshlds`=sum(`Max Hshlds`), `Max Ppl`=sum(`Max Ppl`)), 
  by=`State/Region Name`]

a <- merge(a,
           e[,.(`Township Pcode`, Tot_ppl_manual=`Max Ppl`, Tot_HH_manual=`Max Hshlds`)],
           by.x="TS_Pcode",
           by.y="Township Pcode")


  ## Calculate Ratios ##
a[,affected_tot_HH := Tot_household/`Conventional HH Total Number (n)-`] #ratio of reported affected HH / total HH (2014 census) per township

a[,affected_tot := Tot_ppl_manual/(`Total Pop Both sexes- All`*popCorrection)] #ratio of reported affected HH / total HH (2014 census) per township


a[,affectedDwellings_PPD := Tot_ppl_manual/PPD]
a[,affectedDwellings_census := Tot_ppl_manual/`Mean household size`]
a[,HH_DwellPPD_ratio := Tot_household/affectedDwellings_PPD]
a[,HH_DwellCensus_ratio := Tot_household/affectedDwellings_census] #tests whether the PPD implied by the damage data matches the PPD in the census

#affected (from census & UCC)
#a[,affectedHousingStock_UCC := Tot_household * UCC * houseSize]
a[,resiStock_UCC := (`Conventional HH Total Number (n)-` + (`Pop in Institutions`/`Mean household size`)) *  #total households (conventional + institutions)
    ((`Urban Population %-` * UCC$urban * houseSize$urban) +        #( (%urban x UCC$urban x houseSize$urban) +
      ((1-`Urban Population %-`) * UCC$rural * houseSize$rural))]   #(%rural x ...) )

a[,affectedResiStock_UCC := Tot_HH_manual *                                        #reported affected households x
    ((`Urban Population %-` * UCC$urban * houseSize$urban*(1-MDR$stilts$urban)) +        #( (%urban x UCC$urban) +
       ((1-`Urban Population %-`) * UCC$rural * houseSize$rural*(1-MDR$stilts$rural)))]   #(%urban x UCC$urban) + (%rural x UCC$rural) x

a[,affectedGDP1 := affected_tot*`GDP Est 1 via State`]
a[,affectedGDP2 := affected_tot*`GDP Est 2 via Region old stat`]

#affected (from CATDAT)
a[,affectedResiCapstock := affected_tot*`Residential Cap Stock`]
a[,affectedOtherCapstock := affected_tot*`Other Capital Stock (Non-Bldg and Infra)`]
a[,affectedAgriStock := affected_tot*`Agriculture Stock (for Calculation)`]

#damages
a[,damagesResiCapstock_UCC := affectedResiStock_UCC * MDR$bldgs$general]
a[State %in% MDR$bldgs$manual$State,
  damagesResiCapstock_UCC := affectedResiStock_UCC * MDR$bldgs$manual$MDR]

a[,damagesResiCapstock := affectedResiCapstock*MDR$bldgs$general]
a[State %in% MDR$bldgs$manual$State,
  damagesResiCapstock := affectedResiCapstock*MDR$bldgs$manual$MDR]

a[,damagesResi_mean := (damagesResiCapstock_UCC + damagesResiCapstock)/2]


a[,damagesAgriStock := affectedAgriStock*MDR$agri]

a[,damagesOtherCapstock := affectedOtherCapstock*MDR$other$general]
a[State %in% MDR$bldgs$manual$State,
  damagesOtherCapstock := affectedOtherCapstock*MDR$other$manual$MDR]

a[,damagesTotal := damagesResi_mean + damagesAgriStock + damagesOtherCapstock]


## Sums ##
b <- colSums(a[,select_if(a, is.numeric)], na.rm = T) #sum all the numeric columns

b[["TotalHH_conv_instit"]] <- sum(a$`Conventional HH Total Number (n)-`) + 
  sum(a$`Pop in Institutions`/mean(a$`Mean household size`))

b[["resiStock_GDP"]] <- b[["Residential Cap Stock"]]/GDP$`2017` #capStock/GDP (CATDAT)
b[["resiStock_UCC_GDP"]] <- b[["resiStock_UCC"]]/GDP$`2017`     #capStock/GDP (census & UCC)

  ## state aggregation ###
c <- a[,.(resiStock_UCC = sum(resiStock_UCC, na.rm = T),
          `Residential Cap Stock` = sum(`Residential Cap Stock`, na.rm = T),
          totalPop_2014 = sum(`Total Pop Both sexes- All`, na.rm=T),
          reportedAffectedPpl = sum(Tot_ppl, na.rm = T),
          reportedAffectedPpl_manual = sum(Tot_ppl_manual, na.rm = T),
          reportedAffectedHH = sum(Tot_household, na.rm = T),
          affectedResiCapstock = sum(affectedResiCapstock, na.rm = T),
          affectedResiStock_UCC = sum(affectedResiStock_UCC, na.rm = T),
          affectedGDP1 = sum(affectedGDP1, na.rm = T),
          affectedGDP2 = sum(affectedGDP2, na.rm = T),
          affectedAgriStock = sum(affectedAgriStock, na.rm = T),
          affectedOtherCapstock = sum(affectedOtherCapstock, na.rm = T),
          damagesResiCapstock_UCC = sum(damagesResiCapstock_UCC, na.rm = T),
          damagesResiCapstock = sum(damagesResiCapstock, na.rm = T),
          damagesResi_mean = sum(damagesResi_mean, na.rm = T),
          damagesAgriStock = sum(damagesAgriStock, na.rm = T),
          damagesOtherCapstock = sum(damagesOtherCapstock, na.rm = T),
          damagesTotal = sum(damagesTotal, na.rm = T)),
       by=.(State=`State/Region Name`, ST_Pcode=`State/Region Pcode`)]

c[, affected_tot := reportedAffectedPpl/(totalPop_2014*popCorrection)]
c[, affected_tot_manual := reportedAffectedPpl_manual/(totalPop_2014*popCorrection)]


## District aggregation ###
d <- a[,.(resiStock_UCC = sum(resiStock_UCC, na.rm = T),
          `Residential Cap Stock` = sum(`Residential Cap Stock`, na.rm = T),
          totalPop_2014 = sum(`Total Pop Both sexes- All`, na.rm=T),
          reportedAffectedPpl = sum(Tot_ppl, na.rm = T),
          reportedAffectedPpl_manual = sum(Tot_ppl_manual, na.rm = T),
          reportedAffectedHH = sum(Tot_household, na.rm = T),
          affectedResiCapstock = sum(affectedResiCapstock, na.rm = T),
          affectedResiStock_UCC = sum(affectedResiStock_UCC, na.rm = T),
          affectedGDP1 = sum(affectedGDP1, na.rm = T),
          affectedGDP2 = sum(affectedGDP2, na.rm = T),
          affectedAgriStock = sum(affectedAgriStock, na.rm = T),
          affectedOtherCapstock = sum(affectedOtherCapstock, na.rm = T),
          damagesResiCapstock_UCC = sum(damagesResiCapstock_UCC, na.rm = T),
          damagesResiCapstock = sum(damagesResiCapstock, na.rm = T),
          damagesResi_mean = sum(damagesResi_mean, na.rm = T),
          damagesAgriStock = sum(damagesAgriStock, na.rm = T),
          damagesOtherCapstock = sum(damagesOtherCapstock, na.rm = T),
          damagesTotal = sum(damagesTotal, na.rm = T)),
       by=.(District=`District Name`, DT_PCODE=`District Pcode`)]

d[, affected_tot := reportedAffectedPpl/(totalPop_2014*popCorrection)]
d[, affected_tot_manual := reportedAffectedPpl_manual/(totalPop_2014*popCorrection)]


LossCalc <- list()
LossCalc$results$TS <- a
LossCalc$results$ST <- c
LossCalc$results$DT <- d
LossCalc$sums <- b
rm(a,b,c,d)



## CHECK SUMS ##
#write.csv(damageData$totals[sheet==latestSheet$ST], file = "damageData_totals.csv")
#rbindlist(data.table(a), data.table(b), use.names = T, fill = F)


## Add Agri Manual losses ##
a <- read_excel(iFile$GAD_MIMU_Census_CatDat, sheet = "R_import", skip = 6, trim_ws = T)

b <- merge(LossCalc$results$ST, 
           data.table(a)[,.(ST_Pcode, damagesAgriManual)],
           all.x=T, by="ST_Pcode")
if (sum(a$damagesAgriManual) != sum(b$damagesAgriManual, na.rm = T)) warning("Check NG: Not all damagesAgriManual values have been imported (sums do not match)")

b[,damagesTotal:=damagesResi_mean+damagesAgriManual+damagesOtherCapstock]
LossCalc$results$ST <- b


rm(a,b)






## 4.4 PLOT DAMAGES ON MAP ----

## plot flow with time ##
GIS <- list()
GIS$m$flowTime <- leaflet() %>%
  addTiles() %>%
  addMinicharts(lng = gaugeData$long$Lon, lat = gaugeData$long$Lat, chartdata = gaugeData$long$`WaterLevel cm`,
                showLabels = T, time = gaugeData$long$Date1)
#GIS$m$flowTime


a <- read_sf(dsn = iFile$GIS$inp$admn3_mimu$dsn, 
             layer = iFile$GIS$inp$admn3_mimu$layer)
b <- merge(a, LossCalc$results$TS, by.x="TS_PCODE", by.y="TS_Pcode")
c <- merge(b,LossCalc$results$ST, by.x="ST_PCODE", by.y="ST_Pcode")

#State
iFile$GIS$inp$adm1_mimu_bago_shan_joined <- file.path("..","..","2-analysis","GIS", 
                                     "input layers", "damages results shp joined")
d <- read_sf(dsn = iFile$GIS$inp$adm1_mimu_bago_shan_joined)
# d[d$ST_PCODE %in% c("MMR007", "MMR008"),]$ST_PCODE <- "MMR111" #fix the Bago join
# d[d$ST_PCODE %in% c("MMR014", "MMR015", "MMR016"),]$ST_PCODE <- "MMR222" #fix the Bago join

damages_ST <- merge(d,LossCalc$results$ST, by.x="ST_PCODE", by.y="ST_Pcode")

#District
e <- read_sf(dsn=iFile$GIS$inp$adm2_mimu)
f <- merge(e, LossCalc$results$DT, by="DT_PCODE", all.x=T)
g <- merge(f, LossCalc$results$ST, by.x="ST_PCODE", by.y="ST_Pcode", all.x=T)

if("damagesAgriManual" %in% colnames(g)) g$damagesAgriStock.y <- g$damagesAgriManual

  ## Tidy the data ##
c$Tot_ppl[is.na(c$Tot_ppl)] <- 0  #replace NAs in affeted people with 0's
c$Tot_ppl_manual[is.na(c$Tot_ppl_manual)] <- 0  #replace NAs in affeted people with 0's
c$affected_tot[is.na(c$affected_tot.x)] <- 0

g$damagesTotal.x[ g$damagesTotal.x == 0 ] <- NA                    #.x indicates district values. .y indicates state values
g$damagesResi_mean.y[ is.na(g$damagesResi_mean.y) ] <- 0
g$damagesAgriStock.y[ is.na(g$damagesAgriStock.y) ] <- 0
g$damagesOtherCapstock.y[ is.na(g$damagesOtherCapstock.y) ] <- 0

damages_ST$damagesTotal[ damages_ST$damagesTotal == 0 ] <- NA                   
damages_ST$damagesResi_mean[ is.na(damages_ST$damagesResi_mean) ] <- 0
damages_ST$damagesAgriStock[ is.na(damages_ST$damagesAgriManual) ] <- 0
damages_ST$damagesOtherCapstock[ is.na(damages_ST$damagesOtherCapstock) ] <- 0




myPalette <- list()
myPalette$damages <- colorNumeric( palette = "YlOrBr", domain = damages_ST$damagesTotal, na.color = "transparent")
myPalette$bin$tot <- colorBin(palette = "Reds", domain = damages_ST$damagesTotal, bins = c(0,500000, 2000000, 10000000, 1e9), na.color = "transparent")
myPalette$bin$resi <- colorBin(palette = "Blues", domain = damages_ST$damagesTotal, bins = c(0,500000, 2000000, 10000000, 1e9), na.color = "transparent")
myPalette$bin$agri <- colorBin(palette = "YlOrBr", domain = damages_ST$damagesTotal, bins = c(0,500000, 2000000, 10000000, 1e9), na.color = "transparent")
myPalette$bin$other <- colorBin(palette = "Greens", domain = damages_ST$damagesTotal, bins = c(0,500000, 2000000, 10000000, 1e9), na.color = "transparent")

myLab <- list()
myLab$hov$DT <- paste("<b>District</b>: ", g$DT, " (",g$DT_Name_M3, ")<br/>",
                      "<b>Affected People (DDM)</b>: ", formatC(g$reportedAffectedPpl_manual.x,format = "d", big.mark = ","), "<br/>",
                      "<b>% Pop. Affected</b>: ", round(g$affected_tot.x*100,1), "%<br/><br/>",
                      "<b>State</b>: ", g$ST, "<br/>",
                      "<b><u>State</u> Damages</b> (USD):<br/>",
                      "<i>Residential Buildings</i>: $", round(g$damagesResi_mean.y/1e6,1), "m<br/>",
                      "<i>Agriculture</i>: $", round(g$damagesAgriStock.y/1e6,1), "m<br/>",
                      "<i>Other Sectors</i>: $", round(g$damagesOtherCapstock.y/1e6,1), "m",
                      sep="") %>%
  lapply(htmltools::HTML)

myLab$hov$ST <- paste("<b>State</b>: ", damages_ST$ST, " (",damages_ST$NAME_M3, ")<br/>",
                      "</br>",
                      "<b>Estimated Damages</b> (USD):<br/>",
                      "<i>Residential Buildings</i>: $", round(damages_ST$damagesResi_mean/1e6,1), "m<br/>",
                      "<i>Agriculture</i>: $", round(damages_ST$damagesAgriManual/1e6,1), "m<br/>",
                      "<i>Other Sectors</i>: $", round(damages_ST$damagesOtherCapstock/1e6,1), "m",
                      sep="") %>%
  lapply(htmltools::HTML)


damages_ST$damagesResi_mean[ damages_ST$damagesResi_mean==0 ] <- NA
damages_ST$damagesAgriManual[ damages_ST$damagesAgriManual==0 ] <- NA
damages_ST$damagesOtherCapstock[ damages_ST$damagesOtherCapstock==0 ] <- NA



myOptions <- highlightOptions(color="black", weight = 2, bringToFront = T, stroke = T, opacity = 1)

myLabels <- c("<0.5m", "0.5m-2m", "2m-10m", ">10m")


GIS$m$damages$ST <- leaflet(data = damages_ST) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=d,stroke = T,color = "grey",fill = F, opacity = 1, weight = 0.5) %>%  #admin 1 lines
  addPolygons(layerId = ~paste0(ST_PCODE,"_tot"),                                                       #chloropleth
              color = ~myPalette$bin$tot(damagesTotal), fillOpacity = 0.8,
              stroke = F,
              # label = myLab$hov$ST,
              highlight=myOptions,
              group = "Total",
              popup = myLab$hov$ST,
              popupOptions = myOptions) %>%
  addPolygons(layerId = ~paste0(ST_PCODE,"_res"),                                                       #chloropleth
              color = ~myPalette$bin$resi(damagesResi_mean), fillOpacity = 0.8,
              stroke = F,
              group = "Resi",
              popup = myLab$hov$ST,
              highlight=myOptions) %>%
  addPolygons(layerId = ~paste0(ST_PCODE,"_agr"),                                                       #chloropleth
              color = ~myPalette$bin$agri(damagesAgriManual), fillOpacity = 0.8,
              stroke = F,
              popup = myLab$hov$ST,
              group = "Agri",
              highlight=myOptions) %>%
  addPolygons(layerId = ~paste0(ST_PCODE,"_oth"),                                                       #chloropleth
              color = ~myPalette$bin$other(damagesOtherCapstock), fillOpacity = 0.8,
              stroke = F,
              popup = myLab$hov$ST,
              group = "Other",
              highlight=myOptions) %>%
  addLegend(pal = myPalette$bin$other,
            values = ~damagesOtherCapstock,
            title = "Other Damages ($)",
            position = "bottomleft",
            group = "Other",
            labFormat = function(type, cuts, p) {  # https://stackoverflow.com/questions/47410833/how-to-customize-legend-labels-in-r-leaflet
              paste0(myLabels)
            },
            opacity = 0.9) %>%
  addLegend(pal = myPalette$bin$agri,
            values = ~damagesAgriManual,
            title = "Agric. Damages ($)",
            position = "bottomleft",
            group = "Agri",
            labFormat = function(type, cuts, p) {  # https://stackoverflow.com/questions/47410833/how-to-customize-legend-labels-in-r-leaflet
              paste0(myLabels)
            },
            opacity = 0.9) %>%
  addLegend(pal = myPalette$bin$resi,
            values = ~damagesResi_mean,
            title = "Resid. Damages ($)",
            position = "bottomleft",
            group = "Resi",
            labFormat = function(type, cuts, p) {  # https://stackoverflow.com/questions/47410833/how-to-customize-legend-labels-in-r-leaflet
              paste0(myLabels)
            },
            opacity = 0.9) %>%
  addLegend(pal = myPalette$bin$tot,
            values = ~damagesTotal,
            title = "Total Damages ($)",
            position = "bottomleft",
            group = "Total",
            labFormat = function(type, cuts, p) {  # https://stackoverflow.com/questions/47410833/how-to-customize-legend-labels-in-r-leaflet
              paste0(myLabels)
            },
            opacity = 0.9) %>%
  addLayersControl(baseGroups = c("Total", "Resi", "Agri", "Other"),
                   options = layersControlOptions(collapsed = F), position = "topright")

GIS$m$damages$ST




# addLegend("bottomleft",
#           colors =c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404"),
#           labels= c("minor", "","moderate","", "major"),
#           title= "District Damages",
#           opacity = 0.9)



GIS$m$damages$DT <- leaflet(data = g) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(layerId = ~DT_PCODE,                                                       #chloropleth
              color = ~myPalette$damages(damagesTotal.x), fillOpacity = 0.8,
              stroke = F,
              label = myLab$hov$DT,
              highlight=myOptions) %>%
  addPolygons(data=d,stroke = T,color = "grey",fill = F, opacity = 1, weight = 0.5) %>%  #admin 1 lines
  addLegend("bottomleft", 
            colors =c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404"),
            labels= c("minor", "","moderate","", "major"),
            title= "District Damages",
            opacity = 0.9)
GIS$m$damages$DT

  # addLegend(pal = myPalette$damages,
  #           values = ~damagesTotal.x,
  #           title = "District Damages",
  #           position = "bottomleft",
  #           opacity = 0.9)


rm(a,b,c,d,e)

## 6.0 SAVE OUTPUTS ---------------------------------------------------
# dir.create(file.path("outputs",oFolder), showWarnings = F)
# fwrite(LossCalc$results$TS, file = file.path("outputs", oFolder, "Damages_23Aug_township.csv"))
# fwrite(LossCalc$results$DT, file = file.path("outputs", oFolder, "Damages_23Aug_district.csv"))
# fwrite(LossCalc$results$ST, file = file.path("outputs", oFolder, "Damages_23Aug_state.csv"))
# write.csv(LossCalc$sums, file = file.path("outputs", oFolder, "sums_23Aug.csv"))
# save.image(file = file.path("outputs", oFolder, "allData.RData"))

 layout=matrix(c(rep(1, 3), rep(2,4)), nrow = 1)
jpeg(filename = "test.jpg", width = 7, height = 4, units = "in", res = 300)
multiplot(damageData$plots$totals_ppl, damageData$plots$trends_ppl,
          layout = layout)
dev.off()


#saveWidget(GIS$m$damages, file = file.path("outputs", oFolder, "leafletMap.html"))