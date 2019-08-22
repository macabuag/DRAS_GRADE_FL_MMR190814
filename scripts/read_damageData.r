# Title: read_damageData.r
# By: Josh Macabuag
# Date: 19-Aug-2019
# Description:
# - Read Myanmar (MMR) Damage Observations Data from Department of Disaster Management


## 1.0 SETUP -----------------------------------------------------------
## User-Defined Values --
UCC <- 65 #$USD/m^2. Tony: $57 in 2015 PDNA, increased to reflect GDP growth
PPD <- 4.2 #Tony: 4.6 in 2014 census, reduced for population trends
houseSize <- 50 #m^2. Tony: 2015 PDNA
MDR <- 0.1

## Input Files --
iFile <- list()

iFile$damageData <- file.path("..","Spreadsheets","Damage Observations", "Damage Observations190819.xlsx")
iFile$gauge <- file.path("inputs","Stations+ River Water Levels.xlsx")

sheetNames <- c("10-8-2019ed", "12-8-2019ed", "14-8-2019ed", "16-8-2019ed", "19-8-2019ed", "20-8-2019ed")
resultsRange <- "AD3:AI18"
latestSheet <- sheetNames[length(sheetNames)] #change this if you want to consider a date other than the last specified in sheetNames

iFile$`Myanmar PCodes` <- file.path("..","..","1-data","Exposure","Myanmar PCodes Release-VIII.i_Sep2017_Countrywide.xlsx")

#iFile$GAD_MIMU_Census_CatDat <- file.path("..","Spreadsheets", "Township Exp Loss Data", "GAD_MIMU_Census_CATDAT_townshipData_calc.xlsx")
#iFile$GAD_MIMU_Census_CatDat <- file.path("..","Spreadsheets", "Township Exp Loss Data", "GAD_MIMU_Census_CATDAT_townshipData_calcv3.xlsx")
iFile$GAD_MIMU_Census_CatDat <- file.path("..","Spreadsheets", "Township Exp Loss Data", "GAD_MIMU_Census_CATDAT_townshipData_calcv3_x.xlsx")

iFile$censusFolder <- file.path("..","..","1-data", "Exposure", "Census Data")


## GIS ##
iFile$GIS$inp$admn3_mimu$dsn <- file.path("..","..","1-data","Exposure", "Admin Boundaries")
iFile$GIS$inp$admn3_mimu$layer <- "mmr_polbnda_adm3_250k_mimu"

iFile$GIS$inp$adm1_mimu <- file.path("..","..","1-data","Exposure", 
                                     "Admin Boundaries", "mmr_polbnda2_adm1_250k_mimu")

iFile$GIS$inp$SEADRIF_folder <- file.path(file.path("..","..","1-data","Hazard","SEADRIF"))




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
                             range = resultsRange, trim_ws = T))
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
damageData$plots$trends <- ggplot(data=damageData$gg$state, aes(x=date, y=value, colour=State)) +
  geom_line() +
  facet_wrap(~metric, scales = "free") +
  scale_y_continuous(labels = comma) +
  theme_classic()
damageData$plots$trends


damageData$plots$totals <- ggplot(data=damageData$gg$totals, aes(x=date, y=value)) +
  geom_line() +
  facet_wrap(~metric, scales = "free") +
  scale_y_continuous(labels = comma) +
  theme_classic()
damageData$plots$totals


## 3.0 FLOW DATA -----------------------------------------------------------
## 3.1 READ SPREADSHEETS ----
gaugeData <- list()
gaugeData$raw <- data.table(read_excel(path = iFile$gauge, sheet = 1))


## 3.2 ADD STATE NAME TO GAUGE POINTS ----
## read in shapefiles ##
GIS <- list()
GIS$inp$adm1_mimu <- st_read(dsn=iFile$GIS$inp$adm1_mimu)

## plot gauge locations ##
GIS$m$gaugePoints <- leaflet(data = ) %>%
  addTiles() %>%
  addPolygons(data=GIS$inp$adm1_mimu) %>%
  addCircles(data = gaugeData$raw, lng = ~Lon, lat = ~Lat)
#GIS$m$gaugePoints

## identify which stations in which admin 1 (spatial join on state) ##
b <- st_as_sf(x = gaugeData$raw, coords = c("Lon", "Lat"),  #convert data.table with lat-long to spatial object
              crs=st_crs(GIS$inp$adm1_mimu))                #obtain the CRS of the admin layer to set other layers as the same
nrow(gaugeData$raw)
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
gaugeData$raw <- data.table(gaugeData$shp)

rm(b,c)


## 3.3 PLOT ----
gaugeData$plots$all <- ggplot(data=gaugeData$raw) +
  geom_line(aes(x=Date1, y = `Distance to Danger Level (cm)`,
                colour=Lat, group=factor(Sr))) +
  geom_hline(yintercept = 0, linetype="dashed", alpha=0.2) +
  coord_cartesian(expand = F) +
  labs(title = "Flow Gauge Data", subtitle = iFile$gauge) +
  theme_classic()
gaugeData$plots$all

gaugeData$plots$states <- gaugeData$plots$all +
  facet_wrap(.~ST)
gaugeData$plots$states


## 4.0 LOSSES ----------------------------------------------
## 4.1 FACTOR DAMAGE DATA ----
## Calc Factor 14th - 19th ##
  #join state-level data from 14th & 19th
c <- merge(x = damageData$daily[[latestSheet]], y = damageData$daily$`14-8-2019ed`, by="State", all.x=T, all.y=T) #Naypyidaw doesn't appear

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
a <- read_excel(iFile$damageData, sheet = "14-8-2019ed", range = "AR6:AY71") #65 affected townships (reported on 14th)
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

  #check that the sums add up
e <- colSums(d[,`Total_affected_houses`:`Total_kyat`], na.rm = T)
f <- colSums(damageData$daily[[latestSheet]][State!="Grand Total",`Total_affected_houses`:`Total_kyat`], na.rm = T)
if (abs(sum(e-f))<1) {
  cat(paste0("CHECK OK: factored TS-level totals and original state-level totals match."))
} else {
  warning("CHECK NG: factored TS-level totals and original state-level totals DO NOT match.\nCompare the following:")
  print(damageData$daily[[latestSheet]][,State:Total_kyat])
  print(d[,.(Total_affected_houses=sum(Total_affected_houses)), by=State])
  #colSums(damageData$daily$`14-8-2019ed`[State!="Grand Total",`Total_affected_houses`:`Total_kyat`], na.rm = T)
}


## Add Township name ##
damageData$daily$latest_factored_TS <- merge(Myanmar_PCodes$Townships[,.(TS_Pcode, Township, District)], d, by="TS_Pcode")
damageData$daily$latest_factored_TS$date <- as.Date(substr(latestSheet, start = 1, stop = nchar(latestSheet)-2), 
                                                    format = "%d-%m-%Y")
rm(a,b,c,d,e,f)


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
sum(TS_census$MIMU$cropped[,(`Pop Urban Both sexes- All`/`Pop Rural Both sexes- All`)
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
           all.x=T, all.y=F)
a[,affected_tot_HH := Tot_household/`Conventional HH Total Number (n)-`] #ratio of reported affected HH / total HH (2014 census) per township
a[,affected_tot := Tot_ppl/`Total Pop Both sexes- All`] #ratio of reported affected HH / total HH (2014 census) per township
a[,affectedDwellings_PPD := Tot_ppl/PPD]
a[,affectedDwellings_census := Tot_ppl/`Mean household size`]
a[,HH_DwellPPD_ratio := Tot_household/affectedDwellings_PPD]
a[,HH_DwellCensus_ratio := Tot_household/affectedDwellings_census] #tests whether the PPD implied by the damage data matches the PPD in the census
a[,affectedHousingStock_UCC := Tot_household * UCC * houseSize]
a[,affectedGDP1 := affected_tot*`GDP Est 1 via State`]
a[,affectedGDP2 := affected_tot*`GDP Est 2 via Region old stat`]

a[,affectedResiCapstock := affected_tot*`Residential Cap Stock`]
a[,affectedOtherCapstock := affected_tot*`Other Building Cap Stock`]
a[,affectedAgriStock := affected_tot*`Agriculture Stock (for Calculation)`]


a[,damagesHousing := affectedHousingStock_UCC * MDR]
a[,damagesResiCapstock := affectedResiCapstock*MDR]
a[,damagesOtherCapstock := affectedOtherCapstock*MDR]
a[,damagesAgriStock := affectedAgriStock*MDR]

#a[,.(HH_DwellPPD_ratio,HH_DwellCensus_ratio)]
#ToDo:
#a[,affectedAgri := affected_tot*<AGRI COLUMN>]
#a[,affectedAgri := affected_tot*<AGRI COLUMN>]

colSums(a[,`Number of Village Tracts`:damagesBldgs], na.rm = T)

LossCalc <- list()
LossCalc$results <- a
rm(a)

fwrite(LossCalc$results, file = file.path("outputs", "Damages_20Aug.csv"))

## 4.4 PLOT DAMAGES ON MAP ----


## 4.0 SENTINEL DATA ------------------------------------------------------------------------

if(!require(rgdal)) {
  install.packages("rgdal")
  library(rgdal)
}

if(!require(tmap)) {
  install.packages("tmap")
  library(tmap)
}


SEADRIF <- list()

#list raster files
SEADRIF$fileList <- list.files(iFile$GIS$inp$SEADRIF_folder, pattern = "*.tif$")
SEADRIF$fileTable <- data.table(n=1:length(SEADRIF$fileList),
                                files=SEADRIF$fileList,
                                date=as.Date(word(SEADRIF$fileList,2,sep="_")))

#read in admin3 boundaries
GIS$inp$admn3_mimu <- st_read(dsn = iFile$GIS$inp$admn3_mimu$dsn, 
                              layer = iFile$GIS$inp$admn3_mimu$layer)


countPixels <- function(tileList, polygon, subDivision="ST", subDivNames=unique(polygon[[subDivision]]),
                        pixelToCount=3, outputFolder="outputs"){
  for (iSubDiv in subDivNames) {
    time_start <- Sys.time()
    
    if(is.null(intersect(extent(tileList$tile), extent(polygon[polygon[[subDivision]] == iSubDiv,])))) { #check if the raster overlaps with the state
      tileList$pixelCounts[[iSubDiv]] <- 0
    } else {
      tileList$pixelCounts[[iSubDiv]] <-
        sum(
          getValues(
            mask(x =                                                               
                   crop( tileList$tile,
                         extent(polygon[polygon[[subDivision]] == iSubDiv,])  # 1.reduce the raster extents to that of the state (to save time)
                   ), mask = polygon[polygon[[subDivision]] == iSubDiv,])       # 2.mask the raster by the state boundary
          )==pixelToCount,                                                                       # 3.count the pixels of the masked raster 
          na.rm = T)
    } #end else
    
    tileList$duration[[iSubDiv]] <- Sys.time()-time_start
  } #end for iSubDiv
  
  save(tileList, file = file.path(outputFolder, paste0(tileList$name, ".r")))
  
  return(tileList)
}


SEADRIF$MyanmarAug2019$name <- "MyanmarAug2019"
SEADRIF$MyanmarAug2019$tile <- raster(file.path(iFile$GIS$inp$SEADRIF_folder, "Aug2019", "MyanmarAug2019.tif"))
SEADRIF$MyanmarAug2019 <- countPixels(tileList = SEADRIF$MyanmarAug2019, 
                                      polygon = GIS$inp$admn3_mimu, subDivision = "TS_PCODE")



#for (i in (length(SEADRIF$fileList):1) ) {
for (i in (71:1) ) {
    #read the desired file
  a <- SEADRIF$fileList[[i]]
  b <- word(a, 2, sep = "_")  #date is in the filename
  SEADRIF[[b]]$name <- as.Date(b) #name the tile after the date
  SEADRIF[[b]]$tile <- raster(x = file.path(iFile$GIS$inp$SEADRIF_folder, SEADRIF$fileList[[i]])) #read in the tile
  
  # Count Flooded pixels per township
  SEADRIF[[b]] <- countPixels(tileList = SEADRIF[[b]], polygon = GIS$inp$admn3_mimu, subDivision = "TS_PCODE")
} #end for i



rm(a,b)




## DEBUG/TEST-----------------------------
##make smaller raster
a <- SEADRIF$`2019-07-25`$tile
b <- crop(x = a, extent(a)*0.1) #select the central 10% of a (to make the file smaller)
plot(b)

#count number of pixels with a value
system.time(sum(getValues(b)==0))
system.time(hist(b)) #takes longer. use the getvalue(x)==3 method



#make small raster which crosses border
c <- crop(a, extent(98.5, 98.9, 26.9, 27.1))
plot(c)

#mask
d <- mask(x = c, mask = GIS$inp$admn3_mimu)
e <- mask(x = c, mask = GIS$inp$admn3_mimu[GIS$inp$admn3_mimu$ST == "Kachin",])
system.time(mask(x = c, mask = GIS$inp$admn3_mimu))
system.time(mask(x = c, mask = GIS$inp$admn3_mimu[GIS$inp$admn3_mimu$ST == "Kachin",]))

#system.time(mask(x = a, mask = GIS$inp$admn3_mimu)) #stopped at 10mins!

#so the line is

system.time(sum(getValues(
  mask(x = a, mask = GIS$inp$admn3_mimu[GIS$inp$admn3_mimu$ST == "Kachin",])
)==2, na.rm = T))

system.time(sum(getValues(
  mask(x = crop(a, extent(GIS$inp$admn3_mimu[GIS$inp$admn3_mimu$ST == "Kachin",])),
       mask = GIS$inp$admn3_mimu[GIS$inp$admn3_mimu$ST == "Kachin",])
)==2, na.rm = T))






extent(GIS$inp$admn3_mimu)
extent(GIS$inp$admn3_mimu[GIS$inp$admn3_mimu$ST == "Kachin",])
#stack
#SEADRIF$stack <- 


tm_shape(shp = e) +
  tm_raster() +
  tm_shape(shp = GIS$inp$admn3_mimu) + 
  tm_borders() +
  tm_shape(shp = GIS$inp$admn3_mimu) + 
  tm_polygons("ST")



## 5.0 MAP DATA --------------------------------------------------------------------------
## 5.1 READ IN SHAPEFILES ----

## read in shapefiles ##

## plot flow with time ##
GIS$m$flowTime <- leaflet() %>%
  addTiles() %>%
  addMinicharts(lng = gaugeData$raw$Lon, lat = gaugeData$raw$Lat, chartdata = gaugeData$raw$`WaterLevel cm`,
                showLabels = T, time = gaugeData$raw$Date1)
GIS$m$flowTime




