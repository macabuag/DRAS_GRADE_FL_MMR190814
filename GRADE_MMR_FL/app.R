# Title: RShinyTemplate.r
# By: Josh Macabuag
# Date: 28 Aug 2019
# Description: Template for making Shiny Apps. From https://shiny.rstudio.com/tutorial/
# 5 rules:
#   1) Begin each app with this template
#   2) Add elements as arguments to fluidPage()
#   3) Create reactive inputs with an *Input() function
#   4) Display reactive results with an *Output() function
#   5) Usa the server function to assemble inputs into outputs

## 1.0 SETUP ------------------------------------------------------------------------

## Packages ##
if(!require(data.table)) {
    install.packages("data.table")
    library(data.table)
}

if(!require(tidyr)) {
    install.packages("tidyr")
    library(tidyr)
}


if(!require(shiny)){
    install.packages("shiny")
    library(shiny)
}

if(!require(shinydashboard)){
    install.packages("shinydashboard")
    library(shinydashboard)
}

if(!require(shinycssloaders)){
    install.packages("shinycssloaders")
    library(shinycssloaders)
}

if(!require(DT)) {
    install.packages("DT")
    library(DT)
}


if(!require(sf)){
    install.packages(sf)
    library(sf)
}

if(!require(leaflet)){
    install.packages("leaflet")
    library(leaflet)
}


## 1.2 READ INPUTS --------------------------------------------------------
load(file = "allData.RData")

## User-defined variables -----------------------------------------------
iFile <- list()
iFile$GIS$inp$admn3_mimu$dsn <- file.path("maps", "mmr_polbnda_adm3_250k_mimu")
iFile$GIS$inp$admn3_mimu$layer <- "mmr_polbnda_adm3_250k_mimu"

iFile$GIS$inp$adm2_mimu <- file.path("maps", "mmr_polbnda_adm2_250k_mimu")
iFile$GIS$inp$adm1_mimu <- file.path("maps", "mmr_polbnda2_adm1_250k_mimu")

## Common HTML ##
commonHTML <- list()
commonHTML$disclaimer <- function(collapsed=F){
    box(title = "Disclaimer", status = "primary",solidHeader = F,
        width = 12,collapsible = T, collapsed = collapsed,
        p())} #txt$sidebarText$text[1]
commonHTML$header <- function(){
    fluidRow(column(width = 4, 
                    HTML('<div align="left"><img src="WB cropped.png" height="50" width="60"/></div>')),
             column(width = 4, 
                    HTML('<div align="center"><img src="logo_DRAS.jpg" height="50" width="167"/></div>')),
             column(width = 4, 
                    HTML('<div align="right"><img src="GFDRR cropped.png" height="50" width="55.7"/></div>')))
}


## 1.3 PREPARE MAP -----------------------------------------------------------------
# shiny.io can't read the map from Risk_Database.rdata. So move map to local folder and read/prepare map here

GIS <- list()

a <- read_sf(dsn = iFile$GIS$inp$admn3_mimu$dsn, 
             layer = iFile$GIS$inp$admn3_mimu$layer)
b <- merge(a, LossCalc$results$TS, by.x="TS_PCODE", by.y="TS_Pcode")
c <- merge(b,LossCalc$results$ST, by.x="ST_PCODE", by.y="ST_Pcode")

d <- read_sf(dsn = iFile$GIS$inp$adm1_mimu)

e <- read_sf(dsn=iFile$GIS$inp$adm2_mimu)
f <- merge(e, LossCalc$results$DT, by="DT_PCODE", all.x=T)
g <- merge(f, LossCalc$results$ST, by.x="ST_PCODE", by.y="ST_Pcode", all.x=T)

if("damagesAgriManual" %in% colnames(g)) g$damagesAgriStock.y <- g$damagesAgriManual

## Tidy the data ##
c$Tot_ppl[is.na(c$Tot_ppl)] <- 0  #replace NAs in affeted people with 0's
c$affected_tot[is.na(c$affected_tot.x)] <- 0

g$damagesTotal.x[ g$damagesTotal.x == 0 ] <- NA                    #.x indicates district values. .y indicates state values
g$damagesResi_mean.y[ is.na(g$damagesResi_mean.y) ] <- 0
g$damagesAgriStock.y[ is.na(g$damagesAgriStock.y) ] <- 0
g$damagesOtherCapstock.y[ is.na(g$damagesOtherCapstock.y) ] <- 0



myPalette <- list()
myPalette$damages <- colorNumeric( palette = "YlOrBr", domain = g$damagesTotal.x, na.color = "transparent")


myLab <- list()
myLab$hov$DT <- paste("<b>District</b>: ", g$DT, " (",g$DT_Name_M3, ")<br/>",
                      "<b>Affected People (DDM)</b>: ", formatC(g$reportedAffectedPpl.x,format = "d", big.mark = ","), "<br/>",
                      "<b>% Pop. Affected</b>: ", round(g$affected_tot.x*100,1), "%<br/><br/>",
                      "<b>State</b>: ", g$ST, "<br/>",
                      "<b><u>State</u> Damages</b> (USD):<br/>",
                      "<i>Residential Buildings</i>: $", round(g$damagesResi_mean.y/1e6,1), "m<br/>",
                      "<i>Agriculture</i>: $", round(g$damagesAgriStock.y/1e6,1), "m<br/>",
                      "<i>Other Sectors</i>: $", round(g$damagesOtherCapstock.y/1e6,1), "m",
                      sep="") %>%
    lapply(htmltools::HTML)

myOptions <- highlightOptions(color="black", weight = 2, bringToFront = T, stroke = T, opacity = 1)


GIS$m$damages <- leaflet(data = g) %>%
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


## 2.0 UI ---------------------------------------------------------------------------

header <- dashboardHeader(title = "GRADE MMR FL")

sidebar <- dashboardSidebar(disable = T)

body <- dashboardBody(
    commonHTML$header(),
    fluidRow(column(width = 8,
                     box(width = 12,
                         leafletOutput(outputId = "GRADEmap", height = 600))),
              column(width = 4,
                     fluidRow(box(width=12,
                                  plotOutput(outputId = "flowGaugePlot"))),
                     fluidRow(box(width = 12,
                                  plotOutput(outputId = "DDMtrends"))))),
    fluidRow(box(DT::dataTableOutput(outputId = "damagesTable")))
)


ui <- dashboardPage(header, sidebar, body)


## 3.0 Server -----------------------------------------------------------------------
# 1) Save objects to display to output$
# 2) Build objects to display with render*()
# 3) Use input values with input$

server <- function(input, output, session) {
     ## Map ##
     output$GRADEmap <- renderLeaflet({
         GIS$m$damages
     })
     
     ## Damages Table ##
     output$damagesTable <- DT::renderDataTable({
         dt <- LossCalc$results$ST[,.(State, 
                                      `Residential Damages`=round(damagesResi_mean/1e6,1), 
                                      `Non-Res + Infrastructure Damages` = round(damagesOtherCapstock/1e6,1),
                                      `Agriculture Damages` = round(damagesAgriManual/1e6,1),
                                      `Total Damages` = round(damagesTotal/1e6,1))]
         
         datatable(dt, selection = "none", options = list(dom="t")) #%>% #https://rstudio.github.io/DT/010-style.html
             # formatStyle(names(dt),
             #             background = styleColorBar(range(df), "lightblue"),
             #             backgroundSize = "98% 88%",
             #             backgroundRepeat = "no-repeat",
             #             backgroundPosition = "centre")
     })
     
     ## DDM Trends ##
     output$DDMtrends <- renderPlot({
         damageData$plots$trends
     })
     
     ## Flow Gauge Plot ##
     output$flowGaugePlot <- renderPlot({
         gaugeData$plots$all
     })
 }


## 4.0 Run the application ----------------------------------------------------------------
shinyApp(ui = ui, server = server)
