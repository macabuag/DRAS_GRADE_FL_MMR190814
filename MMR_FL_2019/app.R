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

txt <- fread("siteText.csv")



## User-defined variables -----------------------------------------------
iFile <- list()
iFile$GIS$inp$admn3_mimu$dsn <- file.path("maps", "mmr_polbnda_adm3_250k_mimu")
iFile$GIS$inp$admn3_mimu$layer <- "mmr_polbnda_adm3_250k_mimu"

iFile$GIS$inp$adm2_mimu <- file.path("maps", "mmr_polbnda_adm2_250k_mimu")
iFile$GIS$inp$adm1_mimu <- file.path("maps", "mmr_polbnda2_adm1_250k_mimu")

iFile$GIS$inp$adm1_mimu_bago_shan_joined$dsn <- file.path("maps", "damages results shp joined")
iFile$GIS$inp$adm1_mimu_bago_shan_joined$layer <- "mmr_polbnda2_adm1_250k_mimu-modified-Bago-Shan-joined-v2"


## Common HTML ##
commonHTML <- list()
commonHTML$header <- fluidRow(column(width=4,
                                     HTML('<div align="left">
                                                <img src="World Bank Logo.png" height="50" width="272"/>
                                           </div>')),
                              column(width=4,
                                     HTML('<div align="center">
                                                <img src="logo_DRAS.jpg" height="50" width="178"/>
                                           </div>')),
                              column(width=4,
                                     HTML('<div align="right">
                                             <img src="GFDRR logo.png" height="50" width="216"/>
                                           </div>')
                              ))



## 1.3 PREPARE MAP -----------------------------------------------------------------
# shiny.io can't read the map from Risk_Database.rdata. So move map to local folder and read/prepare map here
a <- read_sf(dsn = iFile$GIS$inp$admn3_mimu$dsn, 
             layer = iFile$GIS$inp$admn3_mimu$layer)
b <- merge(a, LossCalc$results$TS, by.x="TS_PCODE", by.y="TS_Pcode")
c <- merge(b,LossCalc$results$ST, by.x="ST_PCODE", by.y="ST_Pcode")

#State
d <- read_sf(dsn = iFile$GIS$inp$adm1_mimu_bago_shan_joined$dsn,
             layer = iFile$GIS$inp$adm1_mimu_bago_shan_joined$layer)
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
mySmoothFactor <- 1.5

GIS$m$damages$ST <- leaflet(data = damages_ST) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data=d,stroke = T,color = "grey",fill = F, opacity = 1, weight = 0.5) %>%  #admin 1 lines
    addPolygons(layerId = ~paste0(ST_PCODE,"_tot"), smoothFactor = mySmoothFactor,                        #chloropleth
                color = ~myPalette$bin$tot(damagesTotal), fillOpacity = 0.8,
                stroke = F,
                # label = myLab$hov$ST,
                highlight=myOptions,
                group = "Total",
                popup = myLab$hov$ST,
                popupOptions = myOptions) %>%
    addPolygons(layerId = ~paste0(ST_PCODE,"_res"), smoothFactor = mySmoothFactor,
                color = ~myPalette$bin$resi(damagesResi_mean), fillOpacity = 0.8,
                stroke = F,
                group = "Resi",
                popup = myLab$hov$ST,
                highlight=myOptions) %>%
    addPolygons(layerId = ~paste0(ST_PCODE,"_agr"), smoothFactor = mySmoothFactor,
                color = ~myPalette$bin$agri(damagesAgriManual), fillOpacity = 0.8,
                stroke = F,
                popup = myLab$hov$ST,
                group = "Agri",
                highlight=myOptions) %>%
    addPolygons(layerId = ~paste0(ST_PCODE,"_oth"), smoothFactor = mySmoothFactor,
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



## 2.0 UI ---------------------------------------------------------------------------
ui <- navbarPage(title = "GRADE: MMR FL 2019",
                 tabPanel(title = "Interactive Map",
                          div(class="outer",
                              tags$head(
                                  # Include our custom CSS
                                  includeCSS(file.path("www","styles.css")),
                                  includeScript(file.path("www", "geomap.js"))
                              ) ,
                              
                              HTML('</br>'),
                              
                              commonHTML$header, #add logos
                              
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("map", width="100%", height="90%"),
                              
                              # Shiny versions prior to 0.11 should use class = "modal" instead.
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = F,
                                            draggable = TRUE, top = "auto", left = "auto", right = 20, bottom = 60,
                                            width = 600, height = 425,
                                            
                                            h2("Estimate of Capital Damages (US$mn)") ,
                                            
                                            withSpinner(DT::dataTableOutput(outputId = "damagesTable")),
                                            
                                            HTML('<p><small>*"Other" refers to "Non-Residential Buildings and Infrastructure"</small></p>
                                </br>
                                <b>See GRADE Report and KMZ of georeferenced photos and videos 
                                <a href="https://1drv.ms/u/s!AjeYHHDFC30TkPZDPlOVIkaWNhrYQQ?e=hqxqqN"> here </a></b>')
                              )
                          )
                 ), 
                 
                 tabPanel(title = "Disclaimer",
                          HTML('</br>'),
                          commonHTML$header, #add logos
                          
                          HTML('<div align="center"><h1>GLOBAL RAPID POST DISASTER DAMAGE ESTIMATION (GRADE)</h1>
                                <h2>MONSOON FLOODING IN MYANMAR (25 JUNE – 23 AUGUST)</h2>
                                <h3>WORLD BANK GPURL D-RAS TEAM</h3></div>'),
                          br(),
                          h2(txt$title[[2]]),
                          p(txt$text[[2]]),
                          
                          h2(txt$title[[1]]),
                          p(txt$text[[1]])
                 )
)

## 3,0 SERVER -------------------------------------------
server <- function(input, output) {
    
    output$map <- renderLeaflet({
        GIS$m$damages$ST
    })
    
    output$damagesTable <- DT::renderDataTable({
        dt <- LossCalc$results$ST[,.(State, 
                                     `Residential Damages`=round(damagesResi_mean/1e6,1), 
                                     `Other* Damages` = round(damagesOtherCapstock/1e6,1),
                                     `Agriculture Damages` = round(damagesAgriManual/1e6,1),
                                     `Total Damages` = round(damagesTotal/1e6,1))]
        setorder(dt,-`Total Damages`)
        #browser()
        datatable(dt, selection = "none", options = list(dom="tp", pageLength = 5)) %>% #https://rstudio.github.io/DT/010-style.html
            formatStyle(columns = "Total Damages", 
                        background = styleColorBar(data = range(dt$`Total Damages`),
                                                   color = "tomato")) %>%
            formatStyle(columns = "Residential Damages", 
                        background = styleColorBar(data = range(dt$`Residential Damages`),
                                                   color = "skyblue")) %>%
            formatStyle(columns = "Other* Damages", 
                        background = styleColorBar(data = range(dt$`Other* Damages`),
                                                   color = "palegreen")) %>%
            formatStyle(columns = "Agriculture Damages", 
                        background = styleColorBar(data = range(dt$`Agriculture Damages`),
                                                   color = "gold"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)