#####################################
# Author: Mark Loula
# Class: MSDS 498
# Project: Team 54, Divvy Capstone - Station Analyzer  
# Date: 20-Feb-2020
#####################################

library(shiny)
library(leaflet)
library(RCurl)
library(miniUI)
library(DT)
library(dplyr)

logo<-"https://github.com/m5loco/Divvy/raw/master/Divvy.jpg"

##Read Model Results from csv
ns <- read.csv(text = getURL("https://raw.githubusercontent.com/m5loco/Divvy/master/new_stations_enhanced.csv"))

##Perform some data engineering on initial load to prep data for app display and use in leaflet
ns$GEOID<-as.character(ns$GEOID)
ns$Name<-as.character(ns$Name)
ns$longitude<-ns$CENTLON
ns$latitude<-ns$CENTLAT
ns$Name<-gsub("(.+?)(\\, Cook County, Illinois.*)", "\\1", ns$Name)
ns$Tract<-sub(".*Tract ", "", ns$Name)
ns$BlkGrp<-gsub("(.+?)(\\, Census Tract.*)", "\\1", ns$Name)
ns$BlkGrp<-sub(".*Block Group ", "", ns$BlkGrp)
ns$Name<-gsub("Block Group","Blk Grp",ns$Name)
ns$Name<-gsub("Census ","",ns$Name)
ns$BG_Tract<-paste(ns$BlkGrp," | ",ns$Tract,sep="")
ns$age_25_29<-ns$est_female_25_29 + ns$est_male_25_29
ns$HH_OtherPlus<-ns$HH_Alone+ns$HH_Nonfamily+ns$HH_Other

getSelectedGeoIds<-function(mdl,pct)
{
  tree_1<-ns[ns$tree_Yes>pct,'GEOID']
  forest_1<-ns[ns$forest_Yes>pct,'GEOID']
  bayes_1<-ns[ns$bayes_Yes>pct,'GEOID']
  xgboost_1<-ns[ns$new_blocks_xgboost>pct,'GEOID']
  all_1<-Reduce(intersect, list(tree_1,forest_1,bayes_1,xgboost_1))
  
  if (mdl=="Decision Tree") {geoids<-tree_1}
  else if (mdl=="Random Forest") {geoids<-forest_1}
  else if (mdl=="Naive Bayes") {geoids<-bayes_1}
  else if (mdl=="XGBoost") {geoids<-xgboost_1}
  else if (mdl=="All Models") {geoids<-all_1}
  
  geoids
}

getBlockCount<-function(geoids)
{
  length(geoids)
}

getBlocks<-function(geoids)
{
  nsrows<-ns[ns$GEOID %in% geoids,c("BG_Tract","Median_Family_Income","age_25_29",
                                    "Transp_Public_betw2024","Transp_Walked","HH_OtherPlus")]
  nsrows[order(-nsrows$Median_Family_Income),]
}

drawMap<-function(lmap,pins)
{
  lmap %>%
    addTiles()%>%
    addMarkers(
      lng=pins$longitude,
      lat=pins$latitude,
      popup = getPopup(pins),
      group="myMarkers")
}

getPopup<-function(pins)
{
  paste("<table><tr><td>Block Group:</td><td>", pins$BlkGrp, "</td></tr>",
        "<tr><td>Tract:</td><td>", pins$Tract, "</td></tr>",
        "<tr><td>Median Income:</td><td>", paste0("$", formatC(as.numeric(pins$Median_Family_Income), format="f", digits=0, big.mark=",")), "</td></tr>",
        "<tr><td>Age 25-29:</td><td>", pins$age_25_29, "</td></tr>",
        "<tr><td>Public Trans 20-24 mins:</td><td>",pins$Transp_Public_betw2024,"</td></tr>",
        "<tr><td>Walker:</td><td>",pins$Transp_Walked,"</td></tr>",
        "<tr><td>HH-Other+:</td><td>", pins$HH_OtherPlus,"</td></tr></table>")
}

ui <- miniPage(
  gadgetTitleBar("Divvy Field Tools - Location Research",left=NULL,right=NULL),
  miniTabstripPanel(
    miniTabPanel("Model", icon = icon("sliders"),
                 miniContentPanel(
                   img(src=logo,height="25"),
                   titlePanel("Find Location for New Stations:"),
                   radioButtons(inputId = "mdl",
                                label = "Model:",
                                choices = list('All Models','Decision Tree','Random Forest','Naive Bayes','XGBoost'),
                                selected = "All Models"),
                   sliderInput(inputId = "pct",
                               label = "Probability of Success:",
                               value=0.75,min=0.50,max=1.00),
                   div(DT::dataTableOutput("block_counts"),style="font-size:75%"))),
    miniTabPanel("Census Data", icon = icon("table"),
                 miniContentPanel(
                   div(DT::dataTableOutput("blocks"),style="font-size:70%"))),
    miniTabPanel("Map", icon = icon("map-o"),
                 miniContentPanel(padding = 0,leafletOutput("block_map",height="100%")))
    )
)

server <- function(input,output){
  
  sBlocks <-  reactive({getBlocks(getSelectedGeoIds(input$mdl,input$pct))})
  
  output$block_counts <- DT::renderDT(data.frame(model=input$mdl,count=length(sBlocks)),
                                      colnames=c("Model","Predicted Successful Blocks"),
                                      options = list(paging=FALSE,searching=FALSE,dom = 't',ordering=FALSE,select=FALSE),rownames=FALSE) 
  
  output$blocks <- DT::renderDT(datatable(sBlocks(),selection = "multiple",
                                          colnames=c("BlkGrp | Tract","Med. Income","Age 25-29","Pub Trans btw 20-24","Walker","HH-Other+"),
                                          rownames=FALSE,
                                          options=list(paging=TRUE,searching=FALSE,ordering=TRUE,stateSave=TRUE)) 
                                %>% formatCurrency(c("Median_Family_Income"),"$",digits=0))
  
  output$block_map <- renderLeaflet({
    
    blocks<-sBlocks()
    selected_blocks<-blocks[input$blocks_rows_selected,]
    
    if (nrow(selected_blocks)>0){pins<-ns[ns[,"BG_Tract"] %in% selected_blocks[,"BG_Tract"],]}
    else{pins<-ns[ns$BG_Tract %in% blocks$BG_Tract,]}
    
    leaflet() %>% drawMap(pins)
    
  })
  
  observeEvent(input$blocks_rows_selected, {
    
    row_selected = sBlocks()[input$blocks_rows_selected,]
    pins<-ns[ns[,"BG_Tract"] %in% row_selected[,"BG_Tract"],]
    
    leafletProxy('block_map') %>% clearGroup("myMarkers") %>% drawMap(pins)
    
  })
  
}

shinyApp(ui, server)