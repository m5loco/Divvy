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


##Read Model Results from csv
x <- getURL("https://raw.githubusercontent.com/m5loco/Divvy/master/new_stations_enhanced.csv")
ns <- read.csv(text = x)

##Perform some data engineering to prep data for app display and use in leaflet
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

getTable<-function(selected_mdl,pct)
{
  tree_1<-ns[ns$tree_Yes>pct,'GEOID']
  forest_1<-ns[ns$forest_Yes>pct,'GEOID']
  bayes_1<-ns[ns$bayes_Yes>pct,'GEOID']
  xgboost_1<-ns[ns$new_blocks_xgboost>pct,'GEOID']
  all_1<-Reduce(intersect, list(tree_1,forest_1,bayes_1,xgboost_1))
  
  tree_count<-length(tree_1)
  forest_count<-length(forest_1)
  bayes_count<-length(bayes_1)
  xgboost_count<-length(xgboost_1)
  all_count<-length(all_1)
  all_values<-c(tree_count,forest_count,bayes_count,xgboost_count,all_count)
  
  mdl_counts<-data.frame(mdl=c("Decision Tree","Random Forest","Naive Bayes","XGBoost","All Models"),
                         vals=all_values)

  mdl_counts[mdl_counts$mdl==selected_mdl,]
}

getTable_Blocks<-function(mdl,pct)
{
  tree_1<-ns[ns$tree_Yes>pct,'GEOID']
  forest_1<-ns[ns$forest_Yes>pct,'GEOID']
  bayes_1<-ns[ns$bayes_Yes>pct,'GEOID']
  xgboost_1<-ns[ns$new_blocks_xgboost>pct,'GEOID']
  all_1<-Reduce(intersect, list(tree_1,forest_1,bayes_1,xgboost_1))
  
  if (mdl=="All Models") {geoids<-all_1}
  if (mdl=="Decision Tree") {geoids<-tree_1}
  if (mdl=="Random Forest") {geoids<-forest_1}
  if (mdl=="Naive Bayes") {geoids<-bayes_1}
  if (mdl=="XGBoost") {geoids<-xgboost_1}
  
  nsrows<-ns[ns$GEOID %in% geoids,c("BG_Tract","Median_Family_Income","age_25_29","Transp_Public_betw2024","Transp_Walked",
                                    "HH_OtherPlus")]
  nsrows[order(-nsrows$Median_Family_Income),]
  
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
                   img(src="https://github.com/m5loco/Divvy/raw/master/Divvy.jpg",height="25"),
                   titlePanel("Find Location for New Stations:"),
                   radioButtons(inputId = "mdl",
                               label = "Model:",
                               choices = list('All Models','Decision Tree','Random Forest','Naive Bayes','XGBoost'),
                               selected = "All Models"),
                   sliderInput(inputId = "pct",
                               label = "Probability of Success:",
                               value=0.75,min=0.50,max=1.00),
                   div(DT::dataTableOutput("tbl_blk"),style="font-size:75%")
                   )),
    miniTabPanel("Census Data", icon = icon("table"),
                 miniContentPanel(
                   div(DT::dataTableOutput("stbl"),style="font-size:70%")
                 )),
   miniTabPanel("Map", icon = icon("map-o"),
                miniContentPanel(padding = 0,leafletOutput("sMap",height="100%") )
                )
  )
)

server <- function(input,output){
  
  output$tbl_blk <- DT::renderDT(getTable(input$mdl,input$pct),
                             colnames=c("Model","Predicted Successful Blocks"),
                             options = list(paging=FALSE,searching=FALSE,dom = 't',ordering=FALSE,select=FALSE),rownames=FALSE) 
  
  sSub <-  reactive({
    getTable_Blocks(input$mdl,input$pct)
  })
  
  output$stbl <- DT::renderDT(datatable(sSub(),selection = "multiple",
                                        colnames=c("BlkGrp | Tract","Med. Income","Age 25-29","Pub Trans btw 20-24","Walker","HH-Other+"),
                                        rownames=FALSE,
                                        options=list(paging=TRUE,searching=FALSE,ordering=TRUE,stateSave=TRUE)) 
                              %>% formatCurrency(c("Median_Family_Income"),"$",digits=0))
  
  output$sMap <- renderLeaflet({
    
    mdl<-input$mdl
    pct<-input$pct
    
    tree_1<-ns[ns$tree_Yes>pct,'GEOID']
    forest_1<-ns[ns$forest_Yes>pct,'GEOID']
    bayes_1<-ns[ns$bayes_Yes>pct,'GEOID']
    xgboost_1<-ns[ns$new_blocks_xgboost>pct,'GEOID']
    all_1<-Reduce(intersect, list(tree_1,forest_1,bayes_1,xgboost_1))
    
    if (mdl=="All Models") {geoids<-all_1}
    if (mdl=="Decision Tree") {geoids<-tree_1}
    if (mdl=="Random Forest") {geoids<-forest_1}
    if (mdl=="Naive Bayes") {geoids<-bayes_1}
    if (mdl=="XGBoost") {geoids<-xgboost_1}
    
    if (length(geoids)>1)
    {
      pins<-ns[ns$GEOID %in% geoids,]
    
      leaflet() %>%
      addTiles()%>%
        addMarkers(lng=pins$longitude,
                   lat=pins$latitude,
                   popup = getPopup(pins),
                   group="myMarkers")
    }
  })
  
  observeEvent(input$stbl_rows_selected, {

    row_selected = sSub()[input$stbl_rows_selected,]
    pins<-ns[ns[,"BG_Tract"] %in% row_selected[,"BG_Tract"],]
    
    proxy <- leafletProxy('sMap')
    
    clearGroup(proxy,"myMarkers")
    
    proxy %>%
      addTiles()%>%
      addMarkers(
        lng=pins$longitude,
        lat=pins$latitude,
        popup = getPopup(pins),
        group="myMarkers")
  })
  
}

shinyApp(ui, server)