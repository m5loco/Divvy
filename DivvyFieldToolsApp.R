#####################################
# Author: Mark Loula
# Class: MSDS 498
# Project: Team 54, Divvy Capstone - Dashboard View 
# Date: 20-Feb-2020
#####################################

library(shiny)
library(leaflet)
library(RCurl)
library(shinydashboard)
library(DT)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rjson)
library(weathermetrics)
library(measurements)

#####Demand Forecast Inputs
default_station<-"Canal St & Adams St"
default_date<-Sys.Date()

Sys.setenv(TZ='GMT') ######Shiny.io servers run in GMT, this simulates running on the server, remove prior to deplying app to server
offset<-5
chi_tzone<-'America/Chicago'

get_CST_date<-function(in_time)
{
  as.Date(in_time-offset*60*60)       
}

get_CST_time<-function(in_time)
{
  in_time-offset*60*60       
}

get_GMT_time<-function(in_time)
{
  in_time+offset*60*60       
}

cst_date_str<-format(get_CST_date(Sys.time()),format="%Y-%m-%d")
cst_date<-as.Date(cst_date_str)
cst_time<-get_CST_time(Sys.time())
gmt_date_str<-format(Sys.Date(),format='%Y-%m-%d')

station_coeff <- getURL("https://raw.githubusercontent.com/m5loco/Divvy/master/2020_top_station_coefficients.csv")
station_coeff_df <- read.csv(text = station_coeff)
top_stations <- getURL("https://raw.githubusercontent.com/m5loco/Divvy/master/top_stations_ext.csv")
top_stations_df<- read.csv(text = top_stations)
cal <- getURL("https://raw.githubusercontent.com/m5loco/Divvy/master/2020_calendar.csv")
cal_df <- read.csv(text=cal)
cal_df$cal_date<-as.character(cal_df[,1])
cal_df$cal_date<-as.POSIXct(cal_df$cal_date,format="%Y-%m-%d")
cal_df$holiday<-as.integer(cal_df$holiday)

coeff_names<-names(station_coeff_df)
coeff_names<-coeff_names[!coeff_names=='station_id']

####Use Chicago Lat/Long for forecast, second station is because sometimes station is down
tryCatch(
  {
    lat<-41.876511229
    lon<--87.62054801
    endpoint<-fromJSON(file=paste("https://api.weather.gov/points/",lat,",",lon,sep=""))
    forecast_url<-endpoint$properties$forecast
    forecast_json <- fromJSON(file=forecast_url)
    forecastgrid_url<-endpoint$properties$forecastGridData
    forecastgrid_json <- fromJSON(file=forecastgrid_url)
    },
  error=function(e){
    return(e)
  },
  finally={
    lat<-42.3445
    lon<--88.0417
    endpoint<-fromJSON(file=paste("https://api.weather.gov/points/",lat,",",lon,sep=""))
    forecast_url<-endpoint$properties$forecast
    forecast_json <- fromJSON(file=forecast_url)
    forecastgrid_url<-endpoint$properties$forecastGridData
    forecastgrid_json <- fromJSON(file=forecastgrid_url)
  })

#####Station Analyzer Inputs
x <- getURL("https://raw.githubusercontent.com/m5loco/Divvy/master/new_stations_enhanced.csv")
ns <- read.csv(text = x)
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

get_ext_forecast_vals<-function(validTime,value,ext_method)
{
  hour<-validTime
  interval<-sub(".*/PT","",hour)                  ##parse interval from date time
  interval<-gsub("(.+?)(\\H.*)", "\\1", interval) ##remove hour
  interval<-as.integer(interval)                  ##set to integer
  interval<-interval-1
  
  temp_date<-as.POSIXct(hour,format="%Y-%m-%dT%H",tz=chi_tzone)
  temp_hour<-as.integer(format(temp_date,format='%H'))
  
  forecast_df<-NULL
  
  if (! is.na(temp_hour))
  {
    
    for(h in temp_hour:(temp_hour+interval))
    {
      if (h<24)
      {
        temp_short_date<-format(temp_date,format='%Y-%m-%d')
      }
      else
      {
        temp_short_date<-as.Date(format(temp_date,format='%Y-%m-%d'),tz=chi_tzone)+1
        temp_short_date<-format(temp_short_date,format='%Y-%m-%d')
        h=h-24
      }
      
      hr<-sprintf("%02d",h)
      hour_ext<-as.POSIXct(paste(temp_short_date,hr,sep=" "),format="%Y-%m-%d %H",tz=chi_tzone)
      
      if (ext_method=='naive')
      {
        value_ext<-value
        forecast<-data.frame(hour=hour_ext,value=value_ext)
      }
      else if (ext_method=='spread')
      {
        value_ext<-value/(interval+1)
        forecast<-data.frame(hour=hour_ext,value=value_ext)
      }
      
      if (! is.null(forecast_df)){ forecast_df=rbind(forecast_df,forecast) }
      else { forecast_df<-forecast }
      
    }
  }
  forecast_df
}

get_temp_forecast<-function(forecast_json)
{
  forecast_temps<-forecast_json$properties$temperature$values
  periods<-seq(1,length(forecast_temps))
  forecast_df<-NULL
  
  for (period in periods)
  {
    forecast<- get_ext_forecast_vals(forecast_temps[[period]]$validTime,celsius.to.fahrenheit(forecast_temps[[period]]$value),"naive")
    
    if (! is.null(forecast_df)){ forecast_df=rbind(forecast_df,forecast) }
    else { forecast_df<-forecast }
  }
  
  forecast_df
}

get_precip_forecast<-function(forecast_json)
{
  forecast_precips<-forecast_json$properties$quantitativePrecipitation$values
  periods<-seq(1,length(forecast_precips))
  forecast_df<-NULL
  
  for (period in periods)
  {
    forecast<- get_ext_forecast_vals(forecast_precips[[period]]$validTime,
                                     conv_unit(forecast_precips[[period]]$value,"cm","inch"),"spread")
    
    if (! is.null(forecast_df)){ forecast_df=rbind(forecast_df,forecast) }
    else { forecast_df<-forecast }
  }
  
  forecast_df
}

get_weather_forecast<-function(forecast_json)
{
  forecasts<-forecast_json$properties$periods
  periods<-seq(1,length(forecast_json$properties$periods))
  forecast_df<-NULL
  
  for(period in periods)
  {
    forecast<-data.frame(as.POSIXct(Sys.time(),tz=chi_tzone),lat=lat,lon=lon,
                         number=as.numeric(forecasts[[period]]$number),
                         name=forecasts[[period]]$name,
                         startTime=as.character(forecasts[[period]]$startTime),
                         endTime=as.character(forecasts[[period]]$endTime),   
                         isDaytime=forecasts[[period]]$isDaytime,
                         temperature=as.numeric(forecasts[[period]]$temperature),   
                         temperatureUnit=forecasts[[period]]$temperatureUnit,
                         temperatureTrend=as.factor(ifelse(is.null(forecasts[[period]]$temperatureTrend),
                                                           NA,forecasts[[period]]$temperatureTrend)),   
                         windSpeed=as.numeric(ifelse(forecasts[[period]]$windSpeed,
                                                     NA,forecasts[[period]]$windSpeed)),
                         windDirection=forecasts[[period]]$windDirection,
                         icon=forecasts[[period]]$icon,
                         shortForecast=forecasts[[period]]$shortForecast,
                         detailedForecast=as.character(forecasts[[period]]$detailedForecast))                            
    
    if (is.data.frame(forecast_df)) { forecast_df=rbind(forecast_df,forecast)}
    else { forecast_df<-forecast }
  }
  forecast_df
}


getForecasts<-function(input,temp_forecast,precip_forecast)
{
    if(as.character(input$StationInput)=="")
    {
       station_name<-default_station
    }
    else
    {
      station_name<-input$StationInput
    }

    in_date_str<-format(input$InputDate,format="%Y-%m-%d")
    
    pred_output<-NULL
    
    ###Inputs
    station<-top_stations_df[top_stations_df$station_name==station_name,"from_station_id"]
    
    if (!(length(input$StationInput)==0))
    {
      if (cst_date_str==in_date_str)
      {
        ridedate<-format(cst_time,format="%Y-%m-%d")
        start<-as.numeric(format(cst_time,format='%H'))
        start<-round(trunc(start)+1)
        start<-ifelse(start<6,6,start)
      }
      else
      {
        ridedate<-in_date_str
        start<-6
      }
      
      ##Get coefficients for station and pivot
      coeff<-subset(station_coeff_df[station_coeff_df$station_id==station,],select=c(coeff_names))
      pivoted_coeffs<-gather(coeff)
      pivoted_coeffs$value<-ifelse(is.na(pivoted_coeffs$value),0,pivoted_coeffs$value)
      
      for (h in (start:20))
      {   
        hr<-sprintf("%02d",h)
        trip_hour<-as.POSIXct(paste(ridedate,hr,sep=" "),format="%Y-%m-%d %H")
        
        day_of_week<-format(trip_hour,format='%a')
        hour<-format(trip_hour,format='%H')
        month<-format(trip_hour,format='%m')
        month<-ifelse(month<3|month>10,3,month)
        
        temp<-temp_forecast[temp_forecast$hour==get_GMT_time(trip_hour),"value"]
        precip<-precip_forecast[precip_forecast$hour==get_GMT_time(trip_hour),"value"]
        
        if (length(temp)==0){ temp<-as.numeric(cal_df[cal_df$cal_date==ridedate,'temp'])}
        if (length(precip)==0){ precip<-as.numeric(cal_df[cal_df$cal_date==ridedate,'precip'])}
        
        holiday<-as.integer(cal_df[cal_df$cal_date==ridedate,'holiday'])
        cubs<-as.integer(cal_df[cal_df$cal_date==ridedate,'cubs'])
        sox<-as.integer(cal_df[cal_df$cal_date==ridedate,'sox'])
        bears<-as.integer(cal_df[cal_df$cal_date==ridedate,'bears'])
        hawks<-as.integer(cal_df[cal_df$cal_date==ridedate,'hawks'])
        
        model_hour<-data.frame(X.Intercept.=1,
                               hour_temp_i=temp,
                               Sat=ifelse(day_of_week=='Sat',1,0),
                               Hour_16=ifelse(hour==16,1,0),
                               Hour_17=ifelse(hour==17,1,0),
                               Hour_15=ifelse(hour==15,1,0),
                               Hour_14=ifelse(hour==14,1,0),
                               Hour_18=ifelse(hour==18,1,0),
                               Hour_13=ifelse(hour==13,1,0),
                               Hour_12=ifelse(hour==12,1,0),
                               Hour_19=ifelse(hour==19,1,0),
                               Wed=ifelse(day_of_week=='Wed',1,0),
                               Hour_11=ifelse(hour==11,1,0),
                               Tue=ifelse(day_of_week=='Tue',1,0),
                               Thu=ifelse(day_of_week=='Thu',1,0),
                               Hour_20=ifelse(hour==20,1,0),
                               Mon=ifelse(day_of_week=='Mon',1,0),
                               Fri=ifelse(day_of_week=='Fri',1,0),
                               holiday=holiday,
                               hour_precip_i=precip,
                               Hour_10=ifelse(hour==10,1,0),
                               Oct=ifelse(month==10,1,0),
                               May=ifelse(month==5,1,0),
                               Apr=ifelse(month==4,1,0),
                               Sep=ifelse(month==9,1,0),
                               Mar=ifelse(month==3,1,0),
                               bears=bears,
                               sox=sox,
                               cubs=cubs,
                               Hour_9=ifelse(hour==9,1,0),
                               station_id=station,
                               Hour_7=ifelse(hour==7,1,0),
                               Hour_8=ifelse(hour==8,1,0),
                               bhawks=hawks)
        
        station_hour<-model_hour[,coeff_names]
        
        ##Pivot Row
        pivoted_hour<-gather(station_hour)
        pivoted_hour$value<-ifelse(is.na(pivoted_hour$value),0,pivoted_hour$value)
        
        ##Multiply hour values by coeffients
        sum_of_products<-sum(pivoted_hour$value*pivoted_coeffs$value)
        
        ##Exponate to get prediction
        pred<-exp(sum_of_products)
        pred_df<-data.frame(trip_hour=gsub("(?<![0-9])0+", "", format(trip_hour, format = "%I %p"), perl = TRUE),
                            riders=round(pred),temp=round(temp),precip=round(precip,3))
        
        if (is.null(pred_output)){ pred_output<-pred_df }
        else { pred_output<-rbind(pred_output,pred_df) }
      }
      pred_output
    }
}

weather_fcst<-get_weather_forecast(forecast_json)
weather_fcst<-get_weather_forecast(forecast_json)

temp_fcst<-get_temp_forecast(forecastgrid_json)
temp_fcst<-temp_fcst[! is.na(temp_fcst$hour),]
temp_fcst$hour<-temp_fcst$hour-offset*60*60

precip_fcst<-get_precip_forecast(forecastgrid_json)
precip_fcst<-precip_fcst[! is.na(precip_fcst$hour),]
precip_fcst$hour<-precip_fcst$hour-offset*60*60

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
  
  nsrows<-ns[ns$GEOID %in% geoids,c("BG_Tract","Median_Family_Income","est_female_25_29","est_male_25_29","Transp_betw2024",
                                    "Transp_Public_betw2024","Transp_Walked","HH_Nonfamily","HH_Alone","HH_Other")]
  nsrows[order(-nsrows$Median_Family_Income),]
  
}

body <- dashboardBody(
  tabItems(
    tabItem(tabName="demand",
            fluidRow(column(width=6,
                            box(width=NULL,
                                title = "Selection",solidHeader = TRUE,status = "primary",
                                img(src="https://github.com/m5loco/Divvy/raw/master/Divvy.jpg",height="25"),
                                titlePanel("Forecast Riders for:"),
                                dateInput("InputDate", "Date:",format = "mm/dd/yy",max="2020-05-31",
                                          min=gmt_date_str,value=gmt_date_str),
                                selectizeInput("StationInput","Station:",choices=NULL,selected=default_station),
                                htmlOutput("weather_forecast_image"),
                                htmlOutput("events")
                            )
            ),
            column(width=6,box(width=NULL,title = "Chart",solidHeader = TRUE,status = "primary",
                               plotOutput("ForecastPlot"),
            )
            ),
            ),
            fluidRow(
              column(width=6,box(width=NULL,
                                 title = "Map",solidHeader = TRUE,status = "primary",leafletOutput("myMap"))),
              column(width=6,
                     box(width=NULL,
                         title="Forecast",solidHeader = TRUE,status = "primary",
                         div(DT::dataTableOutput("tbl")),style="font-size:100%")
              )
            )
    ),
    tabItem(tabName="station",
            fluidRow(
              column(width=6,
                     box(width=NULL,status="primary",title="Selection",solidHeader = TRUE,
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
                     )
              ),
              column(width=6,
                     box(width=NULL,leafletOutput("sMap"),status="primary",title="Map",solidHeader = TRUE )
              )
            ),
            fluidRow(
              box(width=NULL,div(DT::dataTableOutput("stbl"),style="font-size:100%"),
                  title="ACS Information",solidHeader = TRUE,status="primary"
              )
            )
    )
  )
)

sidebar<-dashboardSidebar(sidebarMenu(id="container",
  menuItem(h4(strong("Hourly Predictions", align = "center")), tabName = "demand"),
  menuItem(h4(strong("New Station Finder", align = "center")), tabName = "station")))

ui <- dashboardPage(
  dashboardHeader(title = "Divvy Field Tools Dashboard"),
  sidebar,
  body
)

server <- function(input,output,session){
  
  updateSelectizeInput(session, 'StationInput',
                       choices = top_stations_df[order(top_stations_df$station_name),'station_name'],
                       server = TRUE, selected = default_station)
  
  output$tbl <- DT::renderDT(datatable(getForecasts(input,temp_fcst,precip_fcst),
                                      colnames=c("Start Hour","Predicted Riders","Predicted Temp (F)","Predicted Precip (in.)"),
                                      options = list(paging=TRUE,searching=FALSE,ordering=FALSE),rownames=FALSE,
                                      caption=paste(input$StationInput,": ",format(input$InputDate,format='%m/%d/%Y'),sep="")) %>%
                              formatStyle('riders',backgroundColor = 'lightyellow'))
  
  output$ForecastPlot<- renderPlot({
    df<-getForecasts(input,temp_fcst,precip_fcst)
    ggplot(data=df,aes(x=trip_hour,y=riders)) +
      geom_line(aes(group=1),linetype = "solid",size=2,color="darkgray") +
      geom_label(label=df$riders, nudge_x = 0.25, nudge_y = 0.25) +
      ylab("Riders") + xlab("Hour") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90,size=12),axis.title.x=element_blank(),legend.title=element_text(size=12),
            axis.text.y = element_blank(),axis.title.y = element_blank()) +
      ggtitle(paste(input$StationInput,' Forecast: ',format(input$InputDate,format="%m/%d/%Y"),sep=''))
  })
  
  output$myMap <- renderLeaflet({
    
      if (as.character(input$StationInput)=="")
      {
        station_name<-default_station
      }
      else
      {
        station_name<-input$StationInput
      }

      select_station<-top_stations_df[top_stations_df$station_name==station_name,]
      
      leaflet() %>%
        addTiles()%>%
        addMarkers(data=select_station,
                   ~longitude,
                   ~latitude,
                   label=~station_name,
                   labelOptions = labelOptions(noHide = T, direction = "bottom"),
                   group="myMarkers")
    
  })
  
  output$weather_forecast_image<-renderText({
    selected_date<-format(input$InputDate,format="%Y-%m-%d")
    selected_date_ob<-as.Date(selected_date)
    
    if (selected_date==cst_date_str){ fcst<-as.character(weather_fcst[1,'name']) }
    else if (selected_date<(cst_date+7)){ fcst<-format(selected_date_ob,format="%A")}
    else { fcst<-NA}
    
    if (!is.na(fcst))
    {
      filelink<-as.character(weather_fcst[weather_fcst$name==fcst,"icon"])
      fcsttext<-as.character(weather_fcst[weather_fcst$name==fcst,"detailedForecast"])
      c('<table cellpadding="10"><tr><td valign="top"><img src="',filelink,'"></td><td></tr>',
        '<tr><td valign="top"><h5><span style="color:red">Weather Forecast:</span>',fcsttext,'</h5></td></tr></table>')
    }
    else {c("Selected date is out of Weather Forecast range.")}
  })
  
  output$events<-renderText({
    eventhtml<-"<tr>"
    selected_date<-format(input$InputDate,format="%Y-%m-%d")
    
    if(cal_df[cal_df$cal_date==selected_date,'holiday']==1)
    {eventhtml<-c(eventhtml,c('<td><img src=',"https://github.com/m5loco/Divvy/raw/master/holiday.jpg",'  width=50></td>'))}
    
    if(cal_df[cal_df$cal_date==selected_date,'cubs']==1)
    {eventhtml<-c(eventhtml,c('<td><img src=',"https://github.com/m5loco/Divvy/raw/master/cubs.png",'  width=100></td>'))}
    
    if(cal_df[cal_df$cal_date==selected_date,'sox']==1)
    {eventhtml<-c(eventhtml,c('<td><img src=',"https://github.com/m5loco/Divvy/raw/master/sox.jpg",'  width=50></td>'))}
    
    if(cal_df[cal_df$cal_date==selected_date,'bears']==1)
    {eventhtml<-c(eventhtml,c('<td><img src=',"https://github.com/m5loco/Divvy/raw/master/bears.jpg",'  width=50></td>'))}
    
    if(cal_df[cal_df$cal_date==selected_date,'hawks']==1)
    {eventhtml<-c(eventhtml,c('<td><img src=',"https://github.com/m5loco/Divvy/raw/master/hawks.jpg",'  width=50></td>'))}
    
    eventhtml<-c(eventhtml,'</tr>')
    eventhtml
    
  })
  
  output$tbl_blk <- DT::renderDT(getTable(input$mdl,input$pct),
                                 colnames=c("Model","Predicted Successful Blocks"),
                                 options = list(paging=FALSE,searching=FALSE,dom = 't',ordering=FALSE),rownames=FALSE)
  
  sSub <-  reactive({
    getTable_Blocks(input$mdl,input$pct)
  })
  
  output$stbl <- DT::renderDT(datatable(sSub(),selection = "multiple",
                                       colnames=c("BlkGrp | Tract","Med Income","Female 25-29","Male 25-29",
                                       "Trans btw 20-24","Pub Trans btw 20-24","Walker","HH-Nonfamily","HH-Alone","HH-Other"),
                                       rownames=FALSE,
                                       options=list(paging=TRUE,searching=TRUE,ordering=TRUE,stateSave=TRUE)) 
                             %>% formatCurrency(c("Median_Family_Income"),"$",digits=0))
  
  observeEvent(input$stbl_rows_selected, {
    row_selected = sSub()[input$stbl_rows_selected,]
    pins<-ns[ns[,"BG_Tract"] %in% row_selected[,"BG_Tract"],]

    proxy <- leafletProxy('sMap')
    #print(row_selected)
    
    clearGroup(proxy,"myMarkers")
    
    proxy %>%
      addTiles()%>%
      addMarkers(
                 lng=pins$longitude,
                 lat=pins$latitude,
                 popup = paste("<table><tr><td>Block Group:</td><td>", pins$BlkGrp, "</td></tr>",
                               "<tr><td>Tract:</td><td>", pins$Tract, "</td></tr>",
                               "<tr><td>Median Income:</td><td>", paste0("$", formatC(as.numeric(pins$Median_Family_Income), format="f", digits=0, big.mark=",")), "</td></tr>",
                               "<tr><td>Age 25-29:</td><td>", pins$age_25_29, "</td></tr>",
                               "<tr><td>Public Trans 20-24 mins:</td><td>",pins$Transp_Public_betw2024,"</td></tr>",
                               "<tr><td>Walker:</td><td>",pins$Transp_Walked,"</td></tr>",
                               "<tr><td>HH-Other+:</td><td>", pins$HH_OtherPlus,"</td></tr></table>"),group="myMarkers")
  })
  
  
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
                   popup = paste("<table><tr><td>Block Group:</td><td>", pins$BlkGrp, "</td></tr>",
                                 "<tr><td>Tract:</td><td>", pins$Tract, "</td></tr>",
                                 "<tr><td>Median Income:</td><td>", paste0("$", formatC(as.numeric(pins$Median_Family_Income), format="f", digits=0, big.mark=",")), "</td></tr>",
                                 "<tr><td>Age 25-29:</td><td>", pins$age_25_29, "</td></tr>",
                                 "<tr><td>Public Trans 20-24 mins:</td><td>",pins$Transp_Public_betw2024,"</td></tr>",
                                 "<tr><td>Walker:</td><td>",pins$Transp_Walked,"</td></tr>",
                                 "<tr><td>HH-Other+:</td><td>", pins$HH_OtherPlus,"</td></tr></table>"),group="myMarkers")
    }
  }
  )

  observe({
    query <- parseQueryString(session$clientData$url_search)
    query1 <- paste(names(query), query, sep = "=", collapse=", ")
    #print(query1)
    if(query1 == "tab=station"){
      updateTabItems(session, inputId = "container", selected = "station")
    }})
}

shinyApp(ui, server)
