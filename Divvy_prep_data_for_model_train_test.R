library(forecast)
library(imputeTS)
library(sqldf)
library(RSocrata)
library(skimr)
library(MASS)
library(xgboost)


### Extrapolate complete set of Trip Hours
trip_hrs<-seq(as.POSIXct("2016-1-1 0:00",tz=""), as.POSIXct("2019-12-31 23:00",tz=""), by = "hour")
trip_hours <- data.frame(trip_hour=trip_hrs)
trip_hours$year <- as.numeric(format(trip_hours$trip_hour,format='%Y'))
trip_hours$month <- as.numeric(format(trip_hours$trip_hour,format='%m'))
trip_hours$day <- as.numeric(format(trip_hours$trip_hour,format='%d'))
trip_hours$hour <- as.numeric(format(trip_hours$trip_hour,format='%H'))
trip_hours$day_of_week <- format(trip_hours$trip_hour,format='%a')
trip_hours$day_of_week_no <- as.numeric(format(trip_hours$trip_hour,format='%w'))

### Load Trip Data and add extra columns
cnames<-c("trip_id","start_time","end_time","bikeid","tripduration","from_station_id",
          "from_station_name","to_station_id","to_station_name","usertype","gender","birthyear","filename")

ctypes<-c("numeric","POSIXct","POSIXct","numeric","numeric","numeric","character","numeric",
          "character","factor","factor","numeric","character")

trips_all <- read.csv("C:/Bike Data/trips2016-2019.csv",col.names=cnames,colClasses=ctypes)
trips_all$start_date <- as.Date(trips_all$start_time)
trips_all$start_year <- as.numeric(format(trips_all$start_time,format='%Y'))
trips_all$start_month <- as.numeric(format(trips_all$start_time,format='%m'))
trips_all$start_day <- as.numeric(format(trips_all$start_time,format='%d'))
trips_all$start_hour <- as.numeric(format(trips_all$start_time,format='%H'))
trips_all$start_day_of_week <- format(trips_all$start_time,format='%a')
trips_all$start_day_of_week_no <- as.numeric(format(trips_all$start_time,format='%w'))

###Consolidate Trips down to hourly trips by Station
station_hourly_trips<-sqldf("select from_station_id,start_year,start_month,start_day,start_hour,
                            count(*) as count from trips_all group by 1,2,3,4,5 order by 1,2,3,4,5")

###Get List of Station Ids from Trips
station_ids<-sqldf("select from_station_id from trips_all group by 1 order by 1")

###Get a full set of hours for each station
station_months<-expand.grid(station_id=station_ids$from_station_id,trip_hour=trip_hrs)
station_months<-subset(station_months,select=c("station_id","trip_hour"))
station_months$start_year <- as.numeric(format(station_months$trip_hour,format='%Y'))
station_months$start_month <- as.numeric(format(station_months$trip_hour,format='%m'))
station_months$start_day <- as.numeric(format(station_months$trip_hour,format='%d'))
station_months$start_hour <- as.numeric(format(station_months$trip_hour,format='%H'))

###Get Top Stations
top_stations<-sqldf("select from_station_id,count(*) 
                    from trips_all where start_year=2019 group by 1 order by 2 desc limit 100")

###Add in additional info about stations
#stations<-read.socrata("https://data.cityofchicago.org/resource/bbyy-e7gq.json")
stations<-read.csv("C:/Bike Data/stations.csv")

type_by_station_df <-sqldf("select from_station_id,from_station_name,usertype,count(*) as count from trips_all
      where usertype <> 'Dependent' group by 1,2,3")

type_by_station_comp_df <-sqldf("select t1.from_station_id, t1.count as customer, t2.count as subscriber,
                                t1.count/t2.count as ratio from type_by_station_df t1,
                                type_by_station_df t2 where t1.from_station_id = t2.from_station_id and t1.usertype='Customer' 
                                and t2.usertype='Subscriber' order by 4 desc")

top_stations$cust_v_subs<-type_by_station_comp_df[match(top_stations$from_station_id,type_by_station_comp_df$from_station_id),"ratio"]
top_stations$station_name<-stations[match(top_stations$from_station_id,stations$id),'station_name']
top_stations$total_docks<-stations[match(top_stations$from_station_id,stations$id),'total_docks']
top_stations$station_name<-as.character(top_stations$station_name)

###Reduce Hourly Trip Data to peak months and top stations
top_station_trips<-sqldf("select * from station_hourly_trips 
                          where from_station_id in (select from_station_id from top_stations) 
                          and start_month >2 and start_month<11 order by from_station_id,start_year,
                         start_month, start_day,start_hour")

getFirstMondayofMarch<-function(yr)
{
  x <- seq(as.Date(paste(yr,"-03-01",sep="")), as.Date(paste(yr,"-03-31",sep="")), by = "day")
  x[weekdays(x) == "Monday" & as.numeric(format(x, "%d")) <= 7]
}

first_d_16<-as.numeric(format(as.Date(getFirstMondayofMarch(2016),"%Y-%d-%m"),format='%d'))
first_d_17<-as.numeric(format(as.Date(getFirstMondayofMarch(2017),"%Y-%d-%m"),format='%d'))
first_d_18<-as.numeric(format(as.Date(getFirstMondayofMarch(2018),"%Y-%d-%m"),format='%d'))
first_d_19<-as.numeric(format(as.Date(getFirstMondayofMarch(2019),"%Y-%d-%m"),format='%d'))

top_station_trips<-top_station_trips[!((top_station_trips$start_year==2016)&(top_station_trips$start_month==3)&
                                         (top_station_trips$start_day<first_d_16)),]
top_station_trips<-top_station_trips[!((top_station_trips$start_year==2017)&(top_station_trips$start_month==3)&
                                         (top_station_trips$start_day<first_d_17)),]
top_station_trips<-top_station_trips[!((top_station_trips$start_year==2018)&(top_station_trips$start_month==3)&
                                         (top_station_trips$start_day<first_d_18)),]
top_station_trips<-top_station_trips[!((top_station_trips$start_year==2019)&(top_station_trips$start_month==3)&
                                         (top_station_trips$start_day<first_d_19)),]

###Limit Possible Trip Hours to include only peak months
trip_hours$month<-as.numeric(format(trip_hours$trip_hour,format='%m'))
peak_hours<-trip_hours[trip_hours$month>2 & trip_hours$month<11,]

###Get all the possible combintations of stations and hours
top_station_hours<-expand.grid(station_id=top_stations$from_station_id,trip_hour=peak_hours$trip_hour)
top_station_hours<-subset(top_station_hours,select=c("station_id","trip_hour"))
top_station_hours$start_year <- as.numeric(format(top_station_hours$trip_hour,format='%Y'))
top_station_hours$start_month <- as.numeric(format(top_station_hours$trip_hour,format='%m'))
top_station_hours$start_day <- as.numeric(format(top_station_hours$trip_hour,format='%d'))
top_station_hours$start_hour <- as.numeric(format(top_station_hours$trip_hour,format='%H'))
top_station_hours$start_day_of_week_no <- as.numeric(format(top_station_hours$trip_hour,format='%w'))

top_station_hours<-top_station_hours[!((top_station_hours$start_year==2016)&(top_station_hours$start_month==3)&
                                         (top_station_hours$start_day<first_d_16)),]
top_station_hours<-top_station_hours[!((top_station_hours$start_year==2017)&(top_station_hours$start_month==3)&
                                         (top_station_hours$start_day<first_d_17)),]
top_station_hours<-top_station_hours[!((top_station_hours$start_year==2018)&(top_station_hours$start_month==3)&
                                         (top_station_hours$start_day<first_d_18)),]
top_station_hours<-top_station_hours[!((top_station_hours$start_year==2019)&(top_station_hours$start_month==3)&
                                         (top_station_hours$start_day<first_d_19)),]

top_df<-sqldf("select
         sh.station_id,
         sh.trip_hour,
         sh.start_year,
         sh.start_month,
         sh.start_day,
         sh.start_day_of_week_no,
         sh.start_hour,
         sht.count
      from top_station_hours sh
      left join top_station_trips sht 
      on sh.station_id = sht.from_station_id 
         and sh.start_year = sht.start_year 
         and sh.start_month = sht.start_month
         and sh.start_day = sht.start_day
         and sh.start_hour = sht.start_hour")

top_df$count[is.na(top_df$count)]<-0
top_df$count_i_flg<-ifelse(top_df$count>50,1,0)
top_df$count_i<-ifelse(top_df$count>50,50,top_df$count)  #####25 is fairly arbitrary

events_df<-read.csv("C:/Bike Data/Dfs/h_AllData_2016-2019_TimeBasedDF.csv",stringsAsFactors = FALSE)
events_df<-events_df[(! is.na(events_df$DateTime)),]
events_df$DateTime<-as.POSIXct(events_df$DateTime,format=c("%Y-%m-%d %H:%M"),tz="")
events_df<-events_df[(events_df$Month>2) & (events_df$Month<11),]
events_df$Min<-as.numeric(format(events_df$DateTime,format='%M'))
events_df<-events_df[(events_df$Min==0),]
events_df$HourlyDryBulbTemperature<-as.numeric(events_df$HourlyDryBulbTemperature)
events_df$HourlyPrecipitation<-as.numeric(events_df$HourlyPrecipitation)
events_df$HourlyPresentWeatherType<-as.factor(events_df$HourlyPresentWeatherType)

###Cleanup Memory
rm(station_months)
rm(top_station_hours)
rm(station_hourly_trips)
rm(station_ids)
rm(peak_hours)
#rm(trips_all)
rm(trip_hours)
rm(top_station_trips)
rm(type_by_station_comp_df)
rm(type_by_station_df)
rm(stations)
gc()

###Add Weather Data
bkup_top_df<-top_df
top_df<-sqldf("select 
                 t.*,
                 e.HourlyDryBulbTemperature as hour_temp,
                 e.HourlyPrecipitation as hour_precip,
                 e.Holiday_Binary as holiday,
                 e.Cubs_binary as cubs,
                 e.WhiteSox_binary as sox,
                 e.Bears_binary as bears,
                 e.Blackhawks_binary as bhawks
               from
                 top_df t,
                 events_df e
               where
                 e.DateTime = t.trip_hour
               order by t.trip_hour")

###Impute NAs to last recorded value for temp and precipitation
imp_col<-paste("hour_temp",sep='')
p_ts<-ts(top_df[,"hour_temp"])
top_df[,"hour_temp_i"]<-na_locf(p_ts,option="locf")
top_df[is.na(top_df$hour_temp_i),"hour_temp_i"]<-top_df$hour_temp_i

imp_col<-paste("hour_precip",sep='')
p_ts<-ts(top_df[,"hour_precip"])
top_df[,"hour_precip_i"]<-na_locf(p_ts,option="locf")
top_df[is.na(top_df$hour_precip_i),"hour_precip_i"]<-top_df$hour_precip_i

###Add Customer to Subscriber Ratio and Docks, add station name for bearing
top_df<-sqldf("select t.*,
                 s.station_name,
                 s.total_docks,
                 s.cust_v_subs
               from
                 top_df t,
                 top_stations s
               where
                 t.station_id = s.from_station_id
               order by t.station_id,t.trip_hour")

### Add month binaries
#top_df$Mar<-ifelse(top_df$start_month==3,1,0)
top_df$Apr<-ifelse(top_df$start_month==4,1,0)
top_df$May<-ifelse(top_df$start_month==5,1,0)
top_df$Jun<-ifelse(top_df$start_month==6,1,0)
top_df$Jul<-ifelse(top_df$start_month==7,1,0)
top_df$Aug<-ifelse(top_df$start_month==8,1,0)
top_df$Sep<-ifelse(top_df$start_month==9,1,0)
top_df$Oct<-ifelse(top_df$start_month==10,1,0)

## Add day_of_week binaries
#top_df$Sun<-ifelse(top_df$start_day_of_week_no==0,1,0)
top_df$Mon<-ifelse(top_df$start_day_of_week_no==1,1,0)
top_df$Tue<-ifelse(top_df$start_day_of_week_no==2,1,0)
top_df$Wed<-ifelse(top_df$start_day_of_week_no==3,1,0)
top_df$Thu<-ifelse(top_df$start_day_of_week_no==4,1,0)
top_df$Fri<-ifelse(top_df$start_day_of_week_no==5,1,0)
top_df$Sat<-ifelse(top_df$start_day_of_week_no==6,1,0)

## Remove Overnight Data
top_df<-top_df[! top_df$start_hour>20,]
top_df<-top_df[! top_df$start_hour<6,]

## Add hourly_binaries
#top_df$Hour_6 <- ifelse(top_df$start_hour==6,1,0)
top_df$Hour_7 <- ifelse(top_df$start_hour==7,1,0)
top_df$Hour_8 <- ifelse(top_df$start_hour==8,1,0)
top_df$Hour_9 <- ifelse(top_df$start_hour==9,1,0)
top_df$Hour_10 <- ifelse(top_df$start_hour==10,1,0)
top_df$Hour_11 <- ifelse(top_df$start_hour==11,1,0)
top_df$Hour_12 <- ifelse(top_df$start_hour==12,1,0)
top_df$Hour_13 <- ifelse(top_df$start_hour==13,1,0)
top_df$Hour_14 <- ifelse(top_df$start_hour==14,1,0)
top_df$Hour_15 <- ifelse(top_df$start_hour==15,1,0)
top_df$Hour_16 <- ifelse(top_df$start_hour==16,1,0)
top_df$Hour_17 <- ifelse(top_df$start_hour==17,1,0)
top_df$Hour_18 <- ifelse(top_df$start_hour==18,1,0)
top_df$Hour_19 <- ifelse(top_df$start_hour==19,1,0)
top_df$Hour_20 <- ifelse(top_df$start_hour==20,1,0)

setwd("C:/Bike Data/")
write.csv(top_df,"top_df.csv", row.names = FALSE)
write.csv(top_stations,"top_stations.csv", row.names = FALSE)
write.csv(events_df,"top_events.csv",row.names=FALSE)
