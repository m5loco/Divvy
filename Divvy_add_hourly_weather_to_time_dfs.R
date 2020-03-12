####
# Add Hourly Weather Conditions to Time-Based Data
# Time data is to the minute, weather data is hourly
####

library("sqldf")
library("dplyr")

setwd("C:/Bike Data/Dfs/")
ctypes<-rep("character",34)
all_df<-NA

filenames<-c("2016_Q1_TimeBasedDF_Bikes.csv","2016_Q2_TimeBasedDF_Bikes.csv","2016_Q3_TimeBasedDF_Bikes.csv","2016_Q4_TimeBasedDF_Bikes.csv",
            "2017_Q1_TimeBasedDF_Bikes.csv","2017_Q2_TimeBasedDF_Bikes.csv","2017_Q3_TimeBasedDF_Bikes.csv","2017_Q4_TimeBasedDF_Bikes.csv",
            "2018_Q1_TimeBasedDF_Bikes.csv","2018_Q2_TimeBasedDF_Bikes.csv","2018_Q3_TimeBasedDF_Bikes.csv","2018_Q4_TimeBasedDF_Bikes.csv",
            "2019_Q1_TimeBasedDF_Bikes.csv","2019_Q2_TimeBasedDF_Bikes.csv","2019_Q3_TimeBasedDF_Bikes.csv","2019_Q4_TimeBasedDF_Bikes.csv")

hourly_cols<-c("REPORT_TYPE","Date_","DATE","DATE_O","DATE_H","Year","Month","Day","Hour","Date_","HourlyAltimeterSetting","HourlyDewPointTemperature","HourlyDryBulbTemperature",
               "HourlyPrecipitation","HourlyPresentWeatherType","HourlyPressureChange","HourlyPressureTendency","HourlyRelativeHumidity",
               "HourlySeaLevelPressure","HourlySkyConditions","HourlyStationPressure","HourlyVisibility","HourlyWetBulbTemperature",
               "HourlyWindDirection","HourlyWindGustSpeed","HourlyWindSpeed")

weather_df<-read.csv("Weather Hourly, Chicago NOAA Weather Data.csv",colClasses=ctypes)

weather_df$DATE_O<-weather_df$DATE
weather_df$DATE<-gsub("T"," ",weather_df$DATE)
weather_df$DATE_H<-weather_df$DATE
weather_df$DATE<-as.POSIXct(weather_df$DATE,format=c("%Y-%m-%d %H"),tz="America/Chicago")
weather_df$Date_<-as.Date(weather_df$DATE,tz="")
weather_df$Year<-strftime(weather_df$DATE,'%Y')
weather_df$Month<-as.numeric(format(weather_df$DATE,format='%m'))
weather_df$Day<-as.numeric(format(weather_df$DATE,format='%d'))
weather_df$Hour<-as.numeric(format(weather_df$DATE,format='%H'))

weather_df<-weather_df[weather_df$REPORT_TYPE=='FM-15',]

##Delete Duplicates
weather_df<-weather_df %>% distinct(DATE, .keep_all = TRUE)

h_df<-subset(weather_df,select=hourly_cols)

ctypes<-rep("character",33)

for(file in filenames)
{
  
  print(file)
  
  t_df<-read.csv(file,colClasses=ctypes)
  
  if (file == "2016_Q1_TimeBasedDF_Bikes.csv"||file=="2016_Q2_TimeBasedDF_Bikes.csv")
  {
    fmt<-"%m/%d/%Y %H:%M"
  }
  else
  {
    fmt<-"%Y-%m-%d %H:%M"
  }

  t_df$DateTime_<-as.POSIXct(t_df$DateTime,tz="",format=c(fmt))
  t_df$DateTime<-as.POSIXct(t_df$DateTime,tz="",format=c(fmt))  
  t_df$Date_<-as.Date(t_df$DateTime_,tz="")
  t_df$Day<-as.numeric(format(t_df$DateTime_,format='%d'))

  t_plus_h_df<-sqldf("select distinct
        t.*,
         HourlyAltimeterSetting,
         HourlyDewPointTemperature,
         HourlyDryBulbTemperature,
         HourlyPrecipitation,
         HourlyPresentWeatherType,
         HourlyPressureChange,
         HourlyPressureTendency,
         HourlyRelativeHumidity,
         HourlySeaLevelPressure,
         HourlyStationPressure,
         HourlyVisibility,
         HourlyWetBulbTemperature,
         HourlyWindDirection,
         HourlyWindGustSpeed,
         HourlyWindSpeed 
      from t_df t
      left join h_df h
      on t.Date_ = h.Date_ and t.Hour = h.Hour")
  
  t_plus_h_df<- t_plus_h_df[ , !(names(t_plus_h_df) %in% c("DateTime_","Date_"))]
  
  #Delete Duplicates
  t_plus_h_df<- t_plus_h_df %>% distinct(DateTime, .keep_all = TRUE)
  
  t_plus_h_df$DateTime <- strftime(t_plus_h_df$DateTime,'%Y-%m-%d %H:%M')
  
  write.csv(t_plus_h_df,paste("h_",file,sep=""), row.names = FALSE)
  if (is.na(all_df))
  {
    all_df<-t_plus_h_df
  }else
  {
    all_df<-rbind(all_df,t_plus_h_df)
  }
}

write.csv(all_df,"h_AllData_2016-2019_TimeBasedDF.csv", row.names = FALSE)
