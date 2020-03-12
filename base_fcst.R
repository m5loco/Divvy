library(forecast)
library(sqldf)
library(MASS)
library(xgboost)
library(pscl)

setwd("C:/Bike Data/")
# 
# model_vars<-c("count_i","hour_precip_i","Apr","May","Jun","Jul","Aug","Sep","Oct",
#               "Mon","Tue","Wed","Thu","Fri","Sat","Hour_7","Hour_8","Hour_9","Hour_10","Hour_11","Hour_12",
#               "Hour_13","Hour_14","Hour_15","Hour_16","Hour_17","Hour_18","Hour_19","Hour_20",
#               "holiday","cubs","sox","bears","bhawks")
# 
# model_vars<-c("count_i","hour_precip_i","hour_temp_i",
#               "Mon","Tue","Wed","Thu","Fri","Sat","Hour_7","Hour_8","Hour_9","Hour_10","Hour_11","Hour_12",
#               "Hour_13","Hour_14","Hour_15","Hour_16","Hour_17","Hour_18","Hour_19","Hour_20")
# 
# xreg_vars<-c("hour_temp_i","hour_precip_i","holiday")

###Helper function to bind together variables for use with ARIMA and XGBOOST
# bind_xvars<-function(df,vars)
# {
#   v<-NULL
#   for (var in vars)
#   {
#     #v<-cbind(v,paste(var,"=",df[,var],sep=""))
#     v<-cbind(v,df[,var])
#   }
#   as.matrix(v)
# }

##Load Data Files
top_df <- read.csv("top_df.csv")
top_stations_df<-read.csv("top_stations.csv")
stations_df<-read.csv("stations.csv")
top_events_df<-read.csv("top_events.csv")
top_df$trip_hour<-as.POSIXct(top_df$trip_hour,format=c("%Y-%m-%d %H:%M:%S"),tz="")

top_df$count_i<-top_df$count

top_df$NonPeak_Sun<-ifelse(((top_df$start_month==3)|(top_df$start_month==4)|(top_df$start_month==10))
                           &(top_df$start_day_of_week_no==0),1,0)

top_df$NonPeak_Mon<-ifelse(((top_df$start_month==3)|(top_df$start_month==4)|(top_df$start_month==10))
                           &(top_df$start_day_of_week_no==1),1,0)

top_df$NonPeak_Tue<-ifelse(((top_df$start_month==3)|(top_df$start_month==4)|(top_df$start_month==10))
                           &(top_df$start_day_of_week_no==2),1,0)

top_df$NonPeak_Wed<-ifelse(((top_df$start_month==3)|(top_df$start_month==4)|(top_df$start_month==10))
                           &(top_df$start_day_of_week_no==3),1,0)

top_df$NonPeak_Thu<-ifelse(((top_df$start_month==3)|(top_df$start_month==4)|(top_df$start_month==10))
                           &(top_df$start_day_of_week_no==4),1,0)

top_df$NonPeak_Fri<-ifelse(((top_df$start_month==3)|(top_df$start_month==4)|(top_df$start_month==10))
                           &(top_df$start_day_of_week_no==5),1,0)

top_df$NonPeak_Sat<-ifelse(((top_df$start_month==3)|(top_df$start_month==4)|(top_df$start_month==10))
                           &(top_df$start_day_of_week_no==6),1,0)

top_df$May_Sun<-ifelse((top_df$start_month==5)
                           &(top_df$start_day_of_week_no==0),1,0)

top_df$May_Mon<-ifelse((top_df$start_month==5)
                           &(top_df$start_day_of_week_no==1),1,0)

top_df$May_Tue<-ifelse((top_df$start_month==5)
                           &(top_df$start_day_of_week_no==2),1,0)

top_df$May_Wed<-ifelse((top_df$start_month==5)
                           &(top_df$start_day_of_week_no==3),1,0)

top_df$May_Thu<-ifelse((top_df$start_month==5)
                           &(top_df$start_day_of_week_no==4),1,0)

top_df$May_Fri<-ifelse((top_df$start_month==5)
                           &(top_df$start_day_of_week_no==5),1,0)

top_df$May_Sat<-ifelse((top_df$start_month==5)
                           &(top_df$start_day_of_week_no==6),1,0)


top_df$Sep_Sun<-ifelse((top_df$start_month==9)
                           &(top_df$start_day_of_week_no==0),1,0)

top_df$Sep_Mon<-ifelse((top_df$start_month==9)
                           &(top_df$start_day_of_week_no==1),1,0)

top_df$Sep_Tue<-ifelse((top_df$start_month==9)
                           &(top_df$start_day_of_week_no==2),1,0)

top_df$Sep_Wed<-ifelse((top_df$start_month==9)
                           &(top_df$start_day_of_week_no==3),1,0)

top_df$Sep_Thu<-ifelse((top_df$start_month==9)
                           &(top_df$start_day_of_week_no==4),1,0)

top_df$Sep_Fri<-ifelse((top_df$start_month==9)
                           &(top_df$start_day_of_week_no==5),1,0)

top_df$Sep_Sat<-ifelse((top_df$start_month==9)
                           &(top_df$start_day_of_week_no==6),1,0)


top_df$Peak_Sun<-ifelse(((top_df$start_month==6)|(top_df$start_month==7)|(top_df$start_month==8))
                        &(top_df$start_day_of_week_no==0),1,0)

top_df$Peak_Mon<-ifelse(((top_df$start_month==6)|(top_df$start_month==7)|(top_df$start_month==8))
                        &(top_df$start_day_of_week_no==1),1,0)

top_df$Peak_Tue<-ifelse(((top_df$start_month==6)|(top_df$start_month==7)|(top_df$start_month==8))
                        &(top_df$start_day_of_week_no==2),1,0)

top_df$Peak_Wed<-ifelse(((top_df$start_month==6)|(top_df$start_month==7)|(top_df$start_month==8))
                        &(top_df$start_day_of_week_no==3),1,0)

top_df$Peak_Thu<-ifelse(((top_df$start_month==6)|(top_df$start_month==7)|(top_df$start_month==8))
                        &(top_df$start_day_of_week_no==4),1,0)

top_df$Peak_Fri<-ifelse(((top_df$start_month==6)|(top_df$start_month==7)|(top_df$start_month==8))
                        &(top_df$start_day_of_week_no==5),1,0)

top_df$Peak_Sat<-ifelse(((top_df$start_month==6)|(top_df$start_month==7)|(top_df$start_month==8))
                        &(top_df$start_day_of_week_no==6),1,0)

top_df$hour_temp_lt50<-ifelse((top_df$hour_temp_i<50),1,0)
top_df$hour_temp_50s<-ifelse((top_df$hour_temp_i>=50)&(top_df$hour_temp_i<60),1,0)
top_df$hour_temp_60s<-ifelse((top_df$hour_temp_i>=60)&(top_df$hour_temp_i<70),1,0)
top_df$hour_temp_70s<-ifelse((top_df$hour_temp_i>=70)&(top_df$hour_temp_i<80),1,0)
top_df$hour_temp_80s<-ifelse((top_df$hour_temp_i>=80)&(top_df$hour_temp_i<90),1,0)
top_df$hour_temp_90plus<-ifelse((top_df$hour_temp_i>=90),1,0)

top_df$Mar<-ifelse((top_df$start_month==3),1,0)

top_df$Peak_Cubs<-ifelse(((top_df$start_month==6)|(top_df$start_month==7)|(top_df$start_month==8))
                        &(top_df$cubs==1),1,0)

top_df$NonPeak_Cubs<-ifelse(((top_df$start_month==4)|(top_df$start_month==5)|(top_df$start_month==9))
                        &(top_df$cubs==1),1,0)

# model_vars<-c("count_i","hour_precip_i","Hour_7","Hour_8","Hour_9","Hour_10","Hour_11","Hour_12",
#               "Hour_13","Hour_14","Hour_15","Hour_16","Hour_17","Hour_18","Hour_19","Hour_20",
#               "holiday","sox","bears","bhawks",
#               "NonPeak_Sun","NonPeak_Mon","NonPeak_Tue","NonPeak_Wed","NonPeak_Thu","NonPeak_Fri",
#               "May_Sun","May_Mon","May_Tue","May_Wed","May_Thu","May_Fri","May_Sat",
#               "Sep_Sun","Sep_Mon","Sep_Tue","Sep_Wed","Sep_Thu","Sep_Fri","Sep_Sat",
#               "Peak_Sun","Peak_Mon","Peak_Tue","Peak_Wed","Peak_Thu","Peak_Fri","Peak_Sat",
#               "hour_temp_50s","hour_temp_60s","hour_temp_70s","hour_temp_80s","hour_temp_90plus",
#               "Peak_Cubs","NonPeak_Cubs")

model_vars<-c("count_i","hour_precip_i","Mar","Apr","May","Sep","Oct","hour_temp_i",
              "Mon","Tue","Wed","Thu","Fri","Sat","Hour_7","Hour_8","Hour_9","Hour_10","Hour_11","Hour_12",
              "Hour_13","Hour_14","Hour_15","Hour_16","Hour_17","Hour_18","Hour_19","Hour_20",
              "holiday","cubs","sox","bears","bhawks")