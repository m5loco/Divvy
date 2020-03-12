source("c:/Users/Mark/documents/school/MSDS 498/fcst/base_fcst.R")

test_results_df<-subset(top_df[top_df$start_year==2019,],
                        select=c("station_id","trip_hour"))

stations<-top_stations_df$from_station_id

for (station_id in stations)
{
  print(station_id)
  station_trips_df<-top_df[top_df$station_id == station_id,]
  station_train_df<-station_trips_df[station_trips_df$start_year<2019,]
  station_test_df<-station_trips_df[station_trips_df$start_year==2019,]
  station_train_df<-subset(station_train_df,select=model_vars)
  
  xreg_vars<-c("hour_temp_i","hour_precip_i","holiday")
  station_train_ts<-ts(station_train_df$count_i,frequency = 15)
  station_train_xreg<-bind_xvars(station_train_df,xreg_vars)
  station_test_xreg<-bind_xvars(station_test_df,xreg_vars)
  
  ###Forecast Package TS Models
  print("arima")  
  arima.ts<-auto.arima(station_train_ts,xreg=station_train_xreg)
  arima_preds<-forecast(arima.ts,h=nrow(station_test_df),xreg=station_test_xreg)$mean[]
  test_results_df[test_results_df$station_id==station_id,"arima.ts"]<-arima_preds
  
  write.csv(test_results_df,"c:/Users/Mark/documents/school/MSDS 498/fcst/nnetar_2019_preds.csv", row.names = FALSE)
}



