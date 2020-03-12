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
  #arima.ts<-auto.arima(station_train_ts,xreg=station_train_xreg)
  #arima_preds<-forecast(arima.ts,h=nrow(station_test_df),xreg=station_test_xreg)$mean[]
  #test_results_df[test_results_df$station_id==station_id,"arima.ts"]<-arima_preds
  
  #nnetar.ts<-nnetar(station_train_ts,xreg=station_train_xreg)
  #nnetar_preds<-forecast(nnetar.fit,h=nrow(station_test_df),xreg=station_test_xreg)$mean[]
  #test_results_df[test_results_df$station_id==station_id,"nnetar.ts"]<-nnetar_preds
  
  ###XG Model - Use coefficients selected by forward selection
  full.lm <- lm(count_i ~ . ,data=station_train_df)
  lower.lm <- lm(count_i ~ 1,data=station_train_df)
  forward.lm <- stepAIC(object=lower.lm,scope=list(upper=full.lm,lower=lower.lm),direction=c('forward'),trace=0);
  xg_vars<-rownames(summary(forward.lm)$coefficients)
  xg_vars<-xg_vars[!xg_vars=='(Intercept)']
  
  station_train_xgvars<-bind_xvars(station_train_df,xg_vars)
  station_test_xgvars<-bind_xvars(station_test_df,xg_vars)
  
  xg.ts <- xgboost(data = xgb.DMatrix(data = station_train_xgvars, label= station_train_ts), # the data  
                    nround = 10000, # max number of boosting iterations
                    objective = "count:poisson",print_every_n=5000,early_stopping_rounds=1000)
  xg_preds<- predict(xg.ts,xgb.DMatrix(data = station_test_xgvars))
  test_results_df[test_results_df$station_id==station_id,"xg.ts"]<-xg_preds
  write.csv(test_results_df,"c:/Users/Mark/documents/school/MSDS 498/fcst/xg_2019_preds.csv", row.names = FALSE)
}



