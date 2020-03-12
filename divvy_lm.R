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
  station_train_df$count_i<-station_train_df$count
  station_train_df<-subset(station_train_df,select=model_vars)
  
  ###Full LM Model
  full.lm <- lm(count_i ~ . ,data=station_train_df)
  full_preds<-predict(full.lm,newdata=station_test_df)
  test_results_df[test_results_df$station_id==station_id,"full.lm"]<-full_preds
  
  ###Forward Selection Model
  lower.lm <- lm(count_i ~ 1,data=station_train_df)
  forward.lm <- stepAIC(object=lower.lm,scope=list(upper=full.lm,lower=lower.lm),direction=c('forward'),trace=0);
  forward_preds<-predict(forward.lm,newdata=station_test_df)
  test_results_df[test_results_df$station_id==station_id,"forward.lm"]<-forward_preds
  
  # ###Backward Selection Model
  backward.lm <- stepAIC(object=full.lm,direction=c('backward'),trace=0);
  backward_preds<-predict(backward.lm,newdata=station_test_df)
  test_results_df[test_results_df$station_id==station_id,"backward.lm"]<-backward_preds
  # 
  # ###Stepwise Selection Model
  temp.lm <- lm(count_i ~ hour_temp_i,data=station_train_df)
  stepwise.lm <- stepAIC(object=temp.lm,scope=list(upper=formula(full.lm),lower=~1),direction=c('both'),trace=0);
  stepwise_preds<-predict(stepwise.lm,newdata=station_test_df)
  test_results_df[test_results_df$station_id==station_id,"stepwise.lm"]<-stepwise_preds

  ###GLM Models - Use coefficients selected by stepwise selection
  glm_vars<-rownames(summary(forward.lm)$coefficients)
  glm_vars<-glm_vars[!glm_vars=='(Intercept)']
  
  ###Poisson
  poisson.glm <- glm(count_i ~ ., family="poisson"(link="log"), data=subset(station_train_df,select=c("count_i",glm_vars)))
  poisson_preds <- predict(poisson.glm, newdata = station_test_df, type = "response")
  test_results_df[test_results_df$station_id==station_id,"poisson.glm"]<-poisson_preds
  
  write.csv(test_results_df,"c:/Users/Mark/documents/school/MSDS 498/fcst/lm_2019_preds.csv", row.names = FALSE)
  
  # test_results_df[test_results_df$station_id==station_id,"poisson.glm"]<-poisson_preds
  
  ###Zero Inflated
  #zip.glm <-zeroinfl(count_i ~ ., data=subset(station_train_df,select=c("count_i",glm_vars)))
  #zip_preds <- predict(zip.glm, newdata = station_test_df, type = "response")
  #test_results_df[test_results_df$station_id==station_id,"zip.glm"]<-zip_preds
}



