source("c:/users/Mark/Documents/School/MSDS 498/fcst/base_fcst.R")
library("plyr")
library("dplyr")

build_coef_df <- function(station_id,coefs,df)
{
    if (is.null(df))
    {
      df<-data.frame(as.list(coefs))
      df$station_id<-station_id
    }
    else
    {
      new_df<-data.frame(as.list(coefs))
      new_df$station_id<-station_id
      df<-rbind.fill(df,new_df)  
    }
  df
}

stations<-top_stations_df$from_station_id
mdl_coeff<-NULL

for (station_id in stations)
{
  print(station_id)
  station_train_df<-top_df[top_df$station_id == station_id,]
  
  # For testing....
  # station_train_df<-subset(station_train_df[station_train_df$start_year<2019,],select=model_vars)
  
  station_train_df<-subset(station_train_df,select=model_vars)
  
  ###Full LM Model
  full.lm <- lm(count_i ~ . ,data=station_train_df)
  
  ###Forward Selection Model
  lower.lm <- lm(count_i ~ 1,data=station_train_df)
  forward.lm <- stepAIC(object=lower.lm,scope=list(upper=full.lm,lower=lower.lm),direction=c('forward'),trace=0);
  
  ###GLM Models - Use coefficients selected by forward selection
  glm_vars<-rownames(summary(forward.lm)$coefficients)
  glm_vars<-glm_vars[!glm_vars=='(Intercept)']
  
  ###Poisson
  poisson.glm <- glm(count_i ~ ., family="poisson"(link="log"), data=subset(station_train_df,select=c("count_i",glm_vars)))
  mdl_coeff<-build_coef_df(station_id,poisson.glm$coefficients,mdl_coeff)
  write.csv(mdl_coeff,"c:/users/Mark/Documents/School/MSDS 498/fcst/2020_top_station_coefficients.csv",row.names=FALSE)
}



