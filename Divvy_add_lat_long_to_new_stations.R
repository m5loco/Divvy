############################
# Add longitude and latitude to proposed Divvy Station Locations
# export to csv along with station attributes
#
# Author: Mark Loula
# Date: February 2020
#############################
library(censusr)
library(tidycensus)
library(tidyverse)
library(RSocrata)
library(rvest)
library(readxl)
library(sqldf)

census_api_key("2da2d5dae78fe3b07f9d80d9ab75fbc1c2e18542")

setwd("c:/users/Mark/Documents/School/MSDS 498/fcst/")

####Get Geo Id's for Illinois Block Groups
block_groups<- as.data.frame(read_html("https://tigerweb.geo.census.gov/tigerwebmain/Files/bas20/tigerweb_bas20_blkgrp_il.html")
                             %>% html_table(fill=TRUE))
block_groups$GEOID<-as.character(block_groups$GEOID)

####Get block groups from Cook county
vars<-c("B19013_001") ####Pull back one var
census_info<-get_acs(geography = "block group", variables = vars,state="IL",county="Cook",year=2017)

####Read in block groups from station classifier file
new_stations <- read_excel("Divvy_new_station_locations_v2.xlsx",sheet="new_station_locations",
                           col_names=c("Row_num","Name","tree_No","tree_Yes","Decision_Tree","forest_No","forest_Yes",
                                       "Random_Forest","bayes_No","bayes_Yes","Naive_Bayes","new_blocks_xgboost","XGBoost","Agree",
                                       "Median_Family_Income","Transp_Public_betw2024","Transp_Walked","est_female_25_29","HH_Nonfamily",
                                       "Transp_Walked_betw1519","Transp_betw2024","HH_Other","HH_Alone","est_male_25_29"),skip=1)

####Add geo id to new stations joining on block group name
new_stations<-sqldf("select 
                       c.GEOID,
                       ns.*
                    from
                      census_info c,
                      new_stations ns
                    where
                      c.NAME=ns.Name")

####Add Lat and Long to new stations joining on geo id
new_stations<-sqldf("select 
         ns.*,
         bg.CENTLAT,
         bg.CENTLON
       from
         new_stations ns,
         block_groups bg
       where
         ns.GEOID = bg.GEOID
        order by ns.Row_num")

####Write to 
write.csv(new_stations,"new_stations_enhanced.csv",row.names=FALSE)