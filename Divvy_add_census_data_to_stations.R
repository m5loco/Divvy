############################
# Extract acs attributes for a Divvy Station's geographical block group and 
# export to csv along with station attributes
#
# Author: Mark Loula
# Date: February 4, 2020
#
# # Extract estimates from the census.gov american community survey (acs) in 2017 because it is newer
# 
# # Extract stations from the Chicago Data Portal
# # Use latitude and longitude to look up geolocator for the station
# # Extract list of variables available from acs
# # Extract values for the gender variables for Cook County, IL (data only available for block groups)
# # Use geolocator to match data for each station
# # Add variables to stations data frame, renaming with descriptive name
#
# # Census call is also included, but commented out below
#
#############################
library(censusr)
library(tidycensus)
library(tidyverse)
library(RSocrata)

setwd("C:/Bike Data/")

CENSUS_API_KEY="XXXXXXXXXXXXXXX"  ### Get your own API key from https://api.census.gov/data/key_signup.html, replace the X's and uncomment
census_api_key(CENSUS_API_KEY)


### Function to add census data point to stations dataframe
addCensusVar<-function(station_df,census_df,station_var,census_var)
{
  station_df[,station_var]<-census_df[census_df$variable==census_var,][match(substr(station_df$geolocator,1,12),
                                                                             census_df[census_df$variable==census_var,]$GEOID),'estimate']
  station_df
}

###Extract list of variables available for 2017 ACS (American Community Survey) and write to a file
v17<-load_variables(2017, "acs5", cache = TRUE)
write.csv(v17,"acs_vars_v17.csv")

###Read stations from Chicago Data Portal
stations<-read.socrata("https://data.cityofchicago.org/resource/bbyy-e7gq.json") 

###Use function to look up geolocator for latitude and longitude values provided for each station
stations$geolocator<-mapply(call_geolocator_latlon,stations$latitude,stations$longitude)
write.csv(stations,"stations.csv")

###Get gender estimates from the american community survey (https://api.census.gov/data/2018/acs/acs1/variables.html) for Cook County IL

###B01001_001-B01001_049 are gender variables
suffixes <- seq(1,49)
vars<-sprintf("B01001_%03d", suffixes)

###Also get, B19013_001E which is median income
vars<-c("B19013_001",vars)

census_info<-get_acs(geography = "block group", variables = vars,state="IL",county="Cook",year=2017)

###Alternate call for census data
########census_info<-get_decennial(geography = "block", variables = "H043A001", year = 2010, state="IL",county="Cook")

###Add block group variable for each station
stations[,'block_group']<-census_info[census_info$variable=='B01001_001',][match(substr(stations$geolocator,1,12),
                                                                           census_info[census_info$variable=='B01001_001',]$GEOID),'NAME']
###Add total populatation variable for the block group in each station
stations<-addCensusVar(stations,census_info,'est_total_pop','B01001_001')

###Iterate through age group totals
age_groups<-c('total','under_5','5_to_9','10_to_14','15_to_17','18_to_19','20','21','22_24','25_29','30_34','35_to_39','40_to_44','45_to_49','50_to_54',
              '55_59','60_to_61','62_to_64','65_to_66','67_to_69','70_to_74','75_to_79','80_to_84','85_plus')

###Add male variables
suffixes <- seq(2,25)
vars<-sprintf("B01001_%03d", suffixes)

for (i in seq(1,24))
{
  stations<-addCensusVar(stations,census_info,paste('est_male_',age_groups[i],sep=''),vars[i])
}

###Add female variables
suffixes <- seq(26,49)
vars<-sprintf("B01001_%03d", suffixes)

for (i in seq(1,24))
{
  stations<-addCensusVar(stations,census_info,paste('est_female_',age_groups[i],sep=''),vars[i])
}

###Add Median Income
stations<-addCensusVar(stations,census_info,'est_median_inc','B19013_001')

### write data frame to csv
write.csv(stations,"stations_with_census_data.csv")

