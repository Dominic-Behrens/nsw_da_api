#Author: Dominic Behrens
#Purpose: Access NSW DPHI DA API to get and clean a .csv of DAs in NSW

#load and install required packages
if(!require("pacman",character.only = T)) install.packages("pacman")

pacman::p_load(
  tidyverse,
  magrittr,
  janitor,
  httr,
  tibble,
  RCurl,
  jsonlite,
  rvest,
  broom,
  purrr
)

#basic setup
gc()
rm(list=ls())
options(scipen = 999)

#set up response parameters and get response data----
#change parameters here if you'd like to update/look at a specific set of development
#note that here I only pull developments >$5m to look at only large-ish projects
#if you change this, check that your final output has fewer rows than the PageSize variable
#so you haven't missed any.
#note also that this doesn't include CDCs- that's a separate API. 

#define headers
headers<-c(
  'PageSize'='10000',
  'PageNumber'='1',
  'filters'='{ "filters": {"CostOfDevelopmentFrom":5000000,
  "ApplicationType":"Development Application","DevelopmentCategory":"Residential"}}'
)

#run request and get content from the response
api_response<-VERB("GET",
                   url="https://api.apps1.nsw.gov.au/eplanning/data/v0/OnlineDA",
                   add_headers(headers))%>%
 content(as='parsed',
         type='application/json')

#get details
details<-api_response$Application


#tidy up downloaded data----
#function to unnest nested lists, remove unnecessary columns and generally clean up 
#the applications data, then convert to a data.frame for saving or further analysis.  
clean_output<-function(data){
  for(i in seq_along(data)){
    #remove 'lot' list from each location list- causes problems
    data[[i]]$Location<-lapply(data[[i]]$Location, function(loc) {
      loc$Lot <-NULL
      return(loc)
    })
    #drop various unnecessary datapoints
    data[[i]]$VPAStatus<-NULL
    #ensure council is formatted properly
    if(is.list(data[[i]]$Council)){
      data[[i]]$Council<-data[[i]]$Council$CouncilName
    }
    #keep only the first location (this might need to be changed for big subdivisions
    if(length(data[[i]]$Location)>1){
      data[[i]]$Location<-list(data[[i]]$Location[[1]])
    }  
    #concatenate DevelopmentType
    if(is.list(data[[i]]$DevelopmentType)){
      concatenated_types<-paste(sapply(data[[i]]$DevelopmentType,function(dt) dt$DevelopmentType),
                                collapse="-")
      data[[i]]$DevelopmentType<-concatenated_types
    }
  }
#convert to a data.frame
  data_df<-map_dfr(data,as_tibble)%>%
    unnest_wider(Location)%>%
    #remove subdivision type if present, causes issues with saving as csv. 
    select(-any_of('SubdivisionType'))
return(data_df)
}

#clean data
clean_data<-clean_output(details)

#save output
write.csv(clean_data,'./Data/DAs_over_5_million.csv')
