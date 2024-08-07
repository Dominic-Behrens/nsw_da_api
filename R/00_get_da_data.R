#Author: Dominic Behrens
#Purpose: Access NSW DPHI DA API to clean a .csv of all DAs in NSW

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
#change parameters here if you'd like to update/lookk at a specific set of dvelopment
#note that here I only pull developments >$200k to filter out alts/adds and small projects
#if you change this, check that your final output has fewer rows than the PageSize variable
#so you haven't missed any.
#note also that this doesn't include CDCs- that's a separate API. 

#define headers
headers<-c(
  'PageSize'='100',
  'PageNumber'='1',
  'filters'='{ "filters": {"CostOfDevelopmentFrom":200000,
  "ApplicationType":"Development Application"} }'
)

#run request
res<-VERB("GET",url="https://api.apps1.nsw.gov.au/eplanning/data/v0/OnlineDA",add_headers(headers))

#get content from response
content<-content(res,as='parsed',type='application/json')

#get details
details<-content$Application


#tidy up downloaded data----
#first, remove 'lot' list from each element (problematic)

for (i in seq_along(details)){
  details[[i]]$Location<-lapply(details[[i]]$Location, function(loc) {
    loc$Lot <-NULL
    return(loc)
  })
}
#unnest nested lists and dataframe from the applications data

clean_output<-function(data){
  for(i in seq_along(data)){
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
return(data)
}

#clean data
clean_data<-clean_output(details)

#convert this to a df
das_df<-map_dfr(clean_data,as_tibble)%>%
  unnest_wider(Location)%>%
  unnest_wider(SubdivisionType)

#save output
write.csv(das_df,'./Data/da_example.csv')
