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
  broom
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
  'PageSize'='1000000',
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