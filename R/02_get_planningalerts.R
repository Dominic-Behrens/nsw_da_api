#Author: Dominic Behrens
#Purpose: Get planningalerts data for list of active DAs taken from the DPHI api

#load and install required packages
if(!require("pacman",character.only = T)) install.packages("pacman")

pacman::p_load(
  tidyverse,
  magrittr,
  janitor,
  tibble,
  sf,
  httr
)

#basic setup
gc()
rm(list=ls())
options(scipen = 999)

#read in API Key for planning alerts- sensitive so manually select using file.choose
api_key<-as.character(read.table(file.choose()))

#read in active DAs from previous script and filter to those that have at least 10 dwellings
active_das<-read.csv('./Outputs/Active_DAs.csv')%>%
  clean_names()%>%
  select(-c('x_2','x_1'))%>%
  filter(number_of_new_dwellings>=10)%>%
  arrange(desc(number_of_new_dwellings))

#set up function to run planningalerts API call
#and return project details and planningalerts ID to make a submission on

call_api<-function(lat,lon,radius,api_key){
  #set up url 
url_base<-"https://api.planningalerts.org.au/applications.json?key="
url_full<-paste0(url_base,api_key,'&lat=',lat,'&lng=',lon,'&radius=',radius)
  #run API call
api_response<-GET(url_full)%>%
  content(as='parsed')
  #clean into output dataframe
out_frame<-data.frame()
if(length(api_response>0)){
out_frame[1,1:4]<-c(api_response[[1]]$application$id,
                    api_response[[1]]$application$description,
                    api_response[[1]]$application$date_received,
                    api_response[[1]]$application$authority$full_name)
}else{
out_frame[1,1:4]<-rep(NA,4)
}
colnames(out_frame)<-c('pa_id','pa_description','pa_date','pa_determination_authority')
return(out_frame)
}

#Get Outputs by looping over active DAs and pulling data
#init output dataframe
active_das_w_pa<-data.frame()
for(da_num in active_das$planning_portal_application_number){
  #filter to relevant DA
  temp_da<-active_das%>%
    filter(planning_portal_application_number==da_num)
  #get output
  pa_resp_temp<-call_api(temp_da$y,temp_da$x,50,api_key)
  #join to output dataframe
  temp_out<-cbind(temp_da,pa_resp_temp)
  active_das_w_pa%<>%rbind(temp_out)
  rm(temp_da,temp_out,pa_resp_temp)
}

#clean up output to make it easier to use
clean_das<-active_das_w_pa%>%
  filter(!is.na(pa_id))%>%
  mutate(pa_url=paste0('planningalerts.org.au/applications/',pa_id))%>%
  select(c(pa_description,number_of_new_dwellings,number_of_storeys,pa_url,pa_determination_authority,application_status,council,full_address,x,y))

#write to a .csv for cleaning
clean_das%>%
  write.csv('./Outputs/clean_das_w_planningalerts.csv')

