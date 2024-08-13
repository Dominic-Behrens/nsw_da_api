#Author: Dominic Behrens
#Purpose: Filter DA data to only those on exhibition

#load and install required packages
if(!require("pacman",character.only = T)) install.packages("pacman")

pacman::p_load(
  tidyverse,
  magrittr,
  janitor,
  tibble,
  tmap,
  sf
)

#basic setup
gc()
rm(list=ls())
options(scipen = 999)
tmap_mode('view')

#read in data
das_df<-read.csv('./Data/DAs_over_5_million.csv')


#filter to those with an exhibition end date after today's date
today_date<-Sys.time()
active_das<-das_df%>%
  mutate(AssessmentExhibitionEndDate=as.POSIXct(AssessmentExhibitionEndDate))%>%
  filter(AssessmentExhibitionEndDate>today_date)


#put on map
active_da_map<-active_das%>%
  st_as_sf(coords=c('X','Y'))%>%
  tm_shape()+
  tm_sf(col='NumberOfNewDwellings',size=0.001,id="FullAddress",popup.vars=c('NumberOfNewDwellings','PlanningPortalApplicationNumber','NumberOfStoreys','Council'))

#save outputs
write.csv(active_das,'./Outputs/Active_DAs.csv')
tmap_save(active_da_map,'./Outputs/active_da_map.html')