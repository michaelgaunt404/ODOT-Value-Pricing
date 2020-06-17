#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This script remotely knits RMarkdown files.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: script needs to be colocated in same folder as data
#-------- Script writes to same location
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#package install and load~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggmap) #ggplot2 for spatial 
library(tmap) #vis for maps (ggmap alternative)
library(rgdal) #for inport/outport
library(rgeos) #for spatial analysis operations 
library(maptools) #provides mapping functions
library(dplyr) #data manipulation 
library(kableExtra)
library(magrittr)
library(raster) #geo-measurements 
library(data.table)
library(spdep)
library(tidyverse)
library(ggpubr)
library(spdplyr)
library(lubridate)
library(readxl)

tmap_mode('view')

#path and data set-up~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/")
rstudioapi::getSourceEditorContext()$path %>%
  as.character() %>%
  gsub("R.*","\\1", .) %>%
  path.expand() %>%
  setwd()


#data inport~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
combined_extracted_data = fread("./output/combined_extracted_data.csv", 
                                stringsAsFactors = T ) %>% 
  .[,`:=`(Timestamp = gsub("[[:alpha:]]", ' ', Timestamp) %>% 
            as_datetime())] %>%  
  .[,`:=`(Current_Status = as.factor("Kept"))]

# TODO need to have something that pulls `Kept_Extracted_Data` from shapefile_extracR
# needs to be written as shapefile
# used for filtering
# [[P L A C E  H O L D E R]]

Kept_Extracted_Data_Lookup = fread("./output/Kept_Extracted_Data_Lookup.csv", 
                                stringsAsFactors = F) %>% 
  .[,`:=`(Current_Status = as.factor(Current_Status))]


#data munging~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
kept_CED = combined_extracted_data %>%  
  .[Location %in% Kept_Extracted_Data_Lookup$Location, -c("Current_Status")] %>%  
  .[, .(Hour_Counts = sum(Count)), by = .(SRC, Location, 
                                          Directionality, Road, Heading, 
                                          Floor_Hour = floor_date(Timestamp, unit = "hour"))] %>% 
  .[order(Location, Road, Heading, Floor_Hour)] %>% 
  .[hour(Floor_Hour) %between% c(7,9) | 
      hour(Floor_Hour) %between% c(16,18) & 
      year(Floor_Hour) %between% c(2014,2016)] %>% 
  na.omit()










  
