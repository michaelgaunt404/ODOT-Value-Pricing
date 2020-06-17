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
library(magrittr)
library(readxl)
library(tidyverse)
library(lubridate)
library(data.table)

# library(ggmap) #ggplot2 for spatial 
library(tmap) #vis for maps (ggmap alternative)
library(rgdal) #for inport/outport
library(rgeos) #for spatial analysis operations 
library(maptools) #provides mapping functions


#path and data set-up~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/")
rstudioapi::getSourceEditorContext()$path %>% 
  as.character() %>% 
  gsub("R.*","\\1", .) %>% 
  path.expand() %>%  
  setwd()

#data import~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data_folders = list.dirs("./data/ODOT_TrafficData/Traffic Data")[-1]

file_odot = "./data/ODOT_CountLocations_wTDMVolumes"
odot_counts_og_shapefile = readOGR(file_odot, 
                                   "ODOT_TCM_wVolume",
                                   verbose = F)

#wrangling~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~direction info
odot_direction = odot_counts_og_shapefile@data[, c("ID", 'PullFlag')] %>% 
  data.table() %>% 
  .[,`:=`(PullFlag = fct_recode(as.factor(PullFlag), 
                                Directional = "2",
                                Bidirectional = "1", 
                                Dropped = "0"))] 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~extraction
suppressMessages({ 
  suppressWarnings({
    
    station_lookup = data.table()
    # count = 0
    # i = 1
    # j = 1
    # j = 2
    # j = 3

    for (i in 1:length(data_folders)){

      
      tmp = data.table(relative_path = data_folders[i], 
                       Station = data_folders[i] %>% 
                         dir())
      
      tmp_extract_ttl = data.table()
      
      for (j in 1:nrow(tmp)){
        count = count +1
        print(count)
        tmp_extract_location = paste(tmp$relative_path[j], 
                                     tmp$Station[j], sep = "/") %>%  
          read_excel(sheet = "Location", 
                     col_names = F) %>%  
          data.table() 
        
        tmp_extract_tcm = paste(tmp$relative_path[j], 
                                tmp$Station[j], sep = "/") %>%  
          read_excel(sheet = "TCM.Export", 
                     col_names = F) %>%  
          data.table() %>% 
          setnames(c("Timestamp", "Count")) %>%
          .[str_detect(Timestamp, "AM|PM")] %>%  
          .[,`:=`(Timestamp = parse_date_time(Timestamp, "mdy HM p"), 
                  Count = as.numeric(Count))]
        
        tmp_extract_ttl = data.table(Location = tmp_extract_location[1,1][[1]],
                                     Station = tmp$Station[j] %>% str_remove(".xlsx"),
                                     Lat = tmp_extract_location[2,1][[1]] %>% as.numeric(), 
                                     Long = tmp_extract_location[2,2][[1]] %>% as.numeric(), 
                                     Timestamp = tmp_extract_tcm$Timestamp, 
                                     Count = tmp_extract_tcm$Count) %>%
          bind_rows(tmp_extract_ttl, .)
      }
      
      station_lookup = tmp_extract_ttl %>% 
        bind_rows(station_lookup, .)
    }
    print("Done")
  })
})

extracted_odot_data = station_lookup %>% 
  data.table() %>% 
  .[,`:=`(Count_Fidelity = "15-Min", 
          Type = "24 Hour Classification", 
          SRC = "ODOT", 
          Veh_Data = "No")] %>%  
  merge.data.table(odot_direction, 
                   by.x = "Station", 
                   by.y = "ID") %>% 
  setnames(old = "PullFlag", new = "Directionality") %>%  
  .[,`:=`(Current_Status = ifelse(Directionality ==  "Dropped", "Dropped", "Kept"))] %>% 
  .[Directionality == "Dropped",`:=`(Directionality = NA)] 

#write/save~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fwrite(extracted_odot_data, file = "./output/extracted_odot_data.csv")


extracted_odot_data %>% 
  .[!is.na(Location)] %>% 
  .[,head(.SD, 1), by = .(Location)] %>% 
  spatializeR() %>% 
  tm_shape() +
  tm_dots()

odot_counts_og_shapefile@data
odot_counts_og_shapefile %>% 
  # spatializeR() %>% 
  tm_shape() +
  tm_dots()

station_lookup$Location
