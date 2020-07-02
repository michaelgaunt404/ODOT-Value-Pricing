#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This script extracts all data contained in the Oswego PDFs.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: script extracts only `24 hour classificaiton` counts
#-------- script needs to be in R folder 
#-------- comment out `path and data set-up` section if sourced from RMarkdown
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#package install and load~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(data.table)
library(magrittr)
library(tidyverse)
library(readxl)
library(spdplyr)
library(tmap)
library(leaflet)
library(lubridate)
library(janitor)

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
extracted_oswego_data = fread("./output/extracted_oswego_data.csv")
extracted_clackamas_data = fread("./output/extracted_clackamas_data.csv")
extracted_odot_data = fread("./output/extracted_odot_data.csv")
extracted_meyers_data = fread("./output/extracted_meyers_data.csv")
extracted_cove_data = fread("./output/extracted_cove_data.csv")

#munging~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~raw combined data saved for prosterity 
CED_raw = bind_rows(extracted_oswego_data, 
          extracted_clackamas_data,
          extracted_odot_data,
          extracted_meyers_data,
          extracted_cove_data) %>% 
  .[,.(SRC, Location, Lat, Long, Station, 
       Type, Directionality, Current_Status,
       Count_Fidelity, Veh_Data, Veh_Type, Turn_Type,
       Road, Heading, Timestamp, Count)] %>%  
  .[SRC != "ODOT",`:=`(Current_Status = "Dropped")] %>% 
  .[,`:=`(Floor_Timestamp = gsub("[[:alpha:]]", ' ', Timestamp) %>% 
            as_datetime() %>% 
            floor_date(unit = "hours"))] %>% 
  unique()

CED_raw[, -c("Floor_Timestamp")] %>%
  fwrite("./output/CED_raw.csv")

#~~~~~~~~~~~~~~~~~~~raw data initally filtered and aggregated 
# NOTE: DF has <NA>s in `Current Status` column 
# ----: this is okay, `Kept` status for each row is correct and is used in mapping
index_aggregator = c(colnames(CED_raw)[-c(5, 11, 12, 15, 16)])

lookup_turn_counts = CED_raw[Type == "Turn_Counts", .(Location, Road, Heading)] %>%  unique()

exit_volumes_countr = function(data, center, right, left, U){
  data = data %>% 
    .[Type == "Turn_Counts" &
        (Heading ==  center & Turn_Type == "Center") | 
        (Heading == right & Turn_Type == "Right") |
        (Heading == left & Turn_Type == "Left") |
        (Heading == U & Turn_Type == "U"), ] %>% 
    .[order(Location, Turn_Type)] %>%  
    .[Turn_Type == "Center",
      `:=`(Center_Road = Road, 
           Center_Heading = Heading)] %>%  
    fill(c("Center_Road", "Center_Heading"), .direction = "down") %>% 
    data.table() %>% 
    .[, .(Hourly_Counts = sum(Count, na.rm = T)), by = .(SRC, Location, Lat, Long, Type, 
                                              Directionality, Current_Status, Count_Fidelity, 
                                              Veh_Data, Veh_Type, Floor_Timestamp, Center_Road, 
                                              Center_Heading)] %>% 
    .[,`:=`(Intersection_Flow = "Outbound")] %>% 
    setnames(old = c("Center_Road", "Center_Heading"), 
             new = c("Road", "Heading")) 
}

CED_init_filter_and_agg = CED_raw %>% 
  .[, .(Hourly_Counts = sum(Count, na.rm = T)), by = index_aggregator] %>%
  .[Type == "Turn_Counts" ,`:=`(Intersection_Flow = "Inbound")] %>% 
  bind_rows(.,
            exit_volumes_countr(CED_raw, "Southbound", "Eastbound", "Westbound", "Northbound"),
            exit_volumes_countr(CED_raw, "Northbound", "Westbound", "Eastbound", "Southbound"),
            exit_volumes_countr(CED_raw, "Westbound", "Southbound", "Northbound", "Eastbound"),
            exit_volumes_countr(CED_raw, "Eastbound", "Northbound", "Southbound", "Westbound")) 

CED_init_filter_and_agg = CED_init_filter_and_agg %>% 
  .[SRC != "ODOT"] %>% 
  .[hour(Floor_Timestamp) %between% c(7,9) | 
      hour(Floor_Timestamp) %between% c(16,18), `:=`(Current_time = T)] %>% 
  .[year(Floor_Timestamp) %between% c(2014,2016), `:=`(Current_year = T)] %>%
  .[Current_time == T &
      Current_year == T, `:=`(Current_Status = "Kept")] %>% 
  bind_rows(., CED_init_filter_and_agg[SRC == "ODOT"]) %>% 
  data.table() 

#~~~~~~~~~~~~~~~~~~~raw data initally filtered and aggregate
# CED_init_filter_and_agg %>%  
#   .[,.(.N), by = .(Current_time == Current_year)]
#       
# CED_init_filter_and_agg %>%  
#         .[,.(.N), by = .(Type, Current_Status)]
# 
# CED_init_filter_and_agg %>%  
#   .[,.(.N), by = .(SRC, Current_Status)]
# 
# CED_init_filter_and_agg %>%  
#   .[,.(.N), by = .(Type, Current_Status)]
# 
# CED_init_filter_and_agg[Type == "Turn_Counts", .(.N), by = .(Location, Intersection_Flow)] %>% 
#   .[order(Location)] %>%  
#   dcast.data.table(Location~Intersection_Flow, value.var = "N")
# 
# #inflow == outflow checker
# CED_init_filter_and_agg %>%
#   .[Current_Status == "Kept" &
#       Type == "Turn_Counts"] %>%
#   .[,.(sum(Hourly_Counts)), by = .(Location, Floor_Timestamp, Intersection_Flow)] %>% 
#   dcast.data.table(Location+Floor_Timestamp~Intersection_Flow, value.var = "V1")


CED_init_filter_and_agg %>%
  fwrite("./output/CED_init_filter_and_agg.csv")


