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


#munging~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~raw combined data saved for prosterity 
CED_raw = extracted_oswego_data %>% 
  bind_rows(extracted_clackamas_data) %>% 
  bind_rows(extracted_odot_data) %>%  
  bind_rows(extracted_meyers_data) %>% 
  .[,.(SRC, Location, Lat, Long, Station, 
       Type, Directionality, Current_Status,
       Count_Fidelity, Veh_Data, Veh_Type, Turn_Type,
       Road, Heading, Timestamp, Count)] %>%  
  .[SRC != "ODOT",`:=`(Current_Status = "Dropped")] %>% 
  .[,`:=`(Floor_Timestamp = gsub("[[:alpha:]]", ' ', Timestamp) %>% 
            as_datetime() %>% 
            floor_date(unit = "hours"))]

CED_raw[, -c("Floor_Timestamp")] %>%
  fwrite("./output/CED_raw.csv")

#~~~~~~~~~~~~~~~~~~~raw data initally filtered and aggregated 
# NOTE: DF has <NA>s in `Current Status` column 
# ----: this is okay, `Kept` status for each row is correct and is used in mapping
index_aggregator = c(colnames(CED_raw)[-c(5,8, 11, 12, 15, 16)])

CED_init_filter_and_agg = CED_raw %>% 
  .[, .(Hourly_Counts = sum(Count)), by = index_aggregator] %>%
  .[SRC != "ODOT"] %>% 
  .[hour(Floor_Timestamp) %between% c(7,9) | 
      hour(Floor_Timestamp) %between% c(16,18), `:=`(Current_time = T)] %>% 
  .[year(Floor_Timestamp) %between% c(2014,2017), `:=`(Current_year = T)] %>% 
  .[Current_time == T &
      Current_year == T, `:=`(Current_Status = "Kept")] %>% 
  bind_rows(., CED_raw[SRC == "ODOT", -c("Timestamp", "Count")])

CED_init_filter_and_agg %>%
  fwrite("./output/CED_init_filter_and_agg.csv")


exit_volumes_countr = function(data, center, right, left, U){
  data = data %>% 
    .[Type == "Turn_Counts" &
        (Heading ==  center & Turn_Type == "Center") | 
        (Heading == right & Turn_Type == "Right") |
        (Heading == left & Turn_Type == "Left") |
        (Heading == U & Turn_Type == "U"), ] %>%
    .[, .(Hourly_Counts = sum(Count)), by = c(index_aggregator[-10])] %>% 
    .[,`:=`(Heading = center, 
            Flow_Type = "Outbound")]
  return(data)
}

bind_rows(exit_volumes_countr(CED_raw, "Southbound", "Eastbound", "Westbound", "Northbound"),
          exit_volumes_countr(CED_raw, "Northbound", "Westbound", "Eastbound", "Southbound"),
          exit_volumes_countr(CED_raw, "Westbound", "Southbound", "Northbound", "Eastbound"),
          exit_volumes_countr(CED_raw, "Eastbound", "Northbound", "Southbound", "Westbound"))



CED_init_filter_and_agg = 
  CED_raw$Turn_Type %>% 
  .[Type == "Turn_Counts" &
      (Heading == "Southbound" & Turn_Type == "Center") | 
      (Heading == "Eastbound" & Turn_Type == "Right") |
      (Heading == "Westbound" & Turn_Type == "Left") |
      (Heading == "Westbound" & Turn_Type == "Left"), ] %>%
  .[, .(Hourly_Counts = sum(Count)), by = c(index_aggregator[-10])] .


CED_raw %>% 
  .[Type == "Turn_Counts" &
      (Heading == "Northbound" & Turn_Type == "Center") | 
      (Heading == "Westbound" & Turn_Type == "Right") |
      (Heading == "Eastbound" & Turn_Type == "Left"), ] %>%
  .[, .(Hourly_Counts = sum(Count)), by = c(index_aggregator[-10])]

CED_raw %>% 
  .[Type == "Turn_Counts" &
      (Heading == "Westbound" & Turn_Type == "Center") | 
      (Heading == "Southbound" & Turn_Type == "Right") |
      (Heading == "Northbound" & Turn_Type == "Left"), ] %>%
  .[, .(Hourly_Counts = sum(Count)), by = c(index_aggregator[-10])]

CED_raw %>% 
  .[Type == "Turn_Counts" &
      (Heading == "Eastbound" & Turn_Type == "Center") | 
      (Heading == "Northbound" & Turn_Type == "Right") |
      (Heading == "Southbound" & Turn_Type == "Left"), ] %>%
  .[, .(Hourly_Counts = sum(Count)), by = c(index_aggregator[-10])]








# CED_raw %>% 
#   .[,.(.N), by = c("SRC", "Current_Status")]
# 
# CED_init_filter_and_agg %>% 
#   .[,.(.N), by = .(SRC, Current_Status, year(Floor_Timestamp))] %>%
#   .[order(Current_Status, year)]
# 
# CED_init_filter_and_agg %>%
#   .[Current_Status == "Kept"] %>% 
#   .[order(Floor_Timestamp)]
