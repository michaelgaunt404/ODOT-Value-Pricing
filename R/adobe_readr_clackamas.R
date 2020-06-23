#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This script extracts all data contained in Clackamas County PDFs.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: script extracts only `24 hour classificaiton` counts
#-------- script needs to be in R folder 
#-------- comment out `path and data set-up` section if sourced from RMarkdown
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#package install and load~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(data.table)
library(magrittr)
library(dplyr)
library(readxl)
library(stringr)
library(spdplyr)
library(tidyverse)
library(tmap)
library(leaflet)
library(lubridate)
library(ggplot2)
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
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~clackamas
file_path_County = "./data/Clackamas_County/converted/Clackamas_County_Master.xlsx"

county = read_excel(path = file_path_County, 
                    col_names = F) %>%  
  data.table() %>% 
  .[,`:=`(row = rownames(.))]

colnames(county) = colnames(county) %>% 
  str_remove_all("[:punct:]") %>%  
  paste0("name", .)

#wrangling~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~index set-up
index_type_extract = paste("24 Hour Classification", 
                            "Daily Speed", sep = "|")

index_count_columns = c("time", "Total", "Motor_Bikes", "Cars_Trailers",
                        "2_axle_Long", "Buses", "2_axle_6_tire", 
                        '3 Axle', "4 Axle", "<5 Axle Double", "5 Axle Double",
                        ">6 Axle Double", "<6 Axle Multi", "6 Axle Multi", ">6 Axle Multi") %>%  
  str_replace_all(" ", "_")

tmp_counts = county[str_detect(name1, "PM") | 
                      str_detect(name1, "AM") |
                      str_detect(name1, "LOCATION"),]

tmp = tmp %>%  
  .[str_detect(name1, "LOCATION"), 
    `:=`(raw = paste0(name4, name5, name20, name21, name22, name23, 
                      name24, name25, name26, 
                      name27, name28, name45, name46, 
                      name47))] %>%
  data.table() 

test = bind_cols(tmp_counts, tmp_counts %>%  
                   unite("raw")) %>% 
  .[!str_detect(raw, "LOCATION"), `:=`(raw = NA)] %>% 
  .[,`:=`(raw = raw %>%  
            str_remove_all("_NA")),] %>% 
  fill(raw, .direction = "down") %>% 
  data.table()

test = test %>% 
  .[,`:=`(Location = raw %>% 
            gsub("\r\n:.*","\\1", .) %>% 
            gsub(".*:","\\1", .),
          Lat = raw %>% 
            gsub(",.*","\\1", .) %>% 
            gsub(".*\r\n:","\\1", .) %>%
            str_remove("N") %>%
            str_remove(",") %>%  
            str_trim() %>% 
            as.numeric(), 
          Long = raw %>% 
            gsub(".*(-)","\\1", .) %>% 
            gsub("\r\n.*","\\1", .) %>%
            str_remove("W") %>% 
            str_trim() %>% 
            as.numeric(), 
          Date = test$raw %>% 
            gsub(".*Site:","\\1", .) %>% 
            str_trim() %>% 
            str_sub(start = 11) %>% 
            str_remove_all("[[:alpha:]]") %>%  
            gsub("_.*", "\\1", .) %>% 
            str_remove_all("\r\n") %>% 
            parse_date_time("mdy"),
          Heading = test$raw %>% 
            gsub(".*Site:","\\1", .) %>%
            str_remove_all("[:digit:]") %>%
            gsub("\r\n.*","\\1", .) %>% 
            str_trim() %>% 
            as_factor() %>%
            fct_recode(Southbound = "SB",
                       Northbound = "NB",
                       Eastbound = "EB",
                       Westbound = "WB", 
                       Both = ""),
          Type = raw %>%  
            str_extract(index_type_extract))] 

# filters rows for 24 hour classification per location
# filters out non-count rows 
# filters for only important columns - currently only grapping total counts
tmp_1 = test %>% 
  .[str_detect(Type, "24"),] %>%  
  .[!is.na(name10)] %>%
  remove_empty("cols") %>% 
  data.table() %>% 
  .[,`:=`(Timestamp = paste(Date, name1) %>% 
            parse_date_time("ymd IMOp"))]

# renames columns 
colnames(tmp_1)[1:15] = index_count_columns

#final DF 
extracted_clackamas_data = tmp_1 %>%  
  .[,-c("raw", "namerow", "time", "Date")] %>% 
  melt.data.table(measure.vars = index_count_columns[-c(1,2)], 
                  value.name = "Count", variable.name = "Veh_Type") %>%  
  .[,`:=`(SRC = "County", 
          Count = as.numeric(Count),
          Directionality = ifelse(Heading == "Both", "Bidirectional", "Unidirectional"),
          Count_Fidelity = "Hour", 
          Current_Status = "Dropped", 
          Veh_Data = "Yes")] %>%  
  .[is.na(Heading), `:=`(Directionality = "Bidirectional")]

extracted_clackamas_data %>%  
  fwrite("./output/extracted_clackamas_data.csv")
