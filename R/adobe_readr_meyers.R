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
traffic_study_meyers = read_excel("./data/Traffic_Studies/converted/170623_Meyers Road Traffic Analysis Memo.xlsx", 
                                  skip = 105, col_names = F) %>%  
  data.table() %>% 
  clean_names() %>% 
  remove_empty("cols")

data_locations_meyers = fread("./output/manual_gps_extracts/data_locations_meyers.csv") %>%
  .[,`:=`(Lat = coordinate %>% 
            gsub(",.*", "", .),
          Long = coordinate %>% 
            gsub(".*,", "", .))]


#wrangling~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~common functions and indexes
super_cleanr = function(string){ 
  string %>% 
    gsub("X.*","\\1", .) %>% 
    gsub("\r\n.*","\\1", .) %>% 
    gsub("NA.*","\\1", .) %>% 
    str_trim()
}

index_columns_for_count_extractr = c("Date", "raw", "x1", "Location", "State", "QC_job", "Timestamp")




# locates end of data section
cutoff = traffic_study_meyers$x1 %>%
  str_which(., "Appendix") %>%
  min()

# slices data
tmp_QCounts = traffic_study_meyers  %>%
  .[1:cutoff,]

super_cleanr = function(string){ 
  string %>% 
    gsub("X.*","\\1", .) %>% 
    gsub("\r\n.*","\\1", .) %>% 
    gsub("NA.*","\\1", .) %>% 
    str_trim()
}

# extracts counts data 
# finds row that has all the information in it for a given intersection
# condenses that row to sting
# string then parsed for relevant infromation
tmp_QCounts = bind_cols(tmp_QCounts, tmp_QCounts %>%  
                         unite("raw")) %>% 
  .[!str_detect(raw, "LOCATION"), `:=`(raw = NA)] %>%
  .[,`:=`(raw = str_replace_all(raw, "             ", "X") %>% 
            str_replace_all("\r\n", "X"))] %>%  
.[str_detect(raw, "LOCATION"),`:=`(Location = raw %>% 
                                     gsub(".*LOCATION:","\\1", .) %>%
                                     gsub("STATE | QC | DATE.*","\\1", .) %>% 
                                     super_cleanr(),
                                   State = raw %>% 
                                     gsub(".*STATE:","\\1", .) %>% 
                                     gsub("LOCATION | QC | DATE.*","\\1", .) %>%
                                     super_cleanr(),
                                   QC_job = raw %>% 
                                     gsub(".*#:","\\1", .) %>%
                                     gsub("LOCATION | STATE | DATE.*","\\1", .) %>%
                                     super_cleanr(),
                                   Date = raw %>% 
                                     gsub(".*DATE:","\\1", .) %>% 
                                     gsub("LOCATION | STATE | QC.*","\\1", .) %>%
                                     gsub("_.*","\\1", .) %>% 
                                     super_cleanr() %>% 
                                     parse_date_time(c("mdy")) %>%
                                     as.character())]

#searches for and makes proper headers
# TODO needs to be coupled with correct intersection
tmp_colnames = tmp_QCounts %>% 
  .[str_detect(x1, "5-Min Count")] %>%
  remove_empty("cols") %>% 
  unique() %>% 
  bind_cols(. , tmp_QCounts$Location %>% 
              data.table(Location = .) %>% 
              unique() %>% 
              na.omit()) %>% 
  melt.data.table(id.vars = c("Location"),
                  variable.name = "Sequence",
                  value.name = "V1") %>% 
  .[str_detect(V1, "bound")] %>%  
  .[order(Location, Sequence)] %>% 
  .[,.(Location = rep(Location, each = 4),
       Sequence = rep(Sequence, each = 4),
       V1 = rep(V1, each = 4),
       Turn_Type = rep(c("Left", "Center", "Right", "U"), 4)) ] %>%
  .[,`:=`(Road = gsub("\r\n.*", "\\1",V1),
          Heading = gsub(".*\r\n", "\\1",V1) %>% 
            gsub("_.*", "\\1", .) %>%  
            gsub("[[:punct:]]", "\\1", .))] %>% 
  .[,`:=`(Col_seq = 1)] %>% 
  .[,`:=`(Col_seq = cumsum(Col_seq)), by = Location] %>% 
  .[,`:=`(Col_seq = paste0("V", Col_seq))] %>% 
  .[,.(Location, Road, Turn_Type, Heading, Col_seq)]

#cleans resulting count data and fills out remaining columns
#extends values down to make each row tidy
tmp_QCounts = tmp_QCounts %>%  
  fill(Location, .direction = "down") %>% 
  fill(State, .direction = "down") %>% 
  fill(QC_job, .direction = "down") %>% 
  fill(Date, .direction = "down") %>%  
  data.table() %>%  
  .[str_detect(x1, "AM|PM")] %>% 
  .[str_length(x1)<10] %>%
  .[,`:=`(Timestamp = paste(Date, x1) %>% 
            parse_date_time("ymd HM p"))]

#cleans only the count data 
#this is nessessary step
#count columns not consitent in excel and have many merged data columns 
#puts everything to sting and the parses
tmp_count_extract = tmp_QCounts %>% 
  .[,-..index_columns_for_count_extractr] %>% 
  remove_empty("cols") %>%  
  unite("combined_num") %>% 
  data.table() %>% 
  .[,.(combined_num_corrected = str_replace_all(combined_num, " ", "_") %>% 
         gsub('([[:punct:]])\\1+', '\\1', .))] %>% 
  pull(combined_num_corrected) %>% 
  str_split_fixed( "_", 30) %>%  
  data.table()

tmp_count_extract = purrr::map_df(tmp_count_extract[,1:30], as.numeric) %>%  
  data.table() %>%  
  bind_cols(., tmp_QCounts %>% 
              .[, ..index_columns_for_count_extractr] %>% 
              .[,-c(1, 2, 3)])

#builds final data
nest_tmp_count_extract = tmp_count_extract %>%  
  group_by(Location, hour(Timestamp) >12) %>%  
  nest() %>%  
  mutate(reduced = map(data, remove_empty, "cols")) %>% 
  mutate(reduced = map(reduced, set_colnames, c(paste0("V", seq(1,18)), "State", "QC_job", "Timestamp"))) %>% 
  .[,c('Location', "reduced")] 

extracted_meyers_data = nest_tmp_count_extract %>% 
  unnest(cols = "reduced") %>%  
  data.table() %>%  
  melt.data.table(id.vars = c('Location', 'State', 'QC_job', 'Timestamp'), 
                  value.name = "Count") %>%  
  merge.data.table(., tmp_colnames, by.x = c("Location", "variable"), by.y = c("Location", "Col_seq")) %>% 
  .[,`:=`(SRC = "QC", 
          Type = "Turn_Counts",
          Directionality = "Unidirectional", 
          Current_Status = "Kept", 
          Count_Fidelity = "5-min", 
          Veh_Data = "No")] %>%  
  merge.data.table(., 
                   data_locations_meyers[,.(files, Lat, Long)], 
                   by.x = c("Location"), 
                   by.y = c("files"))
  

extracted_meyers_data %>%  
  fwrite("./output/extracted_meyers_data.csv")
