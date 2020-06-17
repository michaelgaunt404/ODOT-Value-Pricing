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


#data import~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
traffic_study_cove = read_excel("./data/Traffic_Studies/converted/cove_and_TIA9.xlsx", 
                                  sheet = "cove", 
                                  col_names = F) %>%  
  data.table() %>% 
  clean_names() %>% 
  remove_empty("cols") 

#data inport~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
traffic_study_cove = traffic_study_cove %>% 
  data.table() %>% 
  .[str_detect(x1, "LOCATION:"), `:=`(Location = x1 %>%  
                                        gsub("QC.*", "", .) %>%  
                                        str_remove("LOCATION:"),
                                      QC_Job = x1 %>%  
                                        gsub(".*QC JOB #:", "", .))] %>% 
  .[str_detect(x1, "CITY/STATE:"), `:=`(City_State = x1 %>%  
                                          gsub("DATE.*", "", .) %>%  
                                          str_remove("CITY/STATE:"),
                                        Date = x1 %>%  
                                          gsub(".*DATE:", "", .) 
  )] %>%
  .[,`:=`(City_State = lead(City_State), 
          Date = lead(Date))] 

traffic_study_cove = traffic_study_cove %>%  
  fill(c("Location", "City_State", "QC_Job", "Date"), .direction = "down") %>%  
  data.table()

index_row_colnames = traffic_study_cove$x1 %>%  
  str_which("\\(")

tmp_colnames = traffic_study_cove %>% 
  .[c(index_row_colnames-1, index_row_colnames) %>%  
      sort(),] %>% 
  .[str_length(x1) < 40, c(1,2)]   

tmp_colnames = bind_cols(tmp_colnames[!str_detect(x1, "\\(")], 
                         tmp_colnames[str_detect(x1, "\\("), 1]) %>% 
  unique() 

tmp_colnames = tmp_colnames %>% 
  setnames(old = c("x1", "x11"), 
           new = c("Road", "Heading")) %>% 
  .[,`:=`(Road = gsub("\r\n.*", "\\1", Road),
          Heading = gsub(".*\r\n", "\\1",Heading) %>% 
            gsub("[[:punct:]]", "\\1", .))] %>% 
  purrr::map_df(., rep, each = 4) %>%  
  data.table(., 
             Turn_Type = rep(c("Left", "Center", "Right", "U"), 
                             nrow(tmp_colnames))) %>%
  .[,`:=`(Col_seq = 1)] %>% 
  .[,`:=`(Col_seq = cumsum(Col_seq)), by = Location] %>% 
  .[,`:=`(Col_seq = paste0("V", Col_seq))] %>% 
  .[,.(Location, Road, Turn_Type, Heading, Col_seq)] %>%   print()


extracted_counts_raw = traffic_study_cove %>% 
  .[str_detect(x1, "AM|PM") &
      !str_detect(x1, "Peak|Report"),] %>%  
  .[,`:=`(Timestamp = gsub("(AM|PM).*", "\\1", x1) %>% 
            paste(Date, .) %>%
            parse_date_time(., "mdy HM p"),
          Data = gsub(".*AM", "\\1", x1) %>% 
            gsub(".*PM", "\\1", .) %>% 
            str_trim("both") )] 

extracted_counts_raw$Timestamp %>% 
  gsub(".*,", "", .) %>% 
  parse_date_time(., "mdy HM p")

extracted_counts = extracted_counts_raw[,7] %>% 
  data.table() %>%  
  pull(Data) %>% 
  str_split_fixed(pattern = " ", 
                  n = 20) %>% 
  data.table() %>% 
  purrr::map_df(., as.numeric) %>%  
  remove_empty("cols") %>% 
  data.table() %>% 
  bind_cols(extracted_counts_raw, 
            .) %>% 
  .[,-c("x1", "Date", "Data", "V17", "V18")] %>% 
  melt.data.table(id.vars = c('Location', 'QC_Job', 'City_State', "Timestamp"), 
                  variable.name = "Col_seq", 
                  value.name = "Count") %>%  
  .[order(Location, Timestamp)]   

extracted_meyers_cove = extracted_counts %>%  
  merge.data.table(tmp_colnames, 
                   by = c("Location", "Col_seq")) %>% 
  .[,`:=`(SRC = "QC", 
          Type = "Turn_Counts",
          Directionality = "Unidirectional", 
          Current_Status = "Kept", 
          Count_Fidelity = "5-min", 
          Veh_Data = "No")]
  



































