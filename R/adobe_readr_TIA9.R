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
traffic_study_TIA9 = read_excel("./data/Traffic_Studies/converted/cove_and_TIA9.xlsx", 
                                  sheet = "tia_9_alt", 
                                  col_names = F) %>%  
  data.table() %>% 
  clean_names() %>% 
  remove_empty("cols") %>% 
  setnames(c("Keep", "String_Data", "Date" )) 


# traffic_study_TIA9 = traffic_study_TIA9 %>%  
#   .[Keep == "keep"] %>% 
#   fill(c("Date")) %>% 
  

#data inport~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# traffic_study_TIA9_reduced =  traffic_study_TIA9 %>% 
#   .[!str_detect(x1, "Heavy|15")] 

# index_delimiter = traffic_study_TIA9_reduced$x1 %>%
#   str_which("5-Minute Interval Summary")
# 
# traffic_study_TIA9 = traffic_study_TIA9 %>% 
#   fill(c("Location", "Date"))
# 
# traffic_study_TIA9 %>% 
#   head(70)
# 
# traffic_study_TIA9_reduced %>%  
#   .[!is.na(Location)]
# 
# traffic_study_TIA9 %>%  
#   .[sort(index_master)[265:(265+23)],]

index_delimiter = traffic_study_TIA9$String_Data %>%
  str_which("5-Minute Interval Summary")

index_operators = seq(-3, 20, by =1)

# index_master = c()
# for (i in 1:length(index_operators)){
#   index_master = c(index_master, 
#                    index_delimiter + index_operators[i])
# }

traffic_study_TIA9 = traffic_study_TIA9 %>% 
  data.table() %>% 
  .[index_delimiter-2, `:=`(Location = String_Data)] %>% 
  .[Keep == "keep"] %>% 
  fill(c("Location", "Date")) %>% 
  data.table()

index_row_colnames = traffic_study_TIA9$String_Data %>%  
  str_which("^Interval")

tmp_colnames = traffic_study_TIA9 %>% 
  .[c(index_row_colnames, 
      index_row_colnames+1,
      index_row_colnames+2) %>% 
  sort(),]

tmp_colnames = bind_cols(tmp_colnames[str_detect(String_Data, "^Interval")],
                         tmp_colnames[str_detect(String_Data, "^Start"),2],
                         tmp_colnames[str_detect(String_Data, "^Time"),2]) %>%
  setnames(old = c("String_Data", "String_Data1", "String_Data2"), 
           new = c("Headings", "Roads", "Turns")) %>% print()
  .[,`:=`(Headings = str_remove_all(Headings, "Interval|Pedestrians") %>%  
            str_trim(), 
          Roads = str_remove_all(Roads, "Pedestrians|Start|Interval|Crosswalk") %>%  
            str_trim(),
          Turns = str_remove_all(Turns, "Interval|Time|Total|North|South|East|West") %>%  
            str_trim())] %>% print()
  unique()

  tmp_colnames[,6] %>% 
    data.table() %>%  
    pull(Turns) %>% 
    str_split_fixed(pattern = " ", 
                    n = 30) %>% 
    data.table() %>% 
    remove_empty("cols") %>% 
    bind_cols(tmp_colnames[, 4], .)
  
  
  
  
  
# tmp_colnames = tmp_colnames %>% 
#   setnames(old = c("x1", "x11"), 
#            new = c("Road", "Heading")) %>% 
#   .[,`:=`(Road = gsub("\r\n.*", "\\1", Road),
#           Heading = gsub(".*\r\n", "\\1",Heading) %>% 
#             gsub("[[:punct:]]", "\\1", .))] %>% 
#   purrr::map_df(., rep, each = 4) %>%  
#   data.table(., 
#              Turn_Type = rep(c("Left", "Center", "Right", "U"), 
#                              nrow(tmp_colnames))) %>%
#   .[,`:=`(Col_seq = 1)] %>% 
#   .[,`:=`(Col_seq = cumsum(Col_seq)), by = Location] %>% 
#   .[,`:=`(Col_seq = paste0("V", Col_seq))] %>% 
#   .[,.(Location, Road, Turn_Type, Heading, Col_seq)] %>%   print()


extracted_counts_raw = traffic_study_TIA9 %>% 
  .[str_detect(String_Data, "AM|PM") &
      str_length(String_Data) >25,] %>%  
  .[,`:=`(Timestamp = gsub("(AM|PM).*", "\\1", String_Data) %>% 
            paste(Date, .) %>%
            parse_date_time(., "mdy HM p"),
          Data = gsub(".*AM", "\\1", String_Data) %>% 
            gsub(".*PM", "\\1", .) %>% 
            str_trim("both") )] 

# extracted_counts_raw$Timestamp %>% 
#   gsub(".*,", "", .) %>% 
#   parse_date_time(., "mdy HM p")

extracted_counts = extracted_counts_raw[,6] %>% 
  pull(Data) %>% 
  str_split_fixed(pattern = " ", 
                  n = 25) %>% 
  data.table() %>%
  purrr::map_df(., as.numeric) %>%  
  remove_empty("cols") %>% 
  data.table() %>% 
  bind_cols(extracted_counts_raw, 
            .) %>% 
  .[,-c("String_Data", "Date", "Data", "V17", "V18", "Keep")] 

extracted_counts %>%  
  select(Location)
  
  
extracted_counts %>% 
  .[,-c("Timestamp")] %>% 
  group_by(Location) %>%  
  nest() %>%  
  mutate(reduced = map(data, remove_empty, "cols")) %>% 
  mutate(reduced = map(reduced, set_colnames, c(paste0("V", seq(1,18)), "State", "QC_job", "Timestamp"))) %>% 
  .[,c('Location', "reduced")] 

%>% 
  melt.data.table(id.vars = c('Location', "Timestamp"), 
                  variable.name = "Col_seq", 
                  value.name = "Count") %>%  
  .[order(Location, Timestamp)]   


  



































