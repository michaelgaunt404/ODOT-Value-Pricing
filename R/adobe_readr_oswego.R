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
file_path_LO = "./data/LO_Count_Data/converted"
file_path_LU = "./data/LU_Count_Data_2/converted"

data_files_LO = dir(file_path_LO) %>%  
  data.table(path = paste0(file_path_LO,"/", .),
             location = str_remove(., ".xlsx")) %>% 
  .[,-1]

data_files_LU = dir(file_path_LU) %>%  
  data.table(path = paste0(file_path_LU,"/", .),
             location = str_remove(., ".xlsx")) %>% 
  .[,-1]

data_files = data_files_LO%>% 
  bind_rows(data_files_LU) 

data_files %>% 
  fwrite(file = "./output/local_files.csv")

data_locations = fread("./output/manual_gps_extracts/data_locations_oswego.csv") %>% 
  setnames(old = "files", new = "filename")

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

# currently extracting the locations that are procing difficult to read
# most likely ATD data and most likely not four-way
data_files = data_files[!which(location %in% c("Boones Ferry - Upper", 
                                                    "Carman - Bonita - Waluga", 
                                                    "Country Club - Iron Mountain (Six Corners)", 
                                                    "S Shore - Lakeview", 
                                                    "Stafford - Overlook", 
                                                    "Stafford - Rosemont", 
                                                    "State - D"))]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~GPS clean-up
data_locations = data_locations %>% 
  .[,`:=`(Lat = coordinate %>% 
            gsub(",.*","\\1", .) %>%  
            as.numeric(),
          Long = coordinate %>% 
            gsub(".*,","\\1", .) %>%  
            as.numeric())] %>%  
  .[, .(filename, Lat, Long)]




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~lake oswego data extraction
# NOTE
# the below loop extracts ATD and QC data form the Lake Oswego data sheets
# script takes QC data over ATD data if file contains both
# script takes ATD data if that is the only data avaible for the location
# TODO able to grab all data (ATD/QC) for locations 
# TODO compare those data 
# TODO include excluded locations


suppressMessages({ 
  suppressWarnings({
    
    extracted_location_data = data.table() 
    
    for (i in 1:nrow(data_files)){
      print(data_files[i, path])
      
      #grabs all data from file
      tmp_raw = data_files[i, path] %>%
        read_excel(path = ., col_names = F) %>%  
        mutate_all(as.character) %>% 
        data.table() %>% 
        clean_names() #%>%
      
      #locates QC data identifier
      cutoff = tmp_raw$x1 %>%  
        str_which(., "QC JOB #:") %>%  
        min()
      
      #determines method for data
      if(cutoff != Inf){
        
        #~~~~~~~~~~~~~QC METHOD
        
        #slices data
        tmp_QCounts = tmp_raw %>%
          .[cutoff:nrow(tmp_raw),]
        
        #searches for and makes proper headers
        tmp_colnames = tmp_QCounts %>% 
          .[str_detect(x1, "5-Min Count")] %>% 
          .[1] %>% 
          .[,`:=`(dummy = 1)] %>%  
          melt.data.table(id.vars = c("dummy"), 
                          value.name = "V1") %>% 
          .[!is.na(V1)] %>% 
          .[str_detect(V1, "bound")] %>%  
          .[,.(V1 = rep(V1, each = 4),
               V2 = rep(c("Left", "Center", "Right", "U"), 4)) ] %>% 
          .[,`:=`(combined = paste(str_replace_all(V1, "\r\n", "x"), V2, sep = "_"))] 
        
        #extracts counts data 
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
          str_split_fixed( "_", 18) %>%  
          data.table() %>%  
          .[,-c(17,18)] %>% 
          setnames(tmp_colnames$combined  )
        
        #builds final data
        tmp_count_extract = tmp_QCounts %>% 
          .[, ..index_columns_for_count_extractr] %>% 
          .[,-c(1, 2, 3)] %>% 
          bind_cols(., tmp_count_extract) %>%  
          melt.data.table(id.vars = index_columns_for_count_extractr[-c(1:3)], 
                          value.name = "Count") %>%
          .[,`:=`(Road = gsub("x.*", "\\1",variable),
                  Heading = gsub(".*x", "\\1",variable) %>% 
                    gsub("_.*", "\\1", .),
                  Turn_Type = gsub(".*_", "\\1",variable), 
                  SRC = "QC", 
                  filename = data_files[i, location])] 
        
        
      } else {
        
        #~~~~~~~~~~~~~ATD METHOD
        
        #searches for and makes column names
        cutoff = tmp_raw$x1 %>%  
          str_which(., "Interval") %>% 
          .[2] 
        
        tmp_colnames = tmp_raw %>%
          .[cutoff,] %>% 
          .[,`:=`(dummy = 1)] %>%  
          melt.data.table(id.vars = c("dummy"), 
                          value.name = "V1") %>% 
          .[!is.na(V1)] %>% 
          .[!str_detect(V1, "Interval")] %>%  
          .[,.(V1 = rep(V1, each = 4),
               V2 = rep(c("Left", "Center", "Right", "Bike"), 4)) ] %>% 
          .[,`:=`(combined = paste(str_replace_all(V1, "\r\n", "x"), V2, sep = "_"))]
        
        #makes proper slice of the data
        tmp_ATD = tmp_raw %>%  
          .[10:36,] %>% 
          .[str_length(x1) < 9,] %>% 
          .[str_detect(x1, "AM") |
              str_detect(x1, "PM"),] %>% 
          remove_empty("cols")
        
        #extracts location, time and date
        tmp_info = tmp_raw %>%
          .[str_detect(x1, "Interval Summary"), 1] %>% 
          .[1,] %>% 
          str_split_fixed("\r\n", 3)
        
        #builds final data
        tmp_count_extract = tmp_ATD %>% 
          .[,-c(18)] %>% 
          setnames(c("time", tmp_colnames$combined)) %>% 
          # .[-c(1,2), -c("Interval Total_NA")] %>% 
          melt.data.table(id.vars = c("time"), 
                          value.name = "Count") %>%  
          .[,`:=`(Heading = gsub("x.*", "\\1",variable),
                  Road = gsub(".*x", "\\1",variable) %>% 
                    gsub("_.*", "\\1",.),
                  Turn_Type = gsub(".*_", "\\1",variable), 
                  Location = tmp_info[,1], 
                  Timestamp = paste(tmp_info[,2], time) %>% 
                    parse_date_time("mdy HM p"), 
                  SRC = "ATD", 
                  filename = data_files[i, location])]
      }
      
      extracted_location_data = bind_rows(extracted_location_data, 
                                          tmp_count_extract)
    }
    print("Done")
  })
})


extracted_oswego_data = extracted_location_data %>% 
  .[, .(filename, SRC, Location, Road, Heading, Turn_Type, Timestamp, Count)] %>%  
  .[,`:=`(Count = as.numeric(Count),
          Turn_Type = as.factor(Turn_Type),
          Type = "Turn_Counts",
          Heading = gsub("[[:punct:]]", "", Heading) %>% as.factor(),
          Directionality = "Unidirectional",
          Count_Fidelity = "5-Min", 
          Current_Status = "kept",
          Veh_Data = "No")] %>%  
  merge.data.table(., data_locations, 
                   by = c("filename"), 
                   all = T)

extracted_oswego_data %>%  
  fwrite("./output/extracted_oswego_data.csv")


extracted_oswego_data  %>% visdat::vis_dat()
