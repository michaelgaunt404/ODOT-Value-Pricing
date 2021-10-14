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
library(data.table)
library(magrittr)
library(dplyr)
library(readxl)
library(stringr)
library(spdplyr)
library(tmap)
library(leaflet)

#path and data set-up~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# setwd("~/")
rstudioapi::getSourceEditorContext()$path %>%
  as.character() %>%
  gsub("R.*","\\1", .) %>%
  path.expand() %>%
  setwd()

#data inport~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
station_lookup = fread("./data/station_lookup.csv") 

file = "./data/2015_I-205SubareaCountValidation_051820.xlsx"
sub205_am68 = read_excel(path = file, 
                         sheet = "205Subarea_AM_6to8", 
                         skip = 1) %>%  
  data.table()



#data cleaning~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#station lookup~~~~~~~~
station_lookup$station = station_lookup$station %>%  
  as.character()




#sub file~~~~~~~~
#cleaning column names
column_cleanr = function(data){
  colnames(data) %>% 
    str_remove("\r\n") %>%  
    str_remove_all("[:punct:]") %>% 
    str_trim() %>% 
    str_remove_all(" ") %>% 
    # str_replace_all(" ", "_") %>% 
    # str_replace_all("??", "D") %>%
    gsub("\\.\\d*$", " ", .)
}

colnames(sub205_am68) = sub205_am68 %>%  
  column_cleanr()

#grabbing cleaned columsn for bi/NE/SW columns
index_col_bi = c("2015AM2EmmeCapacity14", "2015AM2EmmeVolumes15", 
                 "2015AM2Counts16", "DifferenceEmmeCounts17",
                 "fromCounts18","GEH19")

index_col_NE = c("2015AM2EmmeCapacity22", "2015AM2EmmeVolumes23", 
                 "2015AM2Counts24", "DifferenceEmmeCounts25",
                 "fromCounts26","GEH27")

index_col_SW = c("2015AM2EmmeCapacity29", "2015AM2EmmeVolumes30", 
                 "2015AM2Counts31", "DifferenceEmmeCounts32",
                 "fromCounts33","GEH34")

#data munging~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#removing empty/summary excel rows 
sub205_am68_cleaned = sub205_am68 %>% 
  .[!is.na(Station)]

sub205_am68_rm = sub205_am68 %>% 
  .[is.na(Station)]

#joining with station_location data 
not_matched = sub205_am68_cleaned %>%  
  anti_join(station_lookup, by = c("Station" = "station")) %>%  
  data.table() %>%  
  .[,`:=`(gps = "n")]

matched = sub205_am68_cleaned %>%  
  merge.data.table(station_lookup, 
                   by.x = c("Station"), 
                   by.y = c("station"), 
                   all.x = F) %>%  
  data.table() %>%  
  .[,`:=`(gps = "y")]


combined = matched %>%  
  bind_rows(not_matched) %>%  
  data.table()

combined %>%  
  .[,`:=`(direction = ifelse( (NEEmmeID=="-")==(SWEmmeID=="-"), 2, 1))]

subset_index = c("Cutline", "Station", "CountYear", 
                 index_col_bi[1:3], index_col_NE[1:3], index_col_SW[1:3],
                 colnames(station_lookup)[4:6], "gps", "direction")

counts_subset = combined[, ..subset_index]
counts_subset$CountYear = counts_subset$CountYear %>%  
  as.factor()


#mapping~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#setting up prjection
count_coors = cbind(counts_subset[!is.na(lat)]$long, counts_subset[!is.na(lat)]$lat) %>%  
  SpatialPoints() 
crs.geo <- CRS("+init=epsg:4326")  # UTM 33N
proj4string(count_coors) <- crs.geo  # define projection system of our data

counts_boi = SpatialPointsDataFrame(count_coors, counts_subset[!is.na(lat)])




#mapping
tmap_mode("view")

plot_boi = counts_boi %>% 
  tm_shape() + 
  tm_bubbles(col= "CountYear",
             # scale = 2,
             border.col = "black", 
             alpha = .5,
             size = "direction",
             scale = 2,
             # contrast=1,
             palette = "plasma",
             id = "Station", 
             popup.vars=c("Year" = "CountYear",
                          "Directions" = "direction",
                          "Lat."="lat",
                          "Long."="long", 
                          "Cutline" =  "Cutline"),
             popup.format=list(lat=list(digits=2),
                               long=list(digits=2)))

lf = tmap_leaflet(plot_boi)
lf = lf %>% leaflet::addMarkers(-122.603654, 45.364644, popup = "Abernathy Bridge")




