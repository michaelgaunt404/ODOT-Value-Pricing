#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Utility function for mapping.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: script pulls from multiple data streams
#-------- script performs applies filtering to keep or drop files
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#package install and load~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
library(sf)
library(leaflet)
library(shiny)

suppressMessages({
  tmap_mode('view')
})

#path and data set-up~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!exists("BEING_SOURCED_FROM_SOMEWHERE")){
  setwd("~/")
  rstudioapi::getSourceEditorContext()$path %>%
    as.character() %>%
    gsub("R.*","\\1", .) %>%
    path.expand() %>%
    setwd()
}

#data inport~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CED_init_filter_and_agg = fread("./output/CED_init_filter_and_agg.csv", 
                                stringsAsFactors = F) %>% 
  .[,`:=`(Floor_Timestamp = gsub("[[:alpha:]]", ' ', Floor_Timestamp) %>% 
            as_datetime())]

potential_data_pickups = read_excel("./output/potential_data_pickups.xlsx") %>% 
  data.table() %>% 
  .[,`:=`(Long = Coordinates %>%
            gsub(".*,", "", .) %>%
            as.numeric(),
          Lat = Coordinates %>%
            gsub(",.*", "", .)  %>%
            as.numeric(), 
          Year = year(Date), 
          SRC = "Potential", 
          Date = as_date(Date), 
          Current_Status = "Kept")]

selected_out_focus_area = read_excel("./output/selected_out_focus_area.xlsx") %>% 
  data.table() 

focus_area_coordinates = read_excel("./output/manual_gps_extracts/test123.xlsx", 
                                    col_names = T) %>% 
  data.table() %>% 
  .[,`:=`(Long = Coordinates %>%
            gsub(".*,", "", .) %>%
            as.numeric(),
          Lat = Coordinates %>%
            gsub(",.*", "", .)  %>%
            as.numeric())]

file_emme = "./data/TDM_EmmeLink_wCounts"
Metro = readOGR(file_emme, 
                "emme_link_wCounts",
                verbose = F)

file_subarea = "./data/I205_subarea_links"
subarea_raw = rgdal::readOGR(file_subarea, 
                             "links",
                             verbose = F)

#data munging~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#mapping utility~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~functions or indexes required for mapping
`%notin%` <- Negate(`%in%`)

simplifizeR = function(data){
  simplified_data = data %>% 
    .[!is.na(Location)] %>% 
    .[,head(.SD, 1), by = .(Location)] %>%
    mutate(Date = as_date(Floor_Timestamp),
           Weekday = lubridate::wday(Floor_Timestamp, label = T,abbr = F),
           Year = year(Floor_Timestamp))
  return(simplified_data)
}

spatializeR = function(data){
  coor_sp = cbind(data$Long, 
                  data$Lat) %>%  
    SpatialPoints() 
  crs.geo = CRS("+init=epsg:4326") 
  proj4string(coor_sp) = crs.geo 
  
  spatial_data = SpatialPointsDataFrame(coor_sp, 
                                        data %>% 
                                          as.data.frame())
  return(spatial_data)
}

mapatizeR = function(tm_shape_object, color){
  map = tm_shape_object +
    tm_dots(col = tmp_color,
            alpha = 1,
            id = "Location", 
            popup.vars=) 
  return(map)
}

popup_value = c("Source/Vendor" = "SRC",
                "Type",
                "Directionality",
                'Date',   
                "Day" = "Weekday",
                "Count_Precision" = 'Count_Fidelity', 
                'Veh_Data', 
                "Current_Status")

# scales::show_col(vapoRwave:::newRetro_palette)
tmp_color = vapoRwave:::newRetro_palette #[c(1,3,5, 7, 9)]

# projection converts most shapefiles to lat/long projceion
default_projection = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


#mapping processes~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~subarea 
#buffers need to be applied in stages to ensure smoothness
Sub_Area = subarea_raw %>%  
  st_as_sf() %>% 
  arrange(-Length) %>%
  head(2000) %>%
  st_buffer(dist = 4000) %>%
  st_union() 

Sub_Area = Sub_Area %>%
  st_buffer(dist = 6000) 

Sub_Area = Sub_Area %>%
  st_buffer(dist = 6000) 

Sub_Area = Sub_Area %>%
  st_buffer(dist = 6000) 

Sub_Area = Sub_Area %>%
  st_buffer(dist = -22000) 

Sub_Area = Sub_Area %>%
  as_Spatial() %>% 
  spTransform(., CRS(default_projection))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~extracted data
  Kept_Extracted_Data = CED_init_filter_and_agg %>% 
  .[!is.na(Lat)] %>% 
  .[Current_Status == "Kept"] %>% 
  simplifizeR() %>% 
  spatializeR() 

Kept_Extracted_Data = raster::intersect(Kept_Extracted_Data, Sub_Area)

Kept_Extracted_Data@data[, c('Location', 'Current_Status')] %>%  
  fwrite(., "./output/Kept_Extracted_Data_Lookup.csv")

# different layers
QC_Data = Kept_Extracted_Data %>%  
  filter(SRC == "QC")

ATD_Data = Kept_Extracted_Data %>%  
  filter(SRC == "ATD")

Clackamas_Data = Kept_Extracted_Data %>%  
  filter(SRC == "County")

ODOT_Data = Kept_Extracted_Data %>%  
  filter(SRC == "ODOT")

# want locations that have been dropped by date and spatial filters 
Dropped_Extracted_Data = CED_init_filter_and_agg %>% 
  .[!is.na(Lat)] %>% 
  .[Location %notin% unique(Kept_Extracted_Data$Location)] %>%  
  .[,`:=`(Current_Status = "Dropped")] %>%  
  simplifizeR() %>% 
  spatializeR() 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~focus area
# TODO make this more `form fitting`
focus_area_poly = cbind(focus_area_coordinates$Long,
                        focus_area_coordinates$Lat) %>%
  Polygon()
focus_area_poly = Polygons(list(focus_area_poly), "Focus_area")
focus_area_poly = SpatialPolygons(list(focus_area_poly))
crs.geo = CRS("+init=epsg:4326")
proj4string(focus_area_poly) = crs.geo

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~subarea roadways
subarea_sf_network = subarea_raw %>%  
  st_as_sf() %>% 
  st_union() 

#making map now to make later mapping faster 
subarea_sf_network_map = tm_shape(subarea_sf_network) +
  tm_lines(alpha = .2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~metro points
Metro = Metro %>% 
  spdplyr:::filter.Spatial(CountYr == 9999) %>% 
  SpatialLinesMidPoints()

Metro = Metro %>% 
  spTransform(., CRS(default_projection))

Kept_Metro_Data = raster::intersect(Metro, Sub_Area)

Dropped_Metro_Data = Metro %>% 
  spdplyr:::filter.Spatial(ID %notin% unique(Kept_Metro_Data$ID))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~raster of combined `kept` points
raster_extent = raster(ncols=18, nrows=18,
                       xmn = Sub_Area@bbox[1,1], xmx = Sub_Area@bbox[1,2],
                       ymn = Sub_Area@bbox[2,1], ymx = Sub_Area@bbox[2,2])

CED_kept_raster <- rasterize(Kept_Extracted_Data, raster_extent, "Location", fun='count')

Metro_kept_raster <- rasterize(Kept_Metro_Data, raster_extent, "ID", fun='count')

Kept_Combined_Data = merge(Metro_kept_raster, CED_kept_raster) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~external flag
print("mappr_utility.R run complete")