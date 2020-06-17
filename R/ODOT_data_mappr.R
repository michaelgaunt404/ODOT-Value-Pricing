#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Main mapping script for ODOT I205 data QC process.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: script pulls from multiple data streams
#-------- script performs applies filtering to keep or drop files
#-------- script maps locations
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#package install and load~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

tmap_mode('view')

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
CED_init_filter_and_agg = fread("./output/CED_init_filter_and_agg.csv", 
                                stringsAsFactors = F) %>% 
  .[,`:=`(Floor_Timestamp = gsub("[[:alpha:]]", ' ', Floor_Timestamp) %>% 
            as_datetime())]

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
                             verbose = T)

#data munging~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~filtering
# CED_init_filter = combined_extracted_data %>%  
#   .[SRC != "ODOT"] %>% 
#   .[hour(Timestamp) %between% c(7,9) | 
#       hour(Timestamp) %between% c(16,18) & 
#       year(Timestamp) %between% c(2014,2016),`:=`(Current_Status = "Kept")] %>% 
#   bind_rows(., combined_extracted_data %>%  
#               .[SRC == "ODOT"])


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
            # legend.hist = TRUE,
            alpha = 1,
            # style = "cat",
            # breaks = seq(2008, 2019),
            id = "Location", 
            # title="Station Type",
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
  .[Current_Status == "Kept"] %>% 
  simplifizeR() %>% 
  spatializeR() 

Kept_Extracted_Data = intersect(Kept_Extracted_Data, Sub_Area)

Kept_Extracted_Data@data[, c('Location', 'Current_Status')] %>%  
  fwrite(., "./output/Kept_Extracted_Data_Lookup.csv")

# different layers
QC_Data = Kept_Extracted_Data %>%  
  filter(SRC == "QC")
  
Clackamas_Data = Kept_Extracted_Data %>%  
  filter(SRC == "County")

ODOT_Data = Kept_Extracted_Data %>%  
  filter(SRC == "ODOT")

# want locations that have been dropped by date and spatial filters 
Dropped_Extracted_Data = CED_init_filter_and_agg %>% 
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

Kept_Metro_Data = intersect(Metro, Sub_Area)

Dropped_Metro_Data = Metro %>% 
  spdplyr:::filter.Spatial(ID %notin% unique(Kept_Metro_Data$ID))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~raster of combined `kept` points
raster_extent = raster(ncols=18, nrows=18,
                       xmn = Sub_Area@bbox[1,1], xmx = Sub_Area@bbox[1,2],
                       ymn = Sub_Area@bbox[2,1], ymx = Sub_Area@bbox[2,2])

CED_kept_raster <- rasterize(Kept_Extracted_Data, raster_extent, "Location", fun='count')

Metro_kept_raster <- rasterize(Kept_Metro_Data, raster_extent, "ID", fun='count')

Kept_Combined_Data = merge(Metro_kept_raster, CED_kept_raster) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~combining all plots

map_kept_data = tm_shape(QC_Data) +
  tm_dots(col = tmp_color[1],
          scale = 2,
          alpha = 1,
          id = "Location", 
          popup.vars = popup_value) +
  tm_shape(Clackamas_Data) +
  tm_dots(col = tmp_color[1],
          scale = 2,
          alpha = 1,
          id = "Location", 
          popup.vars = popup_value) + 
  tm_shape(ODOT_Data) +
  tm_dots(col = tmp_color[1],
          scale = 2,
          alpha = 1,
          id = "Location", 
          popup.vars = popup_value)

map_dropped_data = tm_shape(Dropped_Extracted_Data) +
  tm_dots(col = tmp_color[6],
          alpha = 1,
          id = "Location",
          popup.vars = popup_value) +
  tm_shape(Dropped_Metro_Data) +
  tm_dots(col = tmp_color[6],
          alpha = 1,
          id = "Location")

map_base = subarea_sf_network_map +
  tm_shape(Sub_Area) +
  tm_polygons(col = tmp_color[7],
              border.col = tmp_color[7],
              border.lwd = 3,
              alpha = 0.05) +
tm_shape(focus_area_poly) +
  tm_polygons(col = "#903495",
              border.col = "#903495",
              alpha = .1) +
  tm_shape(Kept_Combined_Data) +
  tm_raster(alpha = .8, 
            palette = "-inferno") +
  tm_add_legend(title = 'Data Inclusion Status',
                type = 'fill',
                col = c("#9239F6", "#FF0076"), 
                labels  = c("Included", "Dropped")) 

master_plot = map_kept_data + map_dropped_data + map_base

lf = master_plot %>% 
  tmap_leaflet() %>% 
  leaflet::hideGroup(c("subarea_sf_network_map",
                       "Kept_Combined_Data", 
                       "Dropped_Extracted_Data", 
                       "Dropped_Metro_Data")) %>% 
  leaflet::addMarkers(-122.603654, 45.364644, popup = "Abernathy Bridge")

#link extraction~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~making SF objects
subarea_projection = subarea_raw %>%  
  proj4string()

subarea_network = subarea_raw %>%  
  st_as_sf() 

Kept_Extracted_Data_SF = Kept_Extracted_Data %>%  
  st_as_sf() %>%  
  st_transform(., CRS(subarea_projection)) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~extracting locatioin/link touches
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~given radius `100`
prox_features = st_is_within_distance(subarea_network, 
                                      Kept_Extracted_Data_SF, 
                                      dist = 100, 
                                      sparse = F) 

index_link_miss = Kept_Extracted_Data_SF$Location %>% 
  .[which(prox_features %>%  colSums() == 0)]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~extraction to DF
link_count_locations = prox_features %>% 
  data.table() %>%  
  setnames(Kept_Extracted_Data_SF$Location %>%  
             as.character()) %>% 
  .[,`:=`(Links = subarea_network$ID)] %>% 
  melt.data.table(id.vars = "Links", 
                  value.name = "Touch", 
                  variable.name = "Location") %>%  
  .[Touch == T] %>%  
  .[order(Location, Links)] 



link_count_locations %>%  
  .[,`:=`(Link_Number = 1)] %>%  
  .[,`:=`(Link_Number = cumsum(Link_Number)), by = Location] %>%  
  dcast.data.table(Location~Link_Number, value.var = "Links")
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~compress to vector 
index_link_mapping = prox_features %>%  
  as.integer() %>% 
  matrix(ncol = ncol(prox_features)) %>% 
  rowSums() > 0

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~quick map
subarea_network[index_link_mapping,] %>%
  tm_shape(.) +
  tm_lines(col = "blue") +
  tm_shape(Kept_Extracted_Data_SF %>% 
             st_buffer(100)) +
  tm_polygons(col = "red", 
              alpha = .1) 

subarea_network[index_link_mapping,]


















#reporting~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TODO think about this a lot more

# Kept_Extracted_Data@dat

yolo = bind_rows(Kept_Extracted_Data@data[,c("Location", "Current_Status", "Type")],
                 Dropped_Extracted_Data@data[,c("Location", "Current_Status", "Type")]) %>%  
  data.table() %>% 
  .[Location != ""]

yolo %>%  
  .[,.(Count = .N), by = .(Current_Status, Type)] %>%  
  ggplot() + 
  geom_col(aes(Current_Status, Count, fill = Type))

Extracted_Data_zones = bind_rows(intersect(Dropped_Extracted_Data, focus_area_poly)@data %>% 
                                            mutate(Zone_Location = "Inside focus area"),
                                          intersect(Dropped_Extracted_Data, Sub_Area)@data %>% 
                                            mutate(Zone_Location = "Inside sub-area"),
                                         intersect(Kept_Extracted_Data, focus_area_poly)@data %>% 
                                           mutate(Zone_Location = "Inside focus area"),
                                         intersect(Kept_Extracted_Data, Sub_Area)@data %>% 
                                           mutate(Zone_Location = "Inside sub-area")) %>% 
  data.table() %>% 
  .[,c("Location", "Current_Status", "Type", "Zone_Location")] %>%
  .[,`:=`(Source = "Extracted Source")]

Metro_Data_zones = bind_rows(intersect(Dropped_Metro_Data, focus_area_poly)@data %>% 
                               mutate(Zone_Location = "Inside focus area") %>% 
                               mutate(Current_Status = "Dropped"),
                             intersect(Dropped_Metro_Data, Sub_Area)@data %>% 
                               mutate(Zone_Location = "Inside sub-area") %>% 
                               mutate(Current_Status = "Dropped"),
                             intersect(Kept_Metro_Data, focus_area_poly)@data %>% 
                               mutate(Zone_Location = "Inside focus area") %>% 
                               mutate(Current_Status = "Kept"),
                             intersect(Kept_Metro_Data, Sub_Area)@data %>% 
                               mutate(Zone_Location = "Inside sub-area") %>% 
                               mutate(Current_Status = "Kept")) %>% 
  data.table() %>% 
  .[,c("ID", "Zone_Location", "Current_Status")] %>% 
  .[,`:=`(Source = "Metro")] %>%  
  setnames(old = "ID", new = "Location")

tmp = Extracted_Data_zones %>% 
  bind_rows(Metro_Data_zones)

tmp %>%  
  .[,.(Count = .N), by = .(Current_Status, Type)] %>%  
  ggplot() + 
  geom_col(aes(Current_Status, Count, fill = Type))

tmp %>%  
  .[,.(Count = .N), by = .(Source, Current_Status)] %>%  
  ggplot() + 
  geom_col(aes(Current_Status, Count, fill = Type))


#METRO DATA 
nrow(Metro) #total
nrow(Kept_Metro_Data) #kept
(nrow(Kept_Metro_Data)/nrow(Metro)) %>% 
  round(3)*100 #percentage

extracted_all = bind_rows(Dropped_Extracted_Data@data, 
          Kept_Extracted_Data@data) %>%  data.table()

#EXCTRACTED
nrow(extracted_all)
nrow(Kept_Extracted_Data)
(nrow(Kept_Extracted_Data)/nrow(extracted_all)) %>% 
  round(3)*100

#breakdowsn in the extracted data
#by source
combined_extracted_data %>% 
  .[,.(.N), by = SRC] %>%  
  .[,`:=`(Percent = 100*round(N/sum(N),3))] %>%
  .[order(-N)]

#by Type
extracted_all %>% 
  .[,.(.N), by = .(Type)] %>% 
  .[,`:=`(Percent = 100*round(N/sum(N),3))] %>%
  .[order(-N)]

# percent of the kept data that is currently in the focus area
(intersect(Kept_Extracted_Data, focus_area_poly)@data %>% 
  mutate(Zone_Location = "Inside focus area") %>%  
  nrow() / (nrow(Kept_Extracted_Data))) %>% 
  round(3)*100









