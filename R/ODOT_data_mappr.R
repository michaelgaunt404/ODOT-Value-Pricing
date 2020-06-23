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
library(sf)

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

master_plot = map_base + map_kept_data + map_dropped_data 

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

focus_area_poly_SF = focus_area_poly %>%  
  st_as_sf() %>% 
  st_transform(., CRS(subarea_projection))

Kept_Metro_Data_SF = Kept_Metro_Data %>%  
  st_as_sf() %>%  
  st_transform(., CRS(subarea_projection)) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~extracting link directions and headings
mod_fun <- function(object, x, y){
  object[x,y]
}

link_direction_extraction = subarea_network[,c("ID", "geometry")] %>% 
  group_by(ID) %>% 
  nest() %>%
  mutate(coor = map(data, st_coordinates)) %>% 
  mutate(coor_dim = map(coor, nrow)) %>% 
  mutate(start_x = map(coor, mod_fun, 1, 1), 
         end_x = map(coor, mod_fun, coor_dim[[1]], 1),
         start_y = map(coor, mod_fun, 1, 2), 
         end_y = map(coor, mod_fun, coor_dim[[1]], 2)) %>% 
  unnest(cols = c("start_x", 
                  "end_x",
                  "start_y", 
                  "end_y")) %>%  
  mutate(Heading_EW = end_x-start_x, 
         Heading_NS = end_y-start_y) %>%  
  mutate(Primary_Direction = ifelse(abs(Heading_NS) > abs(Heading_EW), 
                                    ifelse(Heading_NS > 0, "Northbound", "Southbound"), 
                                    ifelse(Heading_EW > 0, "Eastbound", "Westbound")), 
         Secondary_Direction = ifelse(abs(Heading_NS) > abs(Heading_EW),
                                      ifelse(Heading_EW > 0, "Eastbound", "Westbound"),
                                      ifelse(Heading_NS > 0, "Northbound", "Southbound"))) %>% 
  .[,c("ID", "start_x", "end_x", "start_y", "end_y",  "Heading_EW", 
       "Heading_NS", "Primary_Direction", "Secondary_Direction")] %>% 
  data.frame()  

subarea_network = link_direction_extraction %>% 
  merge(subarea_network, ., by = "ID") 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~extracting touching data locations and links 
index_link_miss = c(Kept_Extracted_Data_SF$Location)
link_count_locations = data.table()
index_link_mapping = c()
for (i in 1:3){
  distance = 100 * i
  
  proximity_features = Kept_Extracted_Data_SF %>% 
    .[Kept_Extracted_Data_SF$Location %in% index_link_miss,] %>% 
    st_is_within_distance(subarea_network, 
                          ., 
                          dist = distance, 
                          sparse = F) 
  
  link_count_locations = bind_rows(link_count_locations, 
                                    proximity_features %>% 
                                      data.table() %>%  
                                      setnames(index_link_miss %>%  
                                                 as.character()) %>% 
                                      .[,`:=`(Links = subarea_network$ID, 
                                              Distance = distance)] %>% 
                                      melt.data.table(id.vars = c("Links", "Distance"), 
                                                      value.name = "Touch", 
                                                      variable.name = "Location") %>%  
                                      .[Touch == T] %>%  
                                      .[order(Location, Links)] %>%  
                                      .[,`:=`(Link_Number = 1)] %>%
                                      .[,`:=`(Link_Number = cumsum(Link_Number)), by = Location]) 
  
  index_link_miss = Kept_Extracted_Data_SF$Location %>% 
    .[which(proximity_features %>%  colSums() == 0)]
  index_link_mapping = cbind(index_link_mapping, proximity_features) 
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~extraction to DF
yolo = Kept_Extracted_Data_SF %>%  
  .[,c("SRC", "Location", 'Type', 'Road', 'Heading')] %>%  
  merge(., link_count_locations %>% 
                     .[order(Distance, Location, -Link_Number)] %>% 
                     .[, .SD[1], by = .(Location, Distance)] %>% 
                     .[,-c("Links")], by = "Location")
 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~making map layers
index_link_mapping = index_link_mapping %>% 
  as.integer() %>% 
  matrix(ncol = ncol(index_link_mapping)) %>% 
  rowSums() > 0

SubArea_Network_Data_Links = subarea_network[index_link_mapping,]

#inside Focus Area
kept_points_inside = st_within(yolo, focus_area_poly_SF, sparse = FALSE)[, 1] %>% 
  yolo[.,]

QC_Data_Inside_FocusArea = kept_points_inside %>%  
  filter(SRC == "QC")

Clackamas_Data_Inside_FocusArea = kept_points_inside %>%  
  filter(SRC == "County")

ATD_Data_Inside_FocusArea = kept_points_inside %>%  
  filter(SRC == "ATD")

ODOT_Data_Inside_FocusArea = kept_points_inside %>%  
  filter(SRC == "ODOT")

Metro_Data_Inside_FocusArea = st_within(Kept_Metro_Data_SF, focus_area_poly_SF, sparse = FALSE)[, 1] %>% 
  Kept_Metro_Data_SF[.,]

Data_Links_Inside_FocusArea = st_intersects(SubArea_Network_Data_Links, focus_area_poly_SF, sparse = FALSE)[, 1] %>% 
  SubArea_Network_Data_Links[.,]

#outside Focus Area
kept_points_outside = st_disjoint(yolo, focus_area_poly_SF, sparse = FALSE)[, 1] %>% 
  yolo[.,]

QC_Data_Outside_FocusArea = kept_points_outside %>%  
  filter(SRC == "QC")

Clackamas_Data_Outside_FocusArea = kept_points_outside %>%  
  filter(SRC == "County")

ATD_Data_Outside_FocusArea = kept_points_outside %>%  
  filter(SRC == "ATD")

ODOT_Data_Outside_FocusArea = kept_points_outside %>%  
  filter(SRC == "ODOT")

Metro_Data_Outside_FocusArea = st_disjoint(Kept_Metro_Data_SF, focus_area_poly_SF, sparse = FALSE)[, 1] %>% 
  Kept_Metro_Data_SF[.,]

Data_Links_Outside_FocusArea = st_disjoint(SubArea_Network_Data_Links, focus_area_poly_SF, sparse = FALSE)[, 1] %>% 
  SubArea_Network_Data_Links[.,]

Focus_Area = focus_area_poly_SF

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~link extraction map
dot_mappr = function(data, i, pop){
  data + 
    tm_dots(col = tmp_color[i], scale = 3,  popup.vars = pop)
}

line_mappr = function(data, i, size, pop){
  data + 
    tm_lines(col = tmp_color[i], lwd = size, popup.vars = pop)
}

popup_dots = c('SRC', 'Type', 'Road', 
  'Heading', "Buffer" = 'Distance', 
  "Link_Number")

popup_lines = c("ID", 
  "Heading_EW",
  "Heading_NS", 
  "Primary_Direction", 
  "Secondary_Direction")

map_insde_FA =  tm_shape(QC_Data_Inside_FocusArea) %>% dot_mappr(., 9, popup_dots) +
  tm_shape(Clackamas_Data_Inside_FocusArea) %>% dot_mappr(., 8, popup_dots) +
  #tm_shape(ATD_Data_Inside_FocusArea) %>% dot_mappr(., 3, popup_dots) +
  tm_shape(ODOT_Data_Inside_FocusArea) %>% dot_mappr(., 6, popup_dots) +
  tm_shape(Metro_Data_Inside_FocusArea) %>% dot_mappr(., 2, "ID") +
  tm_shape(Data_Links_Inside_FocusArea) %>% line_mappr(., 1, 2, popup_lines)

map_outside_FA =  tm_shape(QC_Data_Outside_FocusArea) %>% dot_mappr(., 9, popup_dots) +
  tm_shape(Clackamas_Data_Outside_FocusArea) %>% dot_mappr(., 8, popup_dots) +
  tm_shape(ATD_Data_Outside_FocusArea) %>% dot_mappr(., 3, popup_dots) +
  tm_shape(ODOT_Data_Outside_FocusArea) %>% dot_mappr(., 6, popup_dots) +
  tm_shape(Metro_Data_Outside_FocusArea) %>% dot_mappr(., 2, "ID") +
  tm_shape(Data_Links_Outside_FocusArea) %>% line_mappr(., 1, 2, popup_lines) 
  
map_base_links = tm_shape(Focus_Area) + 
  tm_polygons(alpha = .3) +
  tm_add_legend(title = 'Data Source',
                type = 'fill',
                col = c(tmp_color[9], tmp_color[8], tmp_color[6], tmp_color[1], tmp_color[2]), 
                labels  = c("Qual Count", "Clackamas County", "ODOT", "Extracted TDA Links", "Metro Data")) 


master_plot_links = map_base_links + map_insde_FA + map_outside_FA 

lf_links = master_plot_links %>% 
  tmap_leaflet() %>% 
  leaflet::hideGroup(c("Clackamas_Data_Outside_FocusArea",
                       "QC_Data_Outside_FocusArea", 
                       "ATD_Data_Outside_FocusArea", 
                       "ODOT_Data_Outside_FocusArea",
                       "Metro_Data_Outside_FocusArea",
                       "Data_Links_Outside_FocusArea")) %>% 
  leaflet::addMarkers(-122.603654, 45.364644, popup = "Abernathy Bridge")
   


#link extraction~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
QC_extracted_Links = CED_init_filter_and_agg %>% 
  .[Current_Status == "Kept"] %>% 
  .[Location %in% unique(kept_points_inside$Location)] %>% 
  .[,c("SRC", "Location", 'Type', 'Road', 'Heading', "Intersection_Flow", "Directionality")] %>% 
  merge.data.table(.,link_count_locations %>% 
                     .[order(Distance, Location, -Link_Number)] %>% 
                     .[,-c("Touch")], by = "Location", allow.cartesian=TRUE) %>%
  merge.data.table(., subarea_network %>%  
                     data.table() %>%  
                     .[, .(ID, Primary_Direction, Secondary_Direction)], 
                   by.x = "Links", by.y = "ID") %>%  
  .[SRC != "ODOT"] %>% 
  unique() %>% 
  .[, .(Type, SRC, Location, Road, Directionality, Heading, Primary_Direction, 
        Intersection_Flow, Links, 
        Secondary_Direction, Distance, Link_Number)] %>% 
  .[order(Location, Road, Heading)]

QC_extracted_Links = bind_rows(QC_extracted_Links[Directionality == "Bidirectional"],
          QC_extracted_Links %>% 
            .[Directionality != "Bidirectional" &
                Heading == Primary_Direction])

QC_extracted_Links %>%  
  fwrite("./output/QC/link_QC.csv")

QC_extracted_Links_confirmed = fread("./output/QC/link_QC_confirmed.csv") %>%  
  .[QC_link == "Y", .(Location, Road, Directionality, Heading, Intersection_Flow, Links)] %>%  
  .[,`:=`(Sequence = 1)] %>%  
  .[,`:=`(Sequence = cumsum(Sequence)), by = .(Location, Road, 
                                               Heading, Intersection_Flow)] %>%  
  .[,`:=`(Sequence = paste0("Link_Number", Sequence))] %>% 
  dcast.data.table(Location+Road+Directionality+Heading+Intersection_Flow~Sequence, 
                   value.var = "Links", fill = NA)


CED_init_filter_and_agg %>% 
  .[Location %in% unique(kept_points_inside$Location)] %>%  
  merge.data.table(., QC_extracted_Links_confirmed, 
                   by = c("Location", "Road", "Directionality", "Heading", "Intersection_Flow"))

























































  

Data_Links_Inside_FocusArea_target = Data_Links_Inside_FocusArea %>% 
  filter(ID %in% QC_extracted_Links$Links) %>% 
  st_jitter(., factor = 0.001)


QC_MAP =  tm_shape(QC_Data_Inside_FocusArea) %>% dot_mappr(., 9, popup_dots) +
  tm_shape(Clackamas_Data_Inside_FocusArea) %>% dot_mappr(., 8, popup_dots) +
  tm_shape(ODOT_Data_Inside_FocusArea) %>% dot_mappr(., 6, popup_dots) +
  tm_shape(Data_Links_Inside_FocusArea_target) %>% line_mappr(., 1, 2, popup_lines) +
  tm_shape(Focus_Area) + 
  tm_polygons(alpha = .3)


tm_shape(Data_Links_Inside_FocusArea_target  %>% 
           filter(ID == "40477")) %>% line_mappr(., 1, 2, popup_lines)

tm_shape(SubArea_Network_Data_Links  %>% 
           filter(ID == "40477")) %>% line_mappr(., 1, 2, popup_lines) 

# tmp_QC = "BORLAND RD WEST OF DOLLAR"
# CED_init_filter_and_agg %>% 
#   .[Location == tmp_QC & 
#       Current_Status == "Kept" ] %>% 
#   .[, .(sum(Hourly_Counts)),
#     by = .(Floor_Timestamp, Intersection_Flow)] %>% 
#   dcast.data.table(Floor_Timestamp~Intersection_Flow, value.var = "V1")
# 
# CED_raw %>%
#   .[Location == tmp_QC] %>% 
#   .[str_detect(Timestamp, "T07") |
#       str_detect(Timestamp, "T17"), ] %>% 
#   .[,.(sum(Count)), by = Floor_Timestamp]
# 
# CED_raw %>%
#   .[Location == tmp_QC] %>% 
#   .[str_detect(Timestamp, "T07") |
#       str_detect(Timestamp, "T17"), ] %>% 
#   .[, .(Hourly_Counts = sum(Count, na.rm = T)), by = index_aggregator] %>% 
#   .[,.(sum(Hourly_Counts)), by = Floor_Timestamp]















subarea_network[subarea_network$Start %in% link_count_locations$Links] %>%
  tm_shape(.) +
  tm_lines(col = "blue") 

link_count_locations[Location == "BORLAND RD EAST OF STAFFORD"]

link_count_locations %>% 
  filter(Links %in% c("16603"))

link_direction_extraction %>% 
  filter(ID %in% c("8807"))

link_count_locations %>% 
  filter(Location %in% c("PETES MOUNTAIN RD SOUTH OF SCHAEFFER"))
  

subarea_network %>% 
  filter(ID %in% c("8807")) %>%
  tm_shape(.) +
  tm_lines(col = "blue") 

subarea_network %>% 
  filter(ID %in% c("8807")) %>%  
  st_cast("POINT") %>%  
  mutate(order = 10*as.numeric(rownames(.)), 
         geo_string_x = geometry %>% 
           as.character() %>% 
           gsub(",.*", "\\1", .) %>% 
           str_remove_all("\\(") %>% 
           str_remove_all("[[:alpha:]]") %>%  
           as.numeric(), 
         geo_string_y = geometry %>% 
           as.character() %>% 
           gsub(".*,", "\\1", .) %>% 
           str_remove_all("\\)") %>% 
           str_remove_all("[[:alpha:]]") %>%  
           as.numeric()) %>%  
  tm_shape(.) +
  tm_dots(col = "order", 
          popup.vars = c("ID", "order", "geo_string_x", 
                         "geo_string_y"))


subarea_network %>% 
  filter(ID %in% c("8807")) %>%  
  st_cast("POINT") 
  
subarea_network %>% 
  filter(ID %in% c("8807")) %>%  
  st_coordinates()





  
  

subarea_network %>% 
  filter(ID %in% c("15015")) %>% 
  st_coordinates() %>%
  nrow()
  c(head(.,1))

subarea_network$geometry[50] %>%  
  st_coordinates()

  st_cast("MULTIPOINT") %>% 
  tm_shape(.) +
  tm_dots(col = "blue") 
  
  subarea_network %>% 
    mutate(num_points = geometry %>% 
             st_coordinates() %>% 
             nrow())
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    
 

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









