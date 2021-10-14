#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Maps data locations, extracts links, writes out data for DYNA-MECH
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: sources map_utility.R
#-------- script maps locations
#-------- script extracts DTA links based off data locations
#-------- script writes out links and locations
#-------- manual QC process takes place to validate
#-------- script loads QC link/locations mappings and writes out for model use
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#package library~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(magrittr)

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


#sourcing script~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!exists("Kept_Extracted_Data")) {
  print("F I L E    S O U R C E D")
  suppressMessages({
    suppressWarnings({
      source("./R/mappr_utility.R")
    })
  })
  
} else {
  print("F I L E S    P R E A S E N T")
}

#link extraction~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~making SF objects
subarea_projection = subarea_raw %>%  
  proj4string()

subarea_network = subarea_raw %>%  
  st_as_sf() 

Kept_Extracted_Data_SF = Kept_Extracted_Data@data %>%
  bind_rows(., potential_data_pickups) %>% 
  spatializeR() %>%
  st_as_sf() %>%  
  st_transform(., CRS(subarea_projection)) 

Kept_Metro_Data_SF = Kept_Metro_Data %>%
  st_as_sf() %>%  
  st_transform(., CRS(subarea_projection)) %>% 
  mutate(Location = ID, 
         SRC = "Metro") %>% 
  dplyr::select(Location, SRC)

focus_area_poly_SF = focus_area_poly %>%  
  st_as_sf() %>% 
  st_transform(., CRS(subarea_projection))

calibration_links = read_excel("./data/links/calibrationLinkAttribs.xlsx") %>%  
  data.table() %>%
  melt.data.table(measure.vars = c(11:18), 
                  value.name = "Count") %>%  
  tidyr::separate(., variable, sep="_", into=c("Count_Type", "Hour")) %>%  
  spread(., key = Count_Type, value  = Count) %>% 
  data.table() %>% 
  .[,`:=`(error = 100*round((calc-obs)/obs,3), 
          Hour = paste0(Hour, ifelse(str_detect(Hour, "7|8|9"),"am", "pm")))] %>%
  modify_at(c("obs", "calc", "error"), 
            function(x) round(x,1)) %>%
  .[order(link)]
  # .[,`:=`(abs_error = abs(error))] %>%
  # .[,`:=`(mean_error = mean(error),
  #         mean_abs_error = mean(abs_error)), by = .(link)] %>% 
  # .[,`:=`(mean_abs_error_ampm = mean(abs_error)), by = .(link, str_trunc(Hour, 2, "left", ellipsis = "") %>% 
  #                                                          as.factor())] %>%  
  # modify_at(c("error", "abs_error", "mean_error", "mean_abs_error", "mean_abs_error_ampm"), function(x) 100*round(x,3)) %>% 


# calibration_links_sf = subarea_network %>%  
#   merge(., calibration_links, by.x, "ID", by.y = "link")

cl_error = calibration_links %>%  
  .[, .(link, fromDescr, toDescr, Hour, error)] %>% 
  pivot_wider(names_from = "Hour", values_from  = "error", names_prefix = "error_") %>% 
  data.table() %>% 
  .[,-c('fromDescr', 'toDescr')]

cl_calc = calibration_links %>%  
  .[, .(link, fromDescr, toDescr, Hour, calc)] %>% 
  pivot_wider(names_from = "Hour", values_from  = "calc", names_prefix = "calc_")  %>% 
  data.table() %>% 
  .[,-c('fromDescr', 'toDescr')]

cl_obs = calibration_links %>%  
  .[, .(link, fromDescr, toDescr, Hour, obs)] %>% 
  pivot_wider(names_from = "Hour", values_from  = "obs", names_prefix = "obs_") %>% 
  data.table() %>% 
  .[,-c('fromDescr', 'toDescr')]

calibration_links_sf = calibration_links %>%
  .[,-c("Hour", "calc", "obs", "error")] %>% 
  merge(., cl_error, by = "link") %>%
  merge(., cl_calc, by = "link") %>%
  merge(., cl_obs, by = "link") %>% 
  mutate("7-8am" = paste(`calc_7-8am`, `obs_7-8am`, `error_7-8am`, sep = "__"),
         "8-9am" = paste(`calc_8-9am`, `obs_8-9am`, `error_8-9am`, sep = "__"),
         "4-5pm" = paste(`calc_4-5pm`, `obs_4-5pm`, `error_4-5pm`, sep = "__"), 
         "5-6pm" = paste(`calc_5-6pm`, `obs_5-6pm`, `error_5-6pm`, sep = "__"),
         "Hour_Counts_(below)~~~~~~~~~~~~~~" = paste("Calc.__Observed__Error"),
         color = is.na(`error_5-6pm`)) %>% 
  unique() %>% 
  merge(subarea_network, ., by.x, "ID", by.y = "link")  %>% 
  st_jitter(factor = .001)

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
# F O R   M E T R O 
index_link_miss = c(as.character(Kept_Metro_Data_SF$Location))
link_count_locations_metro = data.table()
index_link_mapping = c()
for (i in 1:3){
  distance = 100 * i
  
  proximity_features = Kept_Metro_Data_SF %>% 
    .[Kept_Metro_Data_SF$Location %in% index_link_miss,] %>%
    st_is_within_distance(subarea_network, 
                          ., 
                          dist = distance, 
                          sparse = F) 
  
  link_count_locations_metro = bind_rows(link_count_locations_metro, 
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
  
  index_link_miss = Kept_Metro_Data_SF$Location %>% 
    .[which(proximity_features %>%  colSums() == 0)]
  index_link_mapping = cbind(index_link_mapping, proximity_features) 
}

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
  merge(., link_count_locations %>% 
          .[order(Distance, Location, -Link_Number)], by = "Location") %>%  
  mutate(Year = as.factor(Year))

yolo_potential = yolo %>%  
  filter(SRC == "Potential")

yolo = yolo %>%  
  filter(SRC != "Potential") 

SubArea_Network_Data_Links_Potetnial_Purchase = subarea_network %>%  
  filter(ID %in% yolo_potential$Links)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~making map layers
index_link_mapping = index_link_mapping %>% 
  as.integer() %>% 
  matrix(ncol = ncol(index_link_mapping)) %>% 
  rowSums() > 0

SubArea_Network_Data_Links = subarea_network[index_link_mapping,] %>%  
  filter(ID %in% yolo$Links)

#inside Focus Area
kept_points_inside = st_within(yolo, focus_area_poly_SF, sparse = FALSE)[, 1] %>% 
  yolo[.,]  

kept_points_outside_filter_pass = yolo %>%  
  filter(Location %in% selected_out_focus_area$Location)

QC_Data_Inside_FocusArea = kept_points_inside %>%
  filter(SRC == "QC")

Clackamas_Data_Inside_FocusArea = kept_points_inside %>%
  filter(SRC == "County")

ATD_Data_Inside_FocusArea = kept_points_inside %>%  
  filter(SRC == "ATD")

ODOT_Data_Inside_FocusArea = sf:::rbind.sf(kept_points_inside %>%  
                                             filter(SRC == "ODOT"), kept_points_outside_filter_pass %>%  
                                             filter(SRC == "ODOT"))

Metro_Data_Inside_FocusArea = st_within(Kept_Metro_Data_SF, focus_area_poly_SF, sparse = FALSE)[, 1] %>% 
  Kept_Metro_Data_SF[.,]

tmp_metro_links = link_count_locations_metro %>% 
  filter(Location %in% Metro_Data_Inside_FocusArea$Location) %>%  
  dplyr::select(Links)

Metro_Links_Inside_Focus_Area = subarea_network %>% 
  filter(ID %in% tmp_metro_links$Links)

Local_Data_Inside_Focus_Area = sf:::rbind.sf(QC_Data_Inside_FocusArea, 
                                             Clackamas_Data_Inside_FocusArea,
                                             ATD_Data_Inside_FocusArea, 
                                             kept_points_outside_filter_pass %>%  
                                               filter(SRC != "ODOT")) %>%  
  mutate(SRC = "Local")

Data_Links_Inside_FocusArea = SubArea_Network_Data_Links %>% 
  filter(ID %in% Local_Data_Inside_Focus_Area$Links |
           ID %in% ODOT_Data_Inside_FocusArea$Links) %>%  
  sf:::rbind.sf(., Metro_Links_Inside_Focus_Area)

#outside Focus Area
kept_points_outside = st_disjoint(yolo, focus_area_poly_SF, sparse = FALSE)[, 1] %>% 
  yolo[.,]

QC_Data_Outside_FocusArea = kept_points_outside %>%
  filter(SRC == "QC")

Clackamas_Data_Outside_FocusArea = kept_points_outside %>%
  filter(SRC == "County")

ATD_Data_Outside_FocusArea = kept_points_outside %>%  
  filter(SRC == "ATD")

Local_Data_Outside_Focus_Area = sf:::rbind.sf(QC_Data_Outside_FocusArea, 
                                              Clackamas_Data_Outside_FocusArea,
                                              ATD_Data_Outside_FocusArea) %>%  
  mutate(SRC = "Local")

ODOT_Data_Outside_FocusArea = kept_points_outside %>%  
  filter(SRC == "ODOT")

Metro_Data_Outside_FocusArea = st_disjoint(Kept_Metro_Data_SF, focus_area_poly_SF, sparse = FALSE)[, 1] %>% 
  Kept_Metro_Data_SF[.,]

tmp_metro_links_out = link_count_locations_metro %>% 
  filter(Location %in% Metro_Data_Outside_FocusArea$Location) %>%  
  dplyr::select(Links)

Metro_Links_Outside_Focus_Area = subarea_network %>% 
  filter(ID %in% tmp_metro_links_out$Links)

Data_Links_Outside_FocusArea = SubArea_Network_Data_Links %>%  
  filter(ID %notin% Data_Links_Inside_FocusArea$ID) %>%  
  sf:::rbind.sf(., Metro_Links_Outside_Focus_Area)

#aux 
Focus_Area = focus_area_poly_SF

Candidate_Local_Data = yolo_potential

Candidate_Local_Data_Links = SubArea_Network_Data_Links_Potetnial_Purchase %>% 
  filter(ID %in% yolo_potential$Links)

ATR_Data = subarea_network %>%  
  filter(ID %in% c(18023))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~link extraction map
tmp_viridis = viridis::plasma(15)[seq(from = 1, by = 3, length.out = 5 )]

dot_mappr = function(data, i, pop){
  if (is.numeric(i)){
    data + 
      tm_dots(col = tmp_color[i], scale = 3,  popup.vars = pop)
  } else {
    data + 
      tm_dots(col = "Year", scale = 3,  
              style = "cat",
              palette = tmp_viridis,
              breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019),
              popup.vars = pop,
              legend.show=FALSE,
              legend.format=list(fun=function(x) formatC(x, digits=0, format="d")))
  }
  
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

map_base_links = subarea_sf_network_map + 
  tm_shape(Focus_Area) + 
  tm_polygons(alpha = .6)

map_insde_FA =  tm_shape(Local_Data_Inside_Focus_Area) %>% dot_mappr(., 9, popup_value) +
  tm_shape(ODOT_Data_Inside_FocusArea) %>% dot_mappr(., 6, popup_value) +
  tm_shape(Metro_Data_Inside_FocusArea) %>% dot_mappr(., 1, "Location") +
  tm_shape(Data_Links_Inside_FocusArea) %>% line_mappr(., 2, 2, popup_lines) +
  tm_shape(ATR_Data)  %>% line_mappr(., 8, 2, "ID")

map_outside_FA =  tm_shape(Local_Data_Outside_Focus_Area) %>% dot_mappr(., 9, popup_value) +
  # tm_shape(ODOT_Data_Outside_FocusArea) %>% dot_mappr(., 6, popup_value) +
  tm_shape(Metro_Data_Outside_FocusArea) %>% dot_mappr(., 1, "Location") +
  tm_shape(Data_Links_Outside_FocusArea) %>% line_mappr(., 2, 2, popup_lines) 

map_pickup = tm_shape(Candidate_Local_Data) %>% dot_mappr(., 4, popup_value) +
  tm_shape(Candidate_Local_Data_Links) %>% line_mappr(., 4, 2, popup_lines)

master_plot_links = map_base_links +
  map_insde_FA + 
  map_outside_FA  + 
  map_pickup +
  tm_add_legend(title = 'Data Source',
                type = 'fill',
                col = c(tmp_color[9], tmp_color[8], tmp_color[6], tmp_color[1], tmp_color[2], tmp_color[4]), 
                labels  = c("Local Data Counts", "ATR Data", "ODOT Counts", "Metro Data", "Extracted TDA Links", "Candidate Data Sites/Links")) +
  tm_shape(calibration_links_sf) +
  tm_lines(col = "color",
           title.col = "Missing Calibration Counts",
           palette = c("#296656", "#00B19D"),
           lwd = 4,
           popup.vars = c("From" = "fromDescr",
                          "To" = "toDescr",
                          "Type" = "type", 
                          "Direction" = "dir", 
                          "Dir. Count" = "countDirs",
                          "Year" = "obs_date",
                          "Hour_Counts_(below)~~~~~~~~~~~~~~",
                          "7-8am", 
                          "8-9am",
                          "4-5pm", 
                          "5-6pm"))

lf_links = master_plot_links %>% 
  tmap_leaflet() %>% 
  leaflet::hideGroup(c("Local_Data_Outside_Focus_Area", 
                       "ODOT_Data_Outside_FocusArea",
                       "Metro_Data_Outside_FocusArea",
                       "Data_Links_Outside_FocusArea",
                       "Candidate_Local_Data",
                       "Candidate_Local_Data_Links",
                       'calibration_links_sf')) %>% 
  leaflet::addMarkers(-122.603654, 45.364644, popup = "Abernethy Bridge")

#map withb years as color=======================================================
map_insde_FA_color =  tm_shape(Local_Data_Inside_Focus_Area) %>% dot_mappr(., "y", popup_value) +
  tm_shape(ODOT_Data_Inside_FocusArea) %>% dot_mappr(., "y", popup_value) +
  tm_shape(Metro_Data_Inside_FocusArea) %>% dot_mappr(., 1, "Location") +
  tm_shape(Data_Links_Inside_FocusArea) %>% line_mappr(., 2, 2, popup_lines) +
  tm_shape(ATR_Data)  %>% line_mappr(., 8, 2, "ID")


map_outside_FA_color =  tm_shape(Local_Data_Outside_Focus_Area) %>% dot_mappr(., "y", popup_value) +
  tm_shape(Metro_Data_Outside_FocusArea) %>% dot_mappr(., 1, "Location") +
  tm_shape(Data_Links_Outside_FocusArea) %>% line_mappr(., 2, 2, popup_lines)

map_pickup_color = tm_shape(Candidate_Local_Data) %>% dot_mappr(., "y", popup_value) +
  tm_shape(Candidate_Local_Data_Links) %>% line_mappr(., 4, 2, popup_lines)

master_plot_links_color = map_base_links +
  map_insde_FA_color +
  map_outside_FA_color  +
  map_pickup_color +
  tm_add_legend(title = 'Year',
                type = 'fill',
                # legend.format=list(fun=function(x) formatC(x, digits=0, format="d"))
                col = tmp_viridis,
                labels  = levels(yolo$Year))

lf_links_color = master_plot_links_color %>%
  tmap_leaflet() %>%
  leaflet::hideGroup(c("Local_Data_Outside_Focus_Area",
                       "ODOT_Data_Outside_FocusArea",
                       "Metro_Data_Outside_FocusArea",
                       "Data_Links_Outside_FocusArea",
                       "Candidate_Local_Data",
                       "Candidate_Local_Data_Links")) %>%
  leaflet::addMarkers(-122.603654, 45.364644, popup = "Abernethy Bridge")



#link extraction================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# if (!exists("BEING_SOURCED_FROM_SOMEWHERE")){
#   
#   QC_extracted_Links = CED_init_filter_and_agg %>%
#     .[Current_Status == "Kept"] %>%
#     .[Location %in% unique(kept_points_inside$Location) |
#         Location %in% unique(kept_points_outside_filter_pass$Location)] %>%
#     .[,c("SRC", "Location", 'Type', 'Road', 'Heading', "Intersection_Flow", "Directionality")] %>%
#     merge.data.table(.,link_count_locations %>%
#                        .[order(Distance, Location, -Link_Number)] %>%
#                        .[,-c("Touch")], by = "Location", allow.cartesian=TRUE) %>%
#     merge.data.table(., subarea_network %>%
#                        data.table() %>%
#                        .[, .(ID, Start, End, Primary_Direction, Secondary_Direction)],
#                      by.x = "Links", by.y = "ID") %>% 
#     unique() %>%
#     .[, .(Type, SRC, Location, Road, Directionality, Heading, Primary_Direction,
#           Intersection_Flow, Links,
#           Secondary_Direction, Distance, Link_Number, Start, End)] %>%
#     .[order(Location, Road, Heading)]
#   
#   QC_extracted_Links = bind_rows(QC_extracted_Links[Directionality == "Bidirectional"],
#                                  QC_extracted_Links %>%
#                                    .[Directionality != "Bidirectional" &
#                                        Heading == Primary_Direction])
#   
#   QC_extracted_Links %>%
#     fwrite("./output/QC/link_QC.csv")
#   
#   QC_extracted_Links_confirmed = fread("./output/QC/link_QC_confirmed.csv") %>% 
#     .[QC_link == "Y", .(Location, Road, Directionality, Heading, Intersection_Flow, Links, Start, End)] %>%
#     unique() %>% 
#     .[,`:=`(Sequence = 1)] %>%
#     .[,`:=`(Sequence = cumsum(Sequence)), by = .(Location, Road,
#                                                  Heading, Intersection_Flow)] %>%
#     .[,`:=`(Sequence = paste0("Link_Number", Sequence))] 
#   
#   extracted_local_data_kept = CED_init_filter_and_agg %>%
#     .[Current_Status == "Kept"] %>%
#     .[Location %in% unique(QC_extracted_Links$Location)] %>%
#     merge.data.table(., QC_extracted_Links_confirmed,
#                      by = c("Location", "Road", "Directionality", "Heading", "Intersection_Flow")) %>%
#     .[, -c("SRC", "Type", 'Current_Status', 'Current_Status', 'Count_Fidelity', 'Veh_Data', 'Veh_Type',
#            'Current_time', 'Current_year')] %>%
#     .[,`:=`(year = year(Floor_Timestamp),
#             Hour = hour(Floor_Timestamp))] %>%
#     .[,`:=`(Hour = ifelse(Hour>12,
#                           paste0(Hour-12,"-",Hour-11, "pm"), 
#                           paste0(Hour,"-",Hour+1, "am")))] %>% 
#     .[,-c("Floor_Timestamp")] %>%
#     dcast.data.table(...~Hour, value.var = "Hourly_Counts") %>%  
#     .[,`:=`(focus = ifelse(Location %in% unique(kept_points_inside$Location), "2", "1"), 
#             supplier = "3", 
#             dirs = ifelse(Directionality == "Bidirectional", "2", "1"))] %>% 
#     setnames(old = c('Start', "End", "Links"), new = c("from", "to", "dynameq link id")) %>%  
#     .[,c(9, 10, 16:18, 13:15, 20, 21, 19, 12, 1, 8)]
#   
#   extracted_local_data_kept %>%
#     fwrite("./output/extracted_local_data_kept.csv")
#   
# }

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~inscript QC
# CED_init_filter_and_agg$Floor_Timestamp %>%
#   format("%I%p")
# 
# Data_Links_Inside_FocusArea_target = Data_Links_Inside_FocusArea %>%
#   filter(ID %in% QC_extracted_Links$Links) %>%
#   st_jitter(., factor = 0.00)
# 
# QC_MAP =  tm_shape(QC_Data_Inside_FocusArea) %>% dot_mappr(., 9, popup_dots) +
#   tm_shape(Clackamas_Data_Inside_FocusArea) %>% dot_mappr(., 8, popup_dots) +
#   tm_shape(ODOT_Data_Inside_FocusArea) %>% dot_mappr(., 6, popup_dots) +
#   tm_shape(Data_Links_Inside_FocusArea_target) %>% line_mappr(., 1, 2, popup_lines) +
#   tm_shape(Focus_Area) +
#   tm_polygons(alpha = .3)
# 
# CED_init_filter_and_agg %>%  
#   .[Location == "82ND DR NORTH OF JENNIFER", 
#     .(Floor_Timestamp, Hourly_Counts)]
# 
# CED_init_filter_and_agg %>%  
#   .[Location == "Pilkington Rd -- Jean Rd", 
#     .(Floor_Timestamp, Road, Heading, Intersection_Flow, Hourly_Counts)] %>%  
#   .[,.(sum(Hourly_Counts)), by = .(Floor_Timestamp, Intersection_Flow)]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~external flag
print("mappr_link_extractr.R run complete")