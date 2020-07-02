#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Maps all data - locations inside and outside subarea
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: sources map_utility.R
#-------- script maps locations
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#package library~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(magrittr)

#path and data set-up~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# setwd("~/")
# rstudioapi::getSourceEditorContext()$path %>%
#   as.character() %>%
#   gsub("R.*","\\1", .) %>%
#   path.expand() %>%
#   setwd()

#sourcing script~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!exists("Kept_Extracted_Data")) {
  print("F I L E    S O U R C E D")
  suppressMessages({
    suppressWarnings({
      source("./R/mappr_utility.R")
    })
  })
  
}
print("F I L E S    P R E A S E N T")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~combining all plots
map_kept_data = tm_shape(QC_Data) +
  tm_dots(col = tmp_color[1],
          scale = 2,
          alpha = 1,
          id = "Location", 
          popup.vars = popup_value) +
  tm_shape(ATD_Data) +
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
          popup.vars = popup_value) + 
  tm_shape(Kept_Metro_Data) +
  tm_dots(col = tmp_color[1],
          scale = 2,
          alpha = 1,
          id = "Location", 
          popup.vars = "ID") 

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

print("mappr_full_data_coverage.R run complete")