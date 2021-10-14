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

# library(ggmap) #ggplot2 for spatial 
install.packages("tmap") #vis for maps (ggmap alternative)
library(tmap)
# library(rgdal) #for inport/outport
# library(rgeos) #for spatial analysis operations 
# library(maptools) #provides mapping functions
# library(dplyr) #data manipulation 
library(sp)
# library(kableExtra)
# library(magrittr)
# library(raster) #geo-measurements 
# library(data.table)
# library(spdep)
# library(tidyverse)
# library(ggpubr)
# library(spdplyr)
# library(lubridate)
# library(readxl)
library(sf)
# library(leaflet)
# library(shiny)

library(utils)
remove.packages("tmap")

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
getwd()
#data inport~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

metro_speeds_times = "./data/Metro_links_with_travelTimes_and_speeds"
metro_speeds_times_links = rgdal::readOGR(metro_speeds_times, 
                             "Metro_NPMRDS_links_w_2016_INRIX_speeds",
                             verbose = F) %>% 
st_as_sf()

  
metro_st_data = metro_speeds_times_links@data %>%
  mutate(Lat = StartLat, 
         Long = StartLong) %>% 
  data.table() %>%  
  # .[,-c("geometry")] %>%  
  .[,`:=`(Lat = StartLat, 
          Long = StartLong)] 

coor_sp = cbind( 45.36434, -122.7596) %>%  
  SpatialPoints() 
crs.geo = CRS("+init=epsg:4326") 
proj4string(coor_sp) = crs.geo 

spatial_data = SpatialPointsDataFrame(coor_sp, 
                                      metro_st_data %>% 
                                        as.data.frame())
spatializeR(metro_st_data) %>% is.projected()
  tm_shape() +
  tm_dots()


latlong = "+init=epsg:4326"
  ukgrid = "+init=epsg:27700"
  google = "+init=epsg:3857"
  # Spatial*
  proj4string(SPDF)
  
  spTransform(metro_speeds_times_links, CRS(google)) %>% 
    tm_shape() +
    tm_lines()
  ## [1] NA
  proj4string(SPDF) = CRS(latlong)


  proj4string(metro_speeds_times_links) <- CRS("+init=epsg:4326") 
  metro_speeds_times_links %>% 
  tm_shape() +
  tm_lines()
  
  metro_speeds_times_links %>% projection()
  jer_aoi_WGS84 %>% is.projected()
  crs.geo = CRS("+init=epsg:4326") 
  proj4string(metro_speeds_times_links) = crs.geo 
  
  jer_aoi_WGS84 <- spTransform(metro_speeds_times_links,
                               crs.geo)
  
  
  metro_speeds_times_links %>%  st_transform(crs = crs.geo)

  
  
  
  p <- rbind(c(3.2,4), c(3,4.6), c(3.8,4.4), c(3.5,3.8), c(3.4,3.6), c(3.9,4.5))
  (mp <- st_multipoint(p))
  ## MULTIPOINT ((3.2 4), (3 4.6), (3.8 4.4), (3.5 3.8), (3.4 3.6), (3.9 4.5))
  s1 <- rbind(c(0,3),c(0,4),c(1,5),c(2,5))
  (ls <- st_linestring(s1))
  ## LINESTRING (0 3, 0 4, 1 5, 2 5)
  s2 <- rbind(c(0.2,3), c(0.2,4), c(1,4.8), c(2,4.8))
  s3 <- rbind(c(0,4.4), c(0.6,5))
  (mls <- st_multilinestring(list(s1,s2,s3)))
  ## MULTILINESTRING ((0 3, 0 4, 1 5, 2 5), (0.2 3, 0.2 4, 1 4.8, 2 4.8), (0 4.4, 0.6 5))
  p1 <- rbind(c(0,0), c(1,0), c(3,2), c(2,4), c(1,4), c(0,0))
  p2 <- rbind(c(1,1), c(1,2), c(2,2), c(1,1))
  pol <-st_polygon(list(p1,p2))
  p3 <- rbind(c(3,0), c(4,0), c(4,1), c(3,1), c(3,0))
  p4 <- rbind(c(3.3,0.3), c(3.8,0.3), c(3.8,0.8), c(3.3,0.8), c(3.3,0.3))[5:1,]
  p5 <- rbind(c(3,3), c(4,2), c(4,3), c(3,3))
  (mpol <- st_multipolygon(list(list(p1,p2), list(p3,p4), list(p5))))
  ## MULTIPOLYGON (((0 0, 1 0, 3 2, 2 4, 1 4, 0 0), (1 1, 1 2, 2 2, 1 1)), ((3 0, 4 0, 4 1, 3 1, 3 0), (3.3 0.3, 3.3 0.8, 3.8 0.8, 3.8 0.3, 3.3 0.3)), ((3 3, 4 2, 4 3, 3 3)))
  (gc <- st_geometrycollection(list(mp, mpol, ls)))
  
  
  nc <- st_read(system.file("shape/nc.shp", package="sf"),
               agr = c(AREA = "aggregate", PERIMETER = "aggregate", CNTY_ = "identity",
                       CNTY_ID = "identity", NAME = "identity", FIPS = "identity", FIPSNO = "identity",
                       CRESS_ID = "identity", BIR74 = "aggregate", SID74 = "aggregate", NWBIR74 = "aggregate",
                       BIR79 = "aggregate", SID79 = "aggregate", NWBIR79 = "aggregate"))
  
  st_agr(nc)
  (nc_geom <- st_geometry(nc))
  
  par(mar = c(0,0,1,0))
  plot(nc[1], reset = FALSE) # reset = FALSE: we want to add to a plot with a legend
  plot(nc[1,1], col = 'grey', add = TRUE)
  
  plot(metro_speeds_times_links[,1], reset = F) 
  ddd
  nc[,1] %>%  plot()
  nc[,1] %>%  tm_shape() + tm_polygons()
  install.packages("mapview")d
  nc[1] %>%  mapview::mapview()

  
  tmap_mode("view")
  tmap_mode("plot")
  data("World")
  
  wrld_1 = st_transform(metro_speeds_times_links, crs = 3857)
  wrld_1 = st_transform(World, crs = ukgrid)
  wrld_1 = st_transform(metro_speeds_times_links, crs = 4326)
  wrld_1 %>%  st_is_valid()
  wrld_1 %>%  st_is_longlat()
  metro_speeds_times_links
  st_geometry(wrld_1)
  
tm_shape(World) +
    tm_polygons("HPI")
  
  hello = tm_shape(wrld_1) +
    tm_lines()
  
  library(tmap)
  data(World, metro, rivers, land)
  
  tmap_mode("view")
  ## tmap mode set to plotting
  tm_shape(land) +
    tm_raster("elevation", palette = terrain.colors(10)) +
    tm_shape(World) +
    tm_borders("white", lwd = .5) +
    tm_text("iso_a3", size = "AREA") +
    tm_shape(metro) +
    tm_symbols(col = "red", size = "pop2020", scale = .5) +
    tm_legend(show = FALSE)

  
  tm_shape(subarea_network) +
    tm_lines() +
    tm_shape(wrld_1 %>%  st_jitter(factor = .001)) +
    tm_lines(col = "AADT", 
             palette = "viridis", 
             style ="sd", 
             scale = 2) 
  
  
  
  
  
  
  
  
  
  
  