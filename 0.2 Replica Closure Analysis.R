# Data Review for the Replica Analysis of closure impacts

library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")

basemaps <- tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery"))

files <- 
  dir("//mspe-gis-file/GISProj/MissouriDOT/10366374_MoDOT_Rail_Corridors/7.2_WIP/crossing_closure_analysis",
      full.names = TRUE)

files[24]

load(files[1])

time_diff <- output_list[[1]]
data <- output_list[[2]]

basemaps +
  tm_shape(data) + tm_lines(col = "trips", lwd = "trips", scale = 5, style = "log10") +
  tm_facets(by = "scenario", sync = TRUE, nrow = 1, ncol = 2)

