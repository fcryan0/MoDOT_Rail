library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")
tmap_options(check.and.fix = TRUE)

basemaps <- tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery"))

load("data/MoDOT_base_data.RData")

st_layers("//ksc-gis01/Data1/MoDOT/RR Safety Study/RR Safety Study_ArcGISPro/7.2_WIP/spatial/gdb/HDR_Project.gdb")

parcels <- 
  read_sf(
    "//ksc-gis01/Data1/MoDOT/RR Safety Study/RR Safety Study_ArcGISPro/7.2_WIP/spatial/gdb/HDR_Project.gdb",
    layer = "UP_RR_Xings_Parcels")

parcels %>% write_sf("FinalParcels_MoDOT.kml")

basemaps +
  tm_shape(parcels) + tm_polygons(alpha = 0.2) + +tm_borders() +
  tm_shape(xings_pub_MO_amtrak %>% filter(CrossingID == "442092K")) + tm_dots(col = "red")
