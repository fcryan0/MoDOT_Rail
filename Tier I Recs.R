

library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")
library(httr)
library(openxlsx)

basemaps <- tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery"))

load("data/MoDOT_base_data.RData")

recs <- readxl::read_excel("Tier 1 Recs.xlsx") %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326)

basemaps + 
  tm_shape(rail_amtrak %>% filter(AmtrakLine != "Southwest Chief")) + tm_lines() +
  tm_shape(recs) + tm_dots(col = "Rec Desc", palette = "Set1", size = 0.5, popup.vars = c("Rec Desc", "CrossingID")
