

library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")
library(httr)
library(openxlsx)

basemaps <- tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery"))
    

crossings <- 
  readxl::read_excel("data/BaseDataForTeams 2023-03-13.xlsx", sheet = "Master Sheet") %>% 
  select(AmtrakLine, Notes, order, CrossingID, Street, x, y, 
         FieldRev = `Field Review Needed`, FieldNotes = `Initial Crossing Review Notes`) %>% 
  filter(~is.na(x)) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326)

crossings_field <- crossings %>% filter(
  is.na(Notes),
  FieldRev == 1
)

basemaps + 
  tm_shape(crossings) + tm_dots(col = "gray") +
  tm_shape(crossings_field) + tm_dots(col = "red")
  
crossings_field %>% write_sf("FieldReviewCrossings.kml")
crossings_field %>% mutate(review_status = 0) %>% write_sf("FieldReviewCrossings.shp")
