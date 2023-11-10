# Review of the Closure Detour Analysis

library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")
library(tidycensus)


basemaps <- tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery"))

load("data/MoDOT_base_data.RData")
tier1 <- read_csv("Tier1_xings.csv") %>% pull(CrossingID)

master_sheet_adt <- readxl::read_excel("data/BaseDataForTeams 2023-03-13.xlsx", sheet = "Master Sheet") %>%
  select(CrossingID, `Final ADT V2`) %>% janitor::clean_names()

detour_shp <- read_sf("data/tier_1_overline_duration_fix.geojson")

detour_data <- read_csv("data/tier_1_summary_duration_fix.csv") %>% 
  pivot_wider(names_from = "scenario", values_from = c("distance", "duration")) %>% 
  transmute(
    crossing_id,
    extra_dist_per_veh = (distance_closure - distance_base) / trips,
    extra_dur_per_veh_min = (duration_closure - duration_base) / trips / 60
  ) %>% 
  left_join(master_sheet_adt) %>% 
  mutate(
    ex_veh_hr = round(extra_dur_per_veh_min * final_adt_v2 / 60, 2),
    ex_veh_mi = round(extra_dist_per_veh * final_adt_v2 / 1609.34, 2)
    ) %>% 
  mutate(text = "") %>% 
  arrange(ex_veh_hr)


focus_id <- "438535M"
basemaps + 
  tm_shape(detour_shp %>% filter(crossing_id == focus_id)) +
    tm_lines(col = "scenario", lwd = "trips", scale = 10, alpha = 0.5, palette = c("blue", "red")) +
  # tm_shape(detour_shp %>% filter(crossing_id == focus_id, scenario == "closure")) +
  #   tm_lines(col = "red", lwd = "trips", scale = 10, alpha = 0.5) +
  tm_facets(by = "scenario", sync = TRUE) +
  tm_shape(xings_pub_MO_amtrak %>% filter(CrossingID == focus_id)) + tm_dots()


detour_data[detour_data$crossing_id =="442821A", "text"] <- "Not viable closure due to flooding of underpass."
detour_data[detour_data$crossing_id =="442239H", "text"] <- "Already semi-permanent closure. Recommend permanent closure."
detour_data[detour_data$crossing_id =="438540J", "text"] <- "Good closure option. But monitor impacts of adjacent closures."
detour_data[detour_data$crossing_id =="438537B", "text"] <- "Good closure option. Relatively high impacts to vehicle miles traveled."
detour_data[detour_data$crossing_id =="438534F", "text"] <- "Possible closure option. Splits property on north side of roadway."
detour_data[detour_data$crossing_id =="803351T", "text"] <- "Not viable. Detour goes through private property."
detour_data[detour_data$crossing_id =="438535M", "text"] <- "Possible closure option. Splits property on both sides of roadway."
detour_data[detour_data$crossing_id =="445937L", "text"] <- "Not good option. Too much extra time and distance for relatively high adt (162)."
detour_data[detour_data$crossing_id =="445921P", "text"] <- "Possible closure option. Impacts local traffic, but not Replica estiamted traffic."
detour_data[detour_data$crossing_id =="438533Y", "text"] <- "Possible closure option. Check against adjacent closures."
detour_data[detour_data$crossing_id =="", "text"] <- ""



# Older Data --------------------------------------------------------------
load("//mspe-gis-file/GISProj/MissouriDOT/10366374_MoDOT_Rail_Corridors/7.2_WIP/HERE_analysis_4_11_2023/438543E.rda")

shp_old <- output_list[[2]]

tm_shape(shp_old) + tm_lines()


test_shp <- read_sf("//mspe-gis-file/GISProj/MissouriDOT/10366374_MoDOT_Rail_Corridors/7.2_WIP/HERE_analysis_5_2/raw_routed/438543E.geojson")

basemaps +
  tm_shape(test_shp) + tm_lines(col = "scenario", palette = "Set1") + tm_facets(by = "scenario")
                       

t1_recs <- readxl::read_excel("C:/Users/frryan/Desktop/_Working Files/MoDOT Rail Corridor Studies/Tier 1 Recs v3.xlsx", sheet = "Recs") %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326)

basemaps + 
  tm_shape(rail_amtrak) + tm_lines() +
  tm_shape(t1_recs) + tm_dots(col = "Rec Desc", palette = "Set1", size = 0.1, popup.vars = TRUE)
