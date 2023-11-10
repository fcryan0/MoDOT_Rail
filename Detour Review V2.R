# Review of the Closure Detour Analysis
# Revised on 2023-06-01 after finalizing the new detour methodology

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

detour_stats <- 
  read_csv("data/summary_stats_5_2_2023.csv") %>% 
  select(crossing_id, trips, avg_chg_duration, max_chg_duration, min_chg_duration) %>% 
  right_join(master_sheet_adt) %>% 
  mutate(
    veh_hrs = round(avg_chg_duration / 60 / 60 * final_adt_v2, 2)
  ) %>% 
  filter(!is.na(crossing_id))



xings_pub_MO_amtrak <- 
  xings_pub_MO_amtrak %>% 
  left_join(detour_stats, by = c("CrossingID" = "crossing_id"))



detour_overlines <- read_sf("data/overlines_5_2_2023.geojson")

# Review of individual detours
# 442252W
# 441995T
# 442252W
# 441935J
# 445933J
# 442259U (most delay)
# 442829E (most delay with ADT < 2000)
crossing_id_focus <-"441935J"
overlines_x <- detour_overlines %>% filter(crossing_id == crossing_id_focus)
basemaps +
  tm_shape(overlines_x) + 
      tm_lines(col = "trips", lwd = "trips", popup.vars = TRUE, scale = 10, palette = "-Spectral") +
  tm_facets(by = "scenario", sync = TRUE, ncol = 2) +
  tm_shape(xings_pub_MO_amtrak %>% filter(CrossingID == crossing_id_focus) %>% select(CrossingID, avg_chg_duration, final_adt_v2, veh_hrs)) + 
      tm_dots(col = "red", size = 0.5)





detour_stats %>% write_csv("Detour_Redo.csv")


stats <- detour_stats %>% right_join(xings_pub_MO_amtrak %>% select(crossing_id = CrossingID)) %>% st_as_sf()
basemaps + 
  tm_shape(stats) + tm_dots(col = "veh_hrs")




    # detour_stats %>% filter(dur_min_delta < 0) %>% write_csv("Crossing_detour_redo.csv")
    # 
    # 
    # 
    # 
    # t1_recs <- readxl::read_excel("C:/Users/frryan/Desktop/_Working Files/MoDOT Rail Corridor Studies/Tier 1 Recs v3.xlsx", sheet = "Recs") %>% 
    #   st_as_sf(coords = c("x", "y"), crs = 4326)
    # 
    # basemaps + 
    #   tm_shape(rail_amtrak) + tm_lines() +
    #   tm_shape(t1_recs) + tm_dots(col = "Rec Desc", palette = "Set1", size = 0.1, popup.vars = TRUE)
