# Script for summarizing the current recommendations for crossing improvements
# MoDOT Passenger Rail Corridor Study

library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")
library(httr)
library(openxlsx)

basemaps <- tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery"))

load("data/MoDOT_base_data.RData")

# Pull in current crossing recommendation sheet

master_sheet <- 
  readxl::read_excel("data/BaseDataForTeams 2023-05-17.xlsx", sheet = "Master Sheet") %>% 
  filter(
    !is.na(Railroad), 
    Notes != "Exclude" | is.na(Notes)
    )

glimpse(master_sheet)

ms_mrr <- master_sheet %>% filter(AmtrakLine == "Missouri River Runner")
ms_te <- master_sheet %>% filter(AmtrakLine == "Texas Eagle")

ms_mrr %>% count(Railroad)
ms_te %>% count(Railroad)

ms_mrr %>% filter(is.na(Notes)) %>% count(Subdivision)



# Crossing Owner
tm_shape(gcisCurrent_MoDOT) + tm_dots(col = "XingOwnr", palette = "Set1")


# Crash Summary
ms_mrr_crash <- gcisAccHist %>% filter(GXID %in% ms_mrr$CrossingID, IYR >= 18)
ms_mrr_crash %>% write_csv("ms_mrr_crash.csv")
ms_mrr_passive <- ms_mrr %>% filter(!WarnDev %in% c("Gates", "GatesWithMedian")) %>% select(CrossingID)
ms_mrr_crash %>% filter(GXID %in% ms_mrr_passive)




crossing_recs %>% count(Railroad)
crossing_recs %>% count(XingOwnr)

basemaps +
  tm_shape(crossing_recs) + tm_dots(col = "Railroad")



# Diagnostic Review Itinerary for Passive Crossings -----------------------

crossings %>% st_drop_geometry() %>% count(warn_dev)

crossings_passive <- crossing_recs %>% filter(!warn_dev %in% c("FlashingOnly", "Gates", "GatesWithMedian"))

basemaps +
  tm_shape(crossings_passive) + tm_dots(col = "Railroad")

crossings_passive %>% 
  mutate(name = paste0(order, ": ", street, " (", crossing_id, ")")) %>% 
  write_sf("Diagnostics/PassiveCrossings.kml")

test <- read_sf("Diagnostic/PassiveCrossings.kml")

# Summary of Recommendations ----------------------------------------------

crossing_recs_tidy <- crossing_recs %>% 
  pivot_longer(
    cols = c(recommendation_1, recommendation_2, recommendation_3), 
    names_to = "RecNum", values_to = "Recommendation") %>% 
  filter(!is.na(Recommendation))

temp_lookup <- 
  crossing_recs_tidy %>% st_drop_geometry() %>% 
  count(Recommendation) %>% arrange((n)) %>% 
  mutate(
    legend_order = row_number() * 10,
    legend_cat = case_when(
      Recommendation %in% c("Advanced flashing warning signs", "Lighting", "Signing and pavement markings") ~ "1: Universal",
      Recommendation %in% c("Upgrade with gates and lights") ~ "2: Gates and Lights",
      Recommendation %in% c("Closure") ~ "3: Closure",
      Recommendation %in% c("Grade Separation") ~ "4: Grade Separation",
      TRUE ~ "5: Other")
    )

crossing_recs_tidy %>% st_drop_geometry() %>% 
  count(Recommendation, RecNum) %>% 
  pivot_wider(names_from = RecNum, values_from = n) %>% 
  rowwise() %>% 
  mutate(
    across(c(recommendation_1, recommendation_2, recommendation_3), ~replace_na(.x, 0)),
    total = recommendation_1 + recommendation_2 + recommendation_3
    ) %>% 
  write_csv("rec_summary.csv")

crossing_recs_tidy <- crossing_recs_tidy %>% 
  left_join(temp_lookup)

basemaps +
  tm_shape(crossing_recs_tidy %>% filter(legend_cat == "1: Universal") %>% mutate(legend_order = rank(legend_order))) + 
      tm_dots(col = "Recommendation", size = "legend_order", palette = "Set1", popup.vars = TRUE, alpha = 0.5) +
  tm_shape(crossing_recs_tidy %>% filter(legend_cat == "2: Gates and Lights") %>% mutate(legend_order = rank(legend_order))) + 
      tm_dots(col = "Recommendation", size = "legend_order", palette = "Set1", popup.vars = TRUE, alpha = 0.5) +
  tm_shape(crossing_recs_tidy %>% filter(legend_cat == "3: Closure") %>% mutate(legend_order = rank(legend_order))) + 
      tm_dots(col = "Recommendation", size = "legend_order", palette = "Set1", popup.vars = TRUE, alpha = 0.5) +
  tm_shape(crossing_recs_tidy %>% filter(legend_cat == "4: Grade Separation") %>% mutate(legend_order = rank(legend_order))) + 
      tm_dots(col = "Recommendation", size = "legend_order", palette = "Set1", popup.vars = TRUE, alpha = 0.5) +
  tm_shape(crossing_recs_tidy %>% filter(legend_cat == "5: Other") %>% mutate(legend_order = rank(legend_order))) + 
      tm_dots(col = "Recommendation", size = "legend_order", palette = "Set1", popup.vars = TRUE, alpha = 0.5)















