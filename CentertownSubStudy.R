library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")

basemaps <- tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery"))

load("data/MoDOT_base_data.RData")


basemaps + 
  tm_shape(gcisCurrent_MoDOT) + tm_dots()


centertownXings <- 
  gcisCurrent_MoDOT %>% 
  filter(CrossingID %in% c("442246T", "442247A", "442249N", "442250H"))

basemaps +
  tm_shape(centertownXings) + tm_dots(col = "red") + tm_text("Street", ymod = 5, col = "darkred")



# StreetLight Analysis ----------------------------------------------------


sf_toproute_Marion <- 
  read_sf("data/streetlight/1563654_Centertown_Top_Routes/1563654_Centertown_Top_Routes_osm_segment.shp")

#Read in raw StreetLight data and combined into single table
#Check for variable name difference between truck and all vehicle analyses (volume vs. traffic)
data_TR <- read_csv("data/streetlight/1563654_Centertown_Top_Routes/1563654_Centertown_Top_Routes_tr_za.csv",
                         col_types = cols(
                           `Average Daily Destination Zone Traffic (StL Volume)` = col_double(),
                           `Average Daily O-D Traffic (StL Volume)` = col_double(),
                           `Average Daily Origin Zone Traffic (StL Volume)` = col_double(),
                           `Destination Zone Direction (degrees)` = col_skip(),
                           `Destination Zone ID` = col_double(),
                           `Destination Zone Is Pass-Through` = col_skip(),
                           `Destination Zone Source Type` = col_skip(),
                           `Destination Zone is Bi-Direction` = col_skip(),
                           `Origin Zone Direction (degrees)` = col_skip(),
                           `Origin Zone ID` = col_double(),
                           `Origin Zone Is Pass-Through` = col_skip(),
                           `Origin Zone Source Type` = col_skip(),
                           `Origin Zone is Bi-Direction` = col_skip()))

#Rename variables, calculate new variables, and filter out extraneous information
data <- data_TR %>%
  rename(
    OZoneID = `Origin Zone ID`,
    OZoneName = `Origin Zone Name`,
    DZoneID = `Destination Zone ID`,
    DZoneName = `Destination Zone Name`,
    StL = `Average Daily O-D Traffic (StL Volume)`,
    StLOTot = `Average Daily Origin Zone Traffic (StL Volume)`,
    StLDTot = `Average Daily Destination Zone Traffic (StL Volume)`)%>%
  mutate(
    gateLoc = if_else(is.na(OZoneID), OZoneName, DZoneName),
    OSM_ID = if_else(is.na(OZoneID), DZoneID, OZoneID),
    gateStL = if_else(is.na(OZoneID), StLOTot, StLDTot)) %>%
  filter(
    `Day Type` == "0: All Days (M-Su)",
    `Day Part` == "0: All Day (12am-12am)"
  ) %>% 
  select(
    gateLoc,
    OSM_ID,
    StL,
    gateStL
  )


#Consolidate any duplicate values of gateLoc and OSM_ID
topRouteTidy <- data %>%
  group_by(gateLoc, OSM_ID) %>%
  summarise(
    StL = sum(StL),
    gateStL = first(gateStL)) %>% 
  mutate(TripPercent = round((StL / gateStL) * 100)) %>% 
  select(-StL, -gateStL)

#Spread to make column for each gate location
topRouteSpread <- topRouteTidy %>%
  spread(gateLoc, TripPercent)


#Remove N/A values
topRouteSpread[is.na(topRouteSpread)] <- 0

#Define the field range containing the gates
range <- c(2:(ncol(topRouteSpread)))

#Calculate a sum of all gate volumes
topRouteSpread <- topRouteSpread %>%
  mutate(totVol = rowSums(.[range]))



shpOSM <- full_join(sf_toproute_Marion, topRouteSpread, by = c("segment_id" = "OSM_ID"))
















# data_stl <- 
#   read_csv("data/streetlight/1563654_Centertown_Top_Routes/1563654_Centertown_Top_Routes_tr_za.csv") %>% 
#   janitor::clean_names() %>% 
#   select(
#     origin_zone_id, origin_zone_name, origin_zone_source,
#     destination_zone_id, destination_zone_name, destination_zone_source, 
#     day_type, day_part, volume = average_daily_o_d_traffic_st_l_volume,
#     trip_proportion)



basemaps + 
  tm_shape(shpOSM) + tm_lines(lwd = 'Marion St', scale = 20, col = 'Marion St', palette = "Reds", title.col = "Percent Trips") +
  tm_shape(centertownXings %>% filter(Street == "Marion Street")) + tm_dots(col = "blue", size = .5)

basemaps + 
  tm_shape(shpOSM) + tm_lines(lwd = 'Monroe St', scale = 20, col = 'Monroe St', palette = "Reds", title.col = "Percent Trips") +
  tm_shape(centertownXings %>% filter(Street == "Monroe Street/MO NN")) + tm_dots(col = "blue", size = 1)

basemaps + 
  tm_shape(shpOSM) + tm_lines(lwd = 'Oak St', scale = 20, col = 'Oak St', palette = "Reds", title.col = "Percent Trips") +
  tm_shape(centertownXings %>% filter(Street == "Oak Street")) + tm_dots(col = "blue", size = 1)

basemaps + 
  tm_shape(shpOSM) + tm_lines(lwd = 'Route Z', scale = 20, col = 'Route Z', palette = "Reds", title.col = "Percent Trips") +
  tm_shape(centertownXings %>% filter(Street == "MO Z")) + tm_dots(col = "blue", size = 1)
