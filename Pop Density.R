library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")
library(tidycensus)


basemaps <- tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery"))

load("data/MoDOT_base_data.RData")

tm_shape(xings_pub_MO_amtrak) + tm_dots()

crs <- 6511

vars <- load_variables(year = 2021, dataset = "acs5")

popData <- 
  get_acs("block group", "B01001_001", state = "MO", geometry = TRUE, year = 2021) %>% 
  st_transform(6511) %>% 
  st_filter(xings_pub_MO_amtrak %>% st_buffer(1609))


basemaps +
  tm_shape(popData) + tm_polygons(col = "estimate", alpha = 0.5) +
  tm_shape(xings_pub_MO_amtrak) + tm_dots() +
  tm_shape(xings_pub_MO_amtrak %>% st_buffer(1609)) + tm_borders()

i <- 265
density_fun <- function(i) {
  crossing <- xings_pub_MO_amtrak[i,]
  
  crossingID <- crossing %>% pull(CrossingID)
  
  crossingCount <- 
    xings_pub_MO_amtrak %>% st_filter(crossing %>% st_buffer(1609)) %>% 
    st_drop_geometry() %>% summarize(count = n()) %>% pull(count)
  
  crossingDensity <- 
    popData %>% st_filter(crossing %>% st_buffer(1609)) %>% select(GEOID, estimate) %>% 
    mutate(
      area_sm = st_area(.) %>% as.numeric() / 2.59e+6,
      popPerSM = estimate / area_sm
      ) %>% 
    st_intersection(crossing %>% select(CrossingID) %>% st_buffer(1609)) %>% 
    mutate(
      area_sm_new = st_area(.) %>% as.numeric() / 2.59e+6,
      pop_new = area_sm_new / area_sm * estimate
      ) %>% 
    st_drop_geometry() %>% 
    summarize(
      pop = sum(pop_new),
      area_sm = sum(area_sm_new),
      popPerSM = pop / area_sm
      ) %>% 
    pull(popPerSM)
  
  return(tibble(
    CrossingID = crossingID, 
    crossingCount = crossingCount, 
    crossingDensity =crossingDensity))
}

densityData <- 
  map_dfr(
    1:length(xings_pub_MO_amtrak$CrossingID),
    ~density_fun(.)
  )

densityData %>% write_csv("CrossingDensity.csv")


ggplot() +
  geom_point(data = densityData, aes(y = crossingDensity, x = crossingCount)) +
  geom_point(data = densityData %>% 
               filter(CrossingID %in% c("442249N", "442246T", "442247A")), 
             aes(y = crossingDensity, x = crossingCount), col = "red") +
  scale_y_log10()
plotly::ggplotly()


basemaps +
  tm_shape(popData) + tm_polygons(col = "estimate", alpha = 0.5) +
  tm_shape(xings_pub_MO_amtrak) + tm_dots() +
  tm_shape(xings_pub_MO_amtrak %>% left_join(densityData)) + tm_dots(col = "crossingDensity", size = "crossingDensity", palette = "BuPu")


densityData %>% 
  group_by(crossingCount) %>% 
  filter(crossingDensity == min(crossingDensity)) %>% 
  arrange(crossingCount) %>% 
  left_join(xings_pub_MO_amtrak %>% select(CrossingID, Street, CityName)) %>% 
  st_as_sf() %>% st_drop_geometry() %>% 
  select(crossingCount, crossingDensity, CrossingID, Street, CityName) %>% 
  write_csv("Crossing and Pop Density.csv")


tm_shape(popData) + tm_polygons(col = "estimate", alpha = 0.5) +
tm_shape(xings_pub_MO_amtrak) + tm_dots() +
tm_shape(xings_pub_MO_amtrak %>% filter(CrossingID == "442113B")) + tm_dots(col = "red")


# Look up places and Counties ---------------------------------------------

library(tigris)
mo_places <- places(state = "MO") %>% st_transform(crs)

crossing_places <- xings_pub_MO_amtrak %>% select(CrossingID) %>% st_intersection(mo_places) %>% 
  select(CrossingID, NAME, NAMELSAD)
crossing_places %>% write_csv("Crossing_Places.csv")

basemaps +
  tm_shape(mo_places) + tm_polygons(alpha = 0.5) +
  tm_shape(rail_amtrak) + tm_lines(col = "AmtrakLine", palette = "Set1") +
  tm_shape(xings_pub_MO_amtrak) + tm_dots() + tm_text(text = "CrossingID", xmod = 0.1)

crossing_counties <- xings_pub_MO_amtrak %>% select(CrossingID) %>% 
  st_intersection(counties_MO %>% st_transform(crs)) %>% 
  select(CrossingID, NAME, NAMELSAD)
crossing_counties %>% write_csv("Crossing_Counties.csv")




tmap_mode("plot")
tm_shape(counties_MO) + tm_polygons(lwd = 3, col.border = "white")
