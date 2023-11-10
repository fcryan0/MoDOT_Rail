


library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")


load("data/MoDOT_base_data.RData")

lines_TE <- rail_amtrak %>% filter(AmtrakLine == "Texas Eagle")
lines_MRR <- rail_amtrak %>% filter(AmtrakLine == "Missouri River Runner")

lines_buff_TE <- lines_TE %>% summarize() %>% st_buffer(8047)
lines_buff_MRR <- lines_MRR %>% summarize() %>% st_buffer(8047)

grid_size = 1609*3

hex_grid_TE <- 
  lines_buff_TE %>% st_make_grid(cellsize = grid_size, square = FALSE) %>% st_as_sf() %>% 
  st_filter(lines_buff_TE) %>% st_difference(lines_TE %>% summarize() %>% st_buffer(20)) %>% 
  mutate(
    id = row_number(),
    name = row_number(),
    is_pass = 0
  )
hex_grid_MRR <- 
  lines_buff_MRR %>% st_make_grid(cellsize = grid_size, square = FALSE) %>% st_as_sf() %>% 
  st_filter(lines_buff_MRR) %>% st_difference(lines_MRR %>% summarize() %>% st_buffer(20)) %>% 
  mutate(
    id = row_number(),
    name = row_number(),
    is_pass = 0
  )

tm_shape(hex_grid_TE) + tm_polygons(alpha = 0.5, col = "blue") +
  tm_shape(hex_grid_MRR) + tm_polygons(alpha = 0.5, col = "purple") +
  tm_shape(lines_TE) + tm_lines() +
  tm_shape(lines_MRR) + tm_lines()


hex_grid_TE %>% write_sf("data/hexForStreetLight_TE.shp")
hex_grid_MRR %>% write_sf("data/hexForStreetLight_MRR.shp")
