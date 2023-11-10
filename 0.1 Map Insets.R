# Create inset maps for the exhibits

library(tidyverse)
library(tigris)
library(sf)
library(tmap)
tmap_mode("plot")
library(basemaps)

basemaps <- tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery"))

load("data/MoDOT_base_data.RData")


crossings <- xings_pub_MO_amtrak$CrossingID

i <- crossings[11]
extent_miles <- 1
createPlot <- function(i, extent_miles) {
  basetile <- 
    basemap(
      ext = xings_pub_MO_amtrak %>% filter(CrossingID == i) %>% st_buffer(5280 * extent_miles), 
      map_service = "osm", map_type = "streets", map_res = 1
    )
  
  plot <- 
    tm_shape(basetile, unit = "imperial") + tm_rgb(alpha = 0.5) +
    tm_shape(rail_amtrak) + tm_lines(lwd = 0.5) +
    tm_shape(xings_pub_MO_amtrak) + tm_dots(size = 0.03) +
    tm_shape(gradeseps_MO_amtrak) + 
        tm_symbols(shape = 24, border.lwd = 0.8, border.col = "blue", alpha = 0, size = 0.05) +
    tm_shape(xings_pub_MO_amtrak %>% filter(CrossingID == i)) + tm_dots(col = "red", size = 0.1) +
    tm_add_legend(
      type = "symbol",
      labels = c("Focal Crossing", "Grade Crossing", "Grade Separation"),
      col = c("red", "black", "blue"),
      shape = c(21, 21, 24), 
      alpha = c(1, 1, 0),
      border.col = c("red", "black", "blue")
      ) +
    tm_layout(
      legend.bg.color = "white",
      outer.margins = c(0,0,0,0),
      asp = 0
    )
 
  
  tmap_save(
    plot,
    filename = paste0("data/InsetMaps/", i, "_inset_corridor.jpg"),
    width = 3, height = 3, dpi = 600
  )
}

createPlot(crossings[11], 0.5)
createPlot(crossings[12], 0.5)
createPlot(crossings[13], 0.5)
createPlot(crossings[1], 0.5)
createPlot(crossings[2], 0.5)
createPlot(crossings[3], 1)


map(
  crossings,
  ~createPlot(. , 0.5)
)
