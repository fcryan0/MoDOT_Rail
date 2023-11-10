# Data Prep for the MoDOT Passenger Rail Corridor Studies

library(tidyverse)
library(tigris)
library(sf)
library(tmap)
tmap_mode("view")

basemaps <- tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery"))


# State/County Boundary Data --------------------------------------------------------------

state <- "MO"
stateFIPS <- str_extract(lookup_code(state), '(?<=\').*?(?=\')')
fips <- tigris::fips_codes %>% mutate(GEOID = paste0(state_code, county_code))

stateShp <- states(cb = TRUE) %>% filter(STUSPS == state) %>% st_transform(4326)

crsuggest::suggest_crs(stateShp)
crs <- 6511  # NAD83 (2011) / Missouri Central; Meters

stateShp <- stateShp %>% st_transform(crs)

stateShp %>% write_sf("data/For_GIS_Teams/Missouri.shp")

counties_MO <- counties(state = "MO", cb = TRUE)
tm_shape(counties_MO) + tm_polygons()

counties_MO %>% write_sf("data/For_GIS_Teams/counties_MO.shp")

# Rail Line Data ----------------------------------------------------------

# From: https://hub.arcgis.com/datasets/fedmaps::north-american-rail-lines-1/about
    # rail_lines <- 
    #   read_sf("https://services2.arcgis.com/FiaPA4ga0iQKduv3/arcgis/rest/services/North_American_Rail_Lines_v1/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
    # 
    # save(rail_lines, file = "rail_lines.RData")
load("rail_lines.RData")

rail_MO <- 
  rail_lines %>% st_transform(crs) %>% 
  st_filter(stateShp) %>% st_intersection(stateShp) %>% 
  filter(!NET %in% c("A", "O", "R", "T", "X"))

basemaps +
  tm_shape(rail_MO) + tm_lines(col = "NET", lwd = 3, palette = "Set1")

rail_MObuff <- 
  rail_lines %>% st_transform(crs) %>% filter(!NET %in% c("A", "O", "R", "T", "X")) %>%  
  st_filter(stateShp %>% st_buffer(1000*100)) %>% 
  summarize()

tm_shape(rail_MObuff) + tm_lines(col = "red") +
  tm_shape(rail_MO)  + tm_lines()
  
rail_amtrak <- rail_MO %>% filter(PASSNGR == "A")

tm_shape(rail_amtrak) + tm_lines(lwd = 5)

# Define the Amtrak lines according the RR subdivision
lineLookup <- 
  unique(rail_amtrak$SUBDIV) %>% as_tibble() %>% 
  mutate(AmtrakLine = c(
    "Missouri River Runner",
    "Texas Eagle",
    "Southwest Chief",
    "Missouri River Runner",
    "Southwest Chief",
    "Texas Eagle",
    "Texas Eagle",
    "Southwest Chief",
    "Southwest Chief",
    "Southwest Chief",
    "Texas Eagle",
    "Southwest Chief"
  )) %>% 
  rename(SUBDIV = value)

rail_amtrak <- rail_amtrak %>% left_join(lineLookup)

basemaps +
  tm_shape(rail_amtrak) + 
    tm_lines(col = "AmtrakLine", lwd = 3, palette = "Set1", popup.vars = TRUE) + 
    tm_text("SUBDIV")

rail_amtrak %>% write_sf("data/For_GIS_Teams/rail_amtrak.shp")
rail_MO %>% write_sf("data/For_GIS_Teams/rail_MO.shp")
rail_MObuff %>% write_sf("data/For_GIS_Teams/rail_MO_buff.shp")






# Rail Crossing and Incident Data -----------------------------------------

# Data from MoDOT
modot_trra_grades_aadt <- 
  read_csv("data/from_MoDOT/MoDOT TRRA Approach Grades_AADT.csv")
modot_up_grades_aadt <- 
  read_csv("data/from_MoDOT/UP Passenger Subs_aadt_approach grade.csv")
modot_extra <- 
  readxl::read_excel("data/From_MoDOT/AADT_approachGrade_ crossings request from HDR.xlsx")
modot_all_xings <- 
  bind_rows(modot_trra_grades_aadt, modot_up_grades_aadt, modot_extra) %>% 
  filter(crossing_id != "803078N") %>% 
  select(crossing_id, RR, Subdivision, app1_grade, app1_grade_perc, app1_dir,
         app2_grade, app2_grade_perc, app2_dir, AADT, AADTYear, Latitude, Longitude, hwy_road_street = `Hwy/Road/Street`) %>% 
  mutate(
    grade_change = app1_grade_perc + app2_grade_perc
  )


# Read in 10 years of Accident/Incident data
gcisAccHist <- map_dfr(
  dir("data/acc", full.names = TRUE),
  ~read_csv(., col_types = cols(.default = "c")) #default to character type to avoid conflicting auto-generated types
) %>% 
  mutate(FatalInjury = case_when(
    as.numeric(TOTKLD) > 0 ~ "Fatal",
    as.numeric(TOTINJ) > 0 ~ "Injury",
    TRUE ~ "PDO"
  ))
gcisAccHist %>% count(FatalInjury)

incidents_year_sev <- gcisAccHist %>% 
  group_by(CrossingID = GXID, FatalInjury, IYR) %>% 
  summarize(count = n()) %>% 
  pivot_wider(names_from = FatalInjury, values_from = count) %>% 
  mutate(
    across(PDO:Fatal, ~replace_na(.,0)), 
    Total = Fatal + Injury + PDO
  ) %>% 
  arrange(CrossingID, IYR)

inc_10yr <- incidents_year_sev %>% 
  group_by(CrossingID) %>% 
  summarize(
    inc_tot_10yr = sum(Total),
    inc_fat_10yr = sum(Fatal),
    inc_inj_10yr = sum(Injury)
  )

inc_5yr <- incidents_year_sev %>% 
  filter(IYR %in% c("22", "21", "20", "19", "18")) %>% 
  group_by(CrossingID) %>% 
  summarize(
    inc_tot_5yr = sum(Total),
    inc_fat_5yr = sum(Fatal),
    inc_inj_5yr = sum(Injury)
  )


# Read in all Current US crossing data
# Download "All States" file from: https://safetydata.fra.dot.gov/OfficeofSafety/publicsite/DownloadCrossingInventoryData.aspx 
gcisCurrent <-
  read_csv("data/inv/PublishedCrossingData-02-28-2023.csv",
           col_types = cols(
             MilePost = col_double())) %>% 
  filter(ReasonID != 16) %>% 
  left_join(inc_10yr) %>% 
  left_join(inc_5yr) %>% 
  left_join(modot_all_xings %>% select(
    CrossingID = crossing_id, latitude_modot = Latitude, longitude_modot = Longitude,
    app1_grade,
    app1_grade_perc,
    app1_dir,
    app2_grade,
    app2_grade_perc,
    app2_dir,
    hwy_road_street,
    aadt_modot = AADT,
    aadt_year_modot = AADTYear
    )) %>% 
  mutate(
    Latitude = if_else(is.na(Latitude), latitude_modot, Latitude),
    Longitude = if_else(is.na(Longitude), longitude_modot, Longitude),
    WarnDev = case_when(
      as.numeric(GateConf) == 3 ~ "FourQuad",
      as.numeric(Gates) > 0 & Channel %in% 1:4 ~ "GatesWithMedian",
      as.numeric(Gates) > 0 ~ "Gates",
      as.numeric(FlashPai) > 0 & as.numeric(Gates) == 0 ~ "FlashingOnly",
      as.numeric(Gates) == 0 &  as.numeric(FlashPai) == 0 &  as.numeric(StopStd) > 0 ~ "StopSign",
      as.numeric(Gates) == 0 &  as.numeric(FlashPai) == 0 &  as.numeric(YieldStd) > 0 ~ "YieldSign",
      as.numeric(Gates) == 0 &  as.numeric(FlashPai) == 0 &  as.numeric(StopStd) == 0 &  as.numeric(XBuck) > 0 ~ "Crossbucks",
      TRUE ~ "Not Classified"
    )
  )

# Fixing individual crossings
gcisCurrent$PassCnt <- if_else(gcisCurrent$CrossingID == "446298Y", 2, gcisCurrent$PassCnt)

# Grade Separations on Amtrak routes in MO
gradeseps_MO_amtrak <- gcisCurrent %>% 
  filter(
    PosXing %in% c(2, 3),
    !is.na(Latitude)
    ) %>% 
  mutate(Position = if_else(PosXing == 2, "RR Under", "RR Over" )) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_transform(crs) %>% 
  st_filter(rail_amtrak %>% st_buffer(100)) %>% 
  st_join(rail_amtrak %>% select(AmtrakLine), join = st_nearest_feature) %>% 
  filter(AmtrakLine != "Southwest Chief")
basemaps +
  tm_shape(gradeseps_MO_amtrak) + tm_dots(col = "Position", palette = "Set1")

# Public Grade crossings on Amtrak routes in MO
xings_pub_MO_amtrak <- gcisCurrent %>% 
  filter(
    PosXing == 1, 
    !is.na(Latitude), 
    TypeXing == 3) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_transform(crs) %>% 
  st_filter(rail_amtrak %>% st_buffer(50)) %>% 
  mutate(Pedestrian = if_else(XPurpose == 1, "No", "Yes")) %>% 
  st_join(rail_amtrak %>% select(AmtrakLine), join = st_nearest_feature) %>% 
  filter(
    AmtrakLine != "Southwest Chief", 
    !CrossingID %in% c(
      "442106R",#Exclude crossings based on review below
      "442108E",
      "442633K",
      "442679Y",
      "442732H",
      "425030T",
      "803289K",
      "803288D",
      "803302W",
      "803293A",
      "445924K",
      "446016F", 
      "446017M",
      "438528C",
      "803355V",
      "803347D",
      "293616G",
      "663508H",
      "663510J",
      "441898J",
      "973885C",
      "973886J",
      "973890Y",
      "973889E",
      "442529R",
      "664331W",
      "664332D",
      "664326A",
      "442709N",
      "664302L",
      "595845L",
      "480278B",
      "674019D"
      )
    )


xings_pub_MO_amtrak %>% count(AmtrakLine)
  
basemaps +
  tm_shape(xings_pub_MO_amtrak) + tm_dots(col = "XPurpose", palette = "Set1")  

# Private Grade crossings on Amtrak routes in MO
xings_priv_MO_amtrak <- gcisCurrent %>% 
  filter(
    #PosXing == 1, 
    !is.na(Latitude), 
    TypeXing == 2) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_transform(crs) %>% 
  st_filter(rail_amtrak %>% st_buffer(50)) %>% 
  mutate(Pedestrian = if_else(XPurpose == 1, "No", "Yes")) %>% 
  st_intersection(rail_amtrak %>% select(AmtrakLine) %>% st_buffer(20)) %>% 
  filter(AmtrakLine != "Southwest Chief")
basemaps +
  tm_shape(xings_priv_MO_amtrak) + tm_dots(col = "XPurpose", palette = "Set1")

xings_priv_MO_amtrak %>% count(PosXing)



xings_pub_MO_amtrak %>% count(AmtrakLine)


gcisCurrent_MoDOT <- 
  gcisCurrent %>% filter(CrossingID %in% modot_all_xings$crossing_id) %>% 
  filter(PassCnt > 0) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  filter(
    !RrSubDiv %in% c(
      "PIXLEY IND LD INDEPENDENCE",
      "Bagnell Ind Ld",
      "Lake Ind Ld",
      "WEST BELT",
      "Lesperance Ind Ld",
      "STE. GENEVIEVE IND LD",
      "Ste. Genevieve Ind Ld",
      "Bonne Terre Ind Ld",
      "Crystal City Ind Ld"
    ),
    !CrossingID %in% c(
      "442106R",
      "442108E",
      "442633K",
      "442679Y",
      "442732H",
      "425030T",
      "803289K",
      "803288D",
      "803302W",
      "803293A",
      "445924K",
      "446016F", 
      "446017M",
      "438528C",
      "803355V"
    )
  )


basemaps +
  tm_shape(rail_amtrak) + 
  tm_lines(col = "AmtrakLine", lwd = 3, palette = "Set1", popup.vars = TRUE) +
  tm_shape(gcisCurrent_MoDOT) + tm_dots(col = "RrSubDiv", size = 0.2) +
  tm_shape(xings_pub_MO_amtrak) + tm_dots(popup.vars = TRUE)





# Questions:

# TRR: Missing grade/AADT info for all crossings between Hoffmeister Ave and the connection to the Missouri RR

# 442145G: Hwy crossing in FRA, but not MoDOT
# 445889Y: Confirm this is private crossing (listed as public in FRA)
# 445887K: Confirm private crossing
# 445889Y: Confirm this is private crossing (listed as public in FRA; streetview from 2015 doesn't show private xing xbucks)
# 445949F: Missing grade/AADT info for this crossing
# 446012D: Confirm this is private crossing (listed as public in FRA)
# 446320J: Confirm this is private crossing (listed as public in FRA); Note that even if this is classified as private, I would recommend including it in our study since it is a busy public works facility, not a simple residential crossing
# 803352A: Missing grade/AADT info for this crossing
# 803349S: Confirm private; Appears gated for limited access; Is gate ever operated by users from the east side (while vehicle is on the crossing?)

# 924199V: Pedestrian crossing in Kingsville; Should we include it?
# 442120L: Pedestrian crossing in Sedalia; Same Question
# 442129X: Ped crossing in Sedalia
# 972368K: Ped crossing at Amtrak station in Washington
# 425015R: Ped crossing at Amtrak station in Kirkwood
# 446317B: Ped crossing at Amtrak station in Poplar Bluff


# 6 ped crossings not in MoDOT
# 9 crossings missing data or need to confirm private
# 16 missing data near the merge point

# 31 total difference


# 278 Our count
xings_pub_MO_amtrak %>% count()
# 247 Their data?
gcisCurrent_MoDOT %>% count()
278-247





# Corridor Breakdowns -----------------------------------------------------

MRR_pub <- xings_pub_MO_amtrak %>% filter(AmtrakLine == "Missouri River Runner") %>% 
  mutate(WarnDev2 = case_when(
    WarnDev == "Gates" ~ "Gates",
    WarnDev == "GatesWithMedian" ~ "Gates",
    WarnDev == "FlashingOnly" ~ "Flashing Lights Only",
    TRUE ~ "Passive"
  ))
MRR_priv <- xings_priv_MO_amtrak %>% filter(AmtrakLine == "Missouri River Runner")
MRR_sep <- gradeseps_MO_amtrak %>% filter(AmtrakLine == "Missouri River Runner")

TE_pub <- xings_pub_MO_amtrak %>% filter(AmtrakLine == "Texas Eagle") %>% 
  mutate(WarnDev2 = case_when(
    WarnDev == "Gates" ~ "Gates",
    WarnDev == "GatesWithMedian" ~ "Gates",
    WarnDev == "FlashingOnly" ~ "Flashing Lights Only",
    TRUE ~ "Passive"
  ))shsev = case_when()
  )
TE_priv <- xings_priv_MO_amtrak %>% filter(AmtrakLine == "Texas Eagle")
TE_sep <- gradeseps_MO_amtrak %>% filter(AmtrakLine == "Texas Eagle")

MRR_priv %>% count(PosXing)
MRR_pub %>% count(PosXing)

TE_pub %>% count(WarnDev2)

basemaps +
  tm_shape(countiesShp) + tm_polygons(col = "gray", border.col = "white", lwd = 2) +
  tm_shape(rail_amtrak %>% filter(AmtrakLine == "Missouri River Runner")) + tm_lines() +
  #tm_shape(MRR_pub %>% filter(WarnDev2 == "Gates")) + tm_dots(col = ) +
  #tm_shape(MRR_pub %>% filter(WarnDev2 == "Passive")) + tm_dots(col = "red", size = 0.1, border.col = "black") +
  tm_shape(MRR_pub %>% filter(inc_fat_5yr > 0)) + tm_dots(col = "red", size = 0.2) +
  tm_shape(MRR_pub %>% filter(inc_inj_5yr > 0)) + tm_dots(col = "orange", size = 0.1) +
  tm_shape(MRR_pub %>% filter(inc_tot_5yr - inc_fat_5yr - inc_inj_5yr > 0)) + tm_dots() 
  

basemaps +
  tm_shape(countiesShp) + tm_polygons(col = "gray", border.col = "white", lwd = 2) +
  tm_shape(rail_amtrak %>% filter(AmtrakLine == "Texas Eagle")) + tm_lines() +
  tm_shape(TE_pub %>% filter(WarnDev2 == "Gates")) + tm_dots(col = ) +
  tm_shape(TE_pub %>% filter(WarnDev2 == "Passive")) + tm_dots(col = "red", size = 0.1, border.col = "black")


# Crossing Data Output -------------------------------------------------------------

xings_pub_MO_amtrak %>% write_sf("data/For_GIS_Teams/xings_pub_MO_amtrak.shp")
xings_priv_MO_amtrak %>% write_sf("data/For_GIS_Teams/xings_priv_MO_amtrak.shp")
gradeseps_MO_amtrak %>% write_sf("data/For_GIS_Teams/gradeseps_MO_amtrak.shp")


save(
  basemaps,
  counties_MO,
  gcisAccHist,
  gcisCurrent_MoDOT,
  gradeseps_MO_amtrak,
  incidents_year_sev,
  modot_all_xings,
  rail_amtrak,
  rail_MO,
  rail_MObuff,
  xings_priv_MO_amtrak,
  xings_pub_MO_amtrak,
  file = "data/MoDOT_base_data.RData"
)




# Crossing review worksheet prep ------------------------------------------

# Missouri River Runner
# Order west to east; Select specific attributes
data_MRR <- 
  xings_pub_MO_amtrak %>% filter(AmtrakLine == "Missouri River Runner") %>% 
  mutate(x_pos = st_coordinates(.)[,1]) %>% arrange(x_pos) %>% mutate(order = row_number()) %>% 
  mutate(order = case_when(
    order == 13 ~ as.integer(12), #manual fix: track travels west briefly
    order == 14 ~ as.integer(13),
    order == 12 ~ as.integer(14),
    TRUE ~ order
  )) %>% 
  select(
    AmtrakLine,
    order,
    CrossingID,
    WarnDev,
    Street,
    hwy_road_street,
    DayThru,
    NghtThru,
    TotalSwt,
    MaxTtSpd,
    Gates,
    FlashPai,
    Aadt,
    AadtYear,
    aadt_modot,
    aadt_year_modot
  ) %>% 
  st_transform(4326) %>% 
  mutate(
    x = st_coordinates(.)[,1], 
    y = st_coordinates(.)[,2] 
  )

data_TexEag <- 
  xings_pub_MO_amtrak %>% filter(AmtrakLine == "Texas Eagle") %>% 
  mutate(x_pos = st_coordinates(.)[,2]) %>% arrange(x_pos) %>% mutate(order = row_number()) %>% 
  # mutate(order = case_when(
  #   order == 13 ~ as.integer(12), #manual fix: track travels west briefly
  #   order == 14 ~ as.integer(13),
  #   order == 12 ~ as.integer(14),
  #   TRUE ~ order
  # )) %>% 
  select(
    AmtrakLine,
    order,
    CrossingID,
    WarnDev,
    Street,
    hwy_road_street,
    DayThru,
    NghtThru,
    TotalSwt,
    MaxTtSpd,
    Gates,
    FlashPai,
    Aadt,
    AadtYear,
    aadt_modot,
    aadt_year_modot
  ) %>% 
  st_transform(4326) %>% 
  mutate(
    x = st_coordinates(.)[,1], 
    y = st_coordinates(.)[,2] 
  )


data_forTeams <- 
  bind_rows(
    data_MRR %>% st_drop_geometry(), 
    data_TexEag %>% st_drop_geometry()
    )

bind_rows(data_MRR, data_TexEag) %>% 
  mutate(name = paste0(AmtrakLine, "_", order, "_", CrossingID)) %>% 
  write_sf("data/FinalCrossings.kml")
  
data_forTeams %>% writexl::write_xlsx("data/BaseDataForTeams.xlsx")

glimpse(data_MRR)
glimpse(xings_pub_MO_amtrak)
data_forTeams %>% count(AmtrakLine, WarnDev)

tm_shape(data_forTeams) + tm_dots(col = "order") + tm_text("order")








# Helper Map --------------------------------------------------------------

load("data/MoDOT_base_data.RData")
rail_buff <- rail_MO %>% st_filter(rail_amtrak %>% st_buffer(5280/2))
gcis_buff <- 
  gcisCurrent %>% filter(!is.na(Latitude)) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% st_transform(crs) %>% 
  st_filter(rail_amtrak %>% st_buffer(5280/2)) %>% 
  select(CrossingID)

basemaps +
  tm_shape(rail_buff) + tm_lines(col = "RROWNER1", palette = "Set1", popup.vars = TRUE, lwd = 2) +
  tm_shape(gcis_buff) + tm_dots() 

