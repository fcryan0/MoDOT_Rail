library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")
library(tidycensus)


basemaps <- tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery"))
load("data/MoDOT_base_data.RData")
tm_shape(xings_pub_MO_amtrak) + tm_dots()
crs <- 6511

master_sheet <- readxl::read_excel("data/BaseDataForTeams 2023-03-13.xlsx", sheet = "Master Sheet") %>% 
  filter(
    !is.na(AmtrakLine),
    is.na(Notes)
    )

master_sheet %>% count(AmtrakLine)

master_sheet %>% ggplot(aes(x = `Daily Trains`)) + geom_histogram(binwidth = 1) + facet_grid(~AmtrakLine)
plotly::ggplotly()

master_sheet %>% ggplot(aes(x = `MaxTtSpd`)) + geom_histogram(binwidth = 1) + facet_grid(~AmtrakLine)
plotly::ggplotly()

xings_pub_MO_amtrak %>% group_by(AmtrakLine) %>% count(PassCnt)



# Slide 14

#Define counties for map
counties_MRR <- 
  counties_MO %>% st_transform(crs) %>% 
  st_filter(xings_pub_MO_amtrak %>% filter(AmtrakLine == "Missouri River Runner"))
tm_shape(counties_MRR) + tm_polygons()
counties_MRR %>% write_sf("forPPT/counties_MRR.shp")
counties_MRR %>% write_sf("forPPT/counties_MRR.kml")

# Define MRR Crossings for map
xings_MRR <- 
  xings_pub_MO_amtrak %>% filter(AmtrakLine == "Missouri River Runner") %>% 
  mutate(passive_cat = 
           case_when(
             Pedestrian == "Yes"~ "Pedestrian",
             WarnDev %in% c("Not Classified", "StopSign", "YieldSign", "Crossbucks") ~ "passive",
             TRUE ~ "active"
           )
  )
xings_MRR %>% count(passive_cat)
xings_MRR %>% write_sf("forPPT/xings_MRR_passive.shp")
xings_MRR %>% write_sf("forPPT/xings_MRR_passive.kml")
xings_MRR %>% filter(passive_cat == "Pedestrian")





# Risk Calculations -------------------------------------------------------

risk <- xings_pub_MO_amtrak %>% 
  mutate(totTracks = IndustryTrk + MainTrk + SidingTrk + TransitTrk + YardTrk) %>%
  select(CrossingID, totTracks) %>% 
  left_join(master_sheet) %>% 
  select(CrossingID, AmtrakLine, Notes, `Tier I`, WarnDev, `Daily Trains`, 
         totTracks, `Tier I Rec Description`, `All Tier Rec Description`) %>% 
  filter(is.na(Notes) | !str_detect(Notes, "Pedestrian|Exclude")) %>% 
  mutate(# Risk for all crossings
    WarnDevEx = case_when(
      WarnDev %in% c("Crossbucks", "Not Classified", "StopSign", "YieldSign") ~ "pass",
      WarnDev %in% c("FlashingOnly") ~ "FL",
      TRUE ~ "gates"
    ),
    WarnDevNewAll = case_when(
      `All Tier Rec Description` %in% c("Closure") ~ "closure",
      `All Tier Rec Description` %>% str_detect("Grade Separation") ~ "grade_sep",
      `All Tier Rec Description` %>% str_detect("Security Gates") ~ "gates",
      `All Tier Rec Description` %>% str_detect("Gates|AFWS|Lighting|Other") ~ "gates",
      `All Tier Rec Description` %>% str_detect("Public-to") ~ "priv",
      `All Tier Rec Description` %>% str_detect("None") ~ WarnDevEx
    ),
    WarnDevChangeAll = paste0(WarnDevEx, "_to_", WarnDevNewAll),
    WarnDevCatAll = case_when(
      CrossingID == "446320J" ~ "pass_to_FL",
      WarnDevChangeAll %>% str_detect("closure") ~ "closure",
      WarnDevChangeAll %>% str_detect("grade_sep") ~ "grade_sep",
      WarnDevEx == WarnDevNewAll ~ "None",
      TRUE ~ WarnDevChangeAll
    ),
    base_risk_red_all = case_when(
      WarnDevCatAll %in% c("closure", "grade_sep") ~ 1,
      WarnDevCatAll == "None" ~ 0,
      WarnDevCatAll == "pass_to_gates" & `Daily Trains` <= 10 & totTracks <=1 ~ 0.75,
      WarnDevCatAll == "pass_to_gates" & `Daily Trains` <= 10 & totTracks > 1 ~ 0.86,
      WarnDevCatAll == "pass_to_gates" & `Daily Trains` > 10 & totTracks <=1 ~ 0.8,
      WarnDevCatAll == "pass_to_gates" & `Daily Trains` > 10 & totTracks > 1 ~ 0.78,
      WarnDevCatAll == "FL_to_gates" & `Daily Trains` <= 10 & totTracks <=1 ~ 0.89,
      WarnDevCatAll == "FL_to_gates" & `Daily Trains` <= 10 & totTracks > 1 ~ 0.65,
      WarnDevCatAll == "FL_to_gates" & `Daily Trains` > 10 & totTracks <=1 ~ 0.69,
      WarnDevCatAll == "FL_to_gates" & `Daily Trains` > 10 & totTracks > 1 ~ 0.63,
      WarnDevCatAll == "pass_to_FL" ~ 0.75,
      WarnDevCatAll == "pass_to_priv" ~ 0,
      TRUE ~ 99
    ),
    AWSF_risk_red_all = if_else(`All Tier Rec Description` %>% str_detect("AFWS"), 0.05, 0),
    Lighting_risk_red_all = if_else(`All Tier Rec Description` %>% str_detect("Lighting"), 0.05, 0),
    final_red_all = 1 - ((1 - base_risk_red_all) * (1 - AWSF_risk_red_all) * (1 - Lighting_risk_red_all))
  ) %>% 
  mutate(# Mutate just for the Tier I crossings
    WarnDevNewT1 = case_when(
      `Tier I Rec Description` %in% c("Closure") ~ "closure",
      `Tier I Rec Description` %>% str_detect("Grade Separation") ~ "grade_sep",
      `Tier I Rec Description` %>% str_detect("Security Gates") ~ "gates",
      `Tier I Rec Description` %>% str_detect("Gates|AFWS|Lighting|Other") ~ "gates",
      `Tier I Rec Description` %>% str_detect("Public-to") ~ "priv",
      `Tier I Rec Description` %>% str_detect("None") ~ WarnDevEx
    ),
    WarnDevChangeT1 = paste0(WarnDevEx, "_to_", WarnDevNewT1),
    WarnDevCatT1 = case_when(
      CrossingID == "446320J" ~ "pass_to_FL",
      WarnDevChangeT1 %>% str_detect("closure") ~ "closure",
      WarnDevChangeT1 %>% str_detect("grade_sep") ~ "grade_sep",
      WarnDevEx == WarnDevNewT1 ~ "None",
      TRUE ~ WarnDevChangeT1
    ),
    base_risk_red_t1 = case_when(
      WarnDevCatT1 %in% c("closure", "grade_sep") ~ 1,
      WarnDevCatT1 == "None" ~ 0,
      WarnDevCatT1 == "pass_to_gates" & `Daily Trains` <= 10 & totTracks <=1 ~ 0.75,
      WarnDevCatT1 == "pass_to_gates" & `Daily Trains` <= 10 & totTracks > 1 ~ 0.86,
      WarnDevCatT1 == "pass_to_gates" & `Daily Trains` > 10 & totTracks <=1 ~ 0.8,
      WarnDevCatT1 == "pass_to_gates" & `Daily Trains` > 10 & totTracks > 1 ~ 0.78,
      WarnDevCatT1 == "FL_to_gates" & `Daily Trains` <= 10 & totTracks <=1 ~ 0.89,
      WarnDevCatT1 == "FL_to_gates" & `Daily Trains` <= 10 & totTracks > 1 ~ 0.65,
      WarnDevCatT1 == "FL_to_gates" & `Daily Trains` > 10 & totTracks <=1 ~ 0.69,
      WarnDevCatT1 == "FL_to_gates" & `Daily Trains` > 10 & totTracks > 1 ~ 0.63,
      WarnDevCatT1 == "pass_to_FL" ~ 0.75,
      WarnDevCatT1 == "pass_to_priv" ~ 0,
      WarnDevChangeT1 %>% str_detect("to_NA") ~ 0,
      TRUE ~ 99
    ),
    AWSF_risk_red_t1 = if_else(`Tier I Rec Description` %>% str_detect("AFWS"), 0.05, 0),
    Lighting_risk_red_t1 = if_else(`Tier I Rec Description` %>% str_detect("Lighting"), 0.05, 0),
    final_red_t1 = 1 - ((1 - base_risk_red_t1) * (1 - AWSF_risk_red_t1) * (1 - Lighting_risk_red_t1))
  ) 

risk %>% st_drop_geometry() %>% group_by(AmtrakLine) %>%
  count(WarnDevChange) %>% arrange(WarnDevChange) 
risk %>% st_drop_geometry() %>% group_by(AmtrakLine) %>%
  count(WarnDevCat)
risk %>% st_drop_geometry() %>% group_by(AmtrakLine) %>%
  count(`All Tier Rec Description`)
risk %>% st_drop_geometry() %>% group_by(AmtrakLine) %>% 
  count(`Tier I Rec Description`)


risk %>% st_drop_geometry() %>% write_csv("RiskCalcs.csv")




master_sheet_summary <- 
  readxl::read_excel("data/BaseDataForTeams 2023-03-13.xlsx", sheet = "Master Sheet") %>% 
  filter(is.na(Notes) | !str_detect(Notes, "Pedestrian|Exclude")) %>% 
  filter(!is.na(AmtrakLine)) %>% 
  select(AmtrakLine, `Est 1-yr Incidents`, 
         `Final Est Incidents All`, `All Tier Rec Description`,
         `Final Est Incidents Tier 1`, `Tier I Rec Description`,
         `Total Trains`, `Final ADT V2`
         ) %>% 
  mutate(
    exp_ind = `Total Trains` * `Final ADT V2`,
    exp_ind_t1 = case_when(
      `Tier I Rec Description` %>% str_detect("Clos|Grade Sep") ~ 0,
      TRUE ~ exp_ind
    ),
    exp_ind_all = case_when(
      `All Tier Rec Description` %>% str_detect("Clos|Grade Sep") ~ 0,
      TRUE ~ exp_ind
    )
    )
    
# Exp Ind and Risk Summary
master_sheet_summary %>% group_by(AmtrakLine) %>% 
  summarize(
    est_inc_before = sum(`Est 1-yr Incidents`),
    est_inc_after_all = sum(`Final Est Incidents All`),
    est_inc_after_t1 = sum(`Final Est Incidents Tier 1`),
    exp_ind_tot = sum(exp_ind),
    exp_ind_all = sum(exp_ind_all),
    exp_ind_t1 = sum(exp_ind_t1)
    ) %>% 
  mutate(
    percent_red_all = 1 - (est_inc_after_all / est_inc_before),
    percent_red_t1 = 1 - (est_inc_after_t1 / est_inc_before),
    delta_inc_all = est_inc_before - est_inc_after_all,
    delta_inc_t1 = est_inc_before - est_inc_after_t1,
    percent_exp_ind_red_all = (exp_ind_all - exp_ind_tot) / exp_ind_tot,
    percent_exp_ind_red_t1 = (exp_ind_t1 - exp_ind_tot) / exp_ind_tot,
    delta_exp_ind_all = exp_ind_tot - exp_ind_all,
    delta_exp_ind_t1 = exp_ind_tot - exp_ind_t1
    ) %>% t()

# Tally of improvement type All Tiers
master_sheet_summary %>% 
  mutate(
    Closures = if_else(str_detect(`All Tier Rec Description`, "Closure"), 1, 0),
    AFWS = if_else(str_detect(`All Tier Rec Description`, "AFWS"), 1, 0),
    Lighting = if_else(str_detect(`All Tier Rec Description`, "Lighting"), 1, 0),
    Other = if_else(str_detect(`All Tier Rec Description`, "Other"), 1, 0),
    PublicToPrivate = if_else(str_detect(`All Tier Rec Description`, "Public-to"), 1, 0),
    SecurityGates = if_else(str_detect(`All Tier Rec Description`, "Security"), 1, 0),
    GradeSeps = if_else(str_detect(`All Tier Rec Description`, "Sep"), 1, 0)
  ) %>% 
  group_by(AmtrakLine) %>% 
  summarize(across(c(Closures:GradeSeps), sum))


master_sheet_summary %>% 
  mutate(
    Closures = if_else(str_detect(`Tier I Rec Description`, "Closure"), 1, 0),
    SecurityGates = if_else(str_detect(`Tier I Rec Description`, "Security"), 1, 0),
    Gates = if_else(str_detect(`Tier I Rec Description`, "Gates"), 1, 0) - SecurityGates,
    AFWS = if_else(str_detect(`Tier I Rec Description`, "AFWS"), 1, 0),
    Lighting = if_else(str_detect(`Tier I Rec Description`, "Lighting"), 1, 0),
    Other = if_else(str_detect(`Tier I Rec Description`, "Other"), 1, 0),
    PublicToPrivate = if_else(str_detect(`Tier I Rec Description`, "Public-to"), 1, 0),
    GradeSeps = if_else(str_detect(`Tier I Rec Description`, "Sep"), 1, 0)
  ) %>% 
  filter(!is.na(`Tier I Rec Description`)) %>% 
  group_by(AmtrakLine) %>% 
  summarize(across(c(Closures:GradeSeps), sum))







