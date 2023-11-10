library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")

load("data/MoDOT_base_data.RData")

glimpse(xings_pub_MO_amtrak)

xings_pub_MO_amtrak %>% st_drop_geometry() %>% group_by(HwySpeed) %>% summarize(count = n())




xings_pub_MO_amtrak %>% st_drop_geometry() %>% 
  mutate(
    RrSubDiv = RrSubDiv %>% str_to_title(),
    inc_fat_5yr = str_replace_na(inc_fat_5yr, 0) %>% as.numeric(),
    inc_inj_5yr = str_replace_na(inc_inj_5yr, 0) %>% as.numeric(),
    inc_tot_5yr = str_replace_na(inc_tot_5yr, 0) %>% as.numeric(),
    inc_pdo_5yr = inc_tot_5yr - inc_fat_5yr - inc_inj_5yr,
    grade_change = app1_grade_perc + app2_grade_perc
  ) %>% 
  select(
    CrossingID,
    MilePost,
    Railroad,
    #XingOwnr,
    RrSubDiv,
    MaxTtSpd,
    HwySpeed,
    inc_fat_5yr,
    inc_inj_5yr,
    inc_pdo_5yr,
    inc_tot_5yr,
    grade_change
  ) %>% 
  write_csv("BaseRailData v1.csv")
