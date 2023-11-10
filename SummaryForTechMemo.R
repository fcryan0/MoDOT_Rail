library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")

load("data/MoDOT_base_data.RData")


# Crash Data Summary ------------------------------------------------------

crash_summary <- 
  gcisAccHist %>% filter(GXID %in% xings_pub_MO_amtrak$CrossingID) %>% 
  left_join(xings_pub_MO_amtrak %>% select(GXID = CrossingID, AmtrakLine)) %>% 
  mutate(Narrative = paste(NARR1, NARR2, NARR3, NARR4, NARR5, sep = " ") %>% str_replace_all(" NA", "")) %>% 
  mutate(
    TYPVEH = case_when(
      TYPVEH == "A" ~ "Auto",
      TYPVEH == "B" ~ "Truck",
      TYPVEH == "C" ~ "Truck-Trailer",
      TYPVEH == "D" ~ "Pickup Truck",
      TYPVEH == "E" ~ "Van",
      TYPVEH == "J" ~ "Other",
      TYPVEH == "K" ~ "Pedestrian",
      TYPVEH == "M" ~ "Other",
      TRUE ~ "Other"
    ),
    POSITION = case_when(
      POSITION == "1" ~ "Stalled/Stuck on Crossing",
      POSITION == "2" ~ "Stopped on Crossing",
      POSITION == "3" ~ "Moving over Crossing",
      POSITION == "5" ~ "Blocked by Crossing Gates",
    ),
    MOTORIST = case_when(
      MOTORIST == "1" ~ "Went Around Gates",
      MOTORIST == "3" ~ "Did Not Stop",
      MOTORIST == "4" ~ "Stopped on Crossing",
      MOTORIST == "5" ~ "Other",
      MOTORIST == "7" ~ "Went Through Gate"
    ),
    IYR = 2000 + as.numeric(IYR),
    TIMEHR2 = if_else(AMPM == "PM" & TIMEHR < 12, as.character(as.numeric(TIMEHR) + 12), TIMEHR),
    TIMEHR3 = paste(TIMEHR2, TIMEMIN, sep = ":"),
    TimeCrash = lubridate::make_datetime(year = IYR, month = IMO, day = DAY, hour = TIMEHR2, min = TIMEMIN)
  ) %>% 
  arrange(AmtrakLine, IYR) %>% 
  select(GXID, AmtrakLine, TimeCrash, TYPVEH, POSITION, MOTORIST, FatalInjury, Narrative)

crash_summary %>% group_by(AmtrakLine) %>% count(TYPVEH) %>% 
  pivot_wider(names_from = AmtrakLine, values_from = n) %>% write_csv("CrashesByVehType.csv")

crash_summary %>% group_by(AmtrakLine) %>% count(MOTORIST) %>% 
  pivot_wider(names_from = AmtrakLine, values_from = n) %>% write_csv("CrashesByAction.csv")

crash_summary %>% write_csv("Crash_SummaryAll.csv")
         

# FRA Accident Prediction Model Results -----------------------------------

FRA_APS <- 
  read_csv("C:/Users/frryan/Desktop/_Working Files/_R/FRA/BaseRiskAnalysis_2023-04-10.csv") %>% 
  mutate(EstAnnCrashes = crashAnnEst_PDO + crashAnnEst_INJ + crashAnnEst_FAT)

FRA_APS_MO <- 
  xings_pub_MO_amtrak %>% 
  select(CrossingID, AmtrakLine) %>% 
  left_join(FRA_APS)
  
basemaps +
  tm_shape(FRA_APS_MO) + 
  tm_dots(
    col = "EstAnnCrashes", scale = 2, size = "EstAnnCrashes", 
    style = "log10_pretty", palette = "-Spectral", title = "Est. Annual Crashes"
    )


FRA_APS_MO %>% 
  ggplot(aes(x = EstAnnCrashes, fill = AmtrakLine)) + geom_density(show.legend = FALSE) +
  scale_x_log10() +
  scale_fill_manual(values = c("darkslateblue", "firebrick")) +
  xlab("Estimated Annual Crashes (Log Scale)") + ylab("Density") +
  facet_grid(~AmtrakLine)
ggsave(filename = "CrashDistribution.jpg", width = 6, height = 2)

plotly::ggplotly()



# Blocked Crossings -------------------------------------------------------

blockedCrossings <- 
  read_csv("C:/Users/frryan/Desktop/_Working Files/_R/FRA/BlockedCrossingReports_2023-04-23.csv")

blockedCrossingsMO <- 
  xings_pub_MO_amtrak %>% select(CrossingID, AmtrakLine, Street, CityName) %>% 
  left_join(blockedCrossings) %>% 
  mutate(total = replace_na(total, 0))
  

blockedCrossingsMO %>% st_drop_geometry() %>% count(total)

basemaps + 
  tm_shape(blockedCrossingsMO %>% filter(total > 0)) + tm_dots(col = "total", size = "total", title = "Blocked Crossing Reports")


blockedCrossingsMO %>% filter(total > 0) %>% st_drop_geometry() %>% arrange(desc(total)) %>% 
  write_csv("BlockedCrossingSummary.csv")




# StreetLight Summary -----------------------------------------------------


stl_vols <- 
  read_csv("StreetLightVolumes.csv") %>% select(-Crossing) %>% 
  pivot_longer(cols = ADT:Ped, names_to = "Mode", values_to = "Volume")

stl_vols %>% ggplot(aes(x = Volume)) + geom_histogram() +
  facet_wrap(~Mode) +
  ylab("Count") + xlab("Estimated Daily Volumes") +
  scale_x_log10()
ggsave(filename = "StreetLightVolumes.jpg")





# High Level Benefit-Cost Analysis ------------------------------------------------------

BCR_Benefits <- 
  readxl::read_excel("Benefit_Cost_Raw.xlsx", sheet = "SafetyBenefits") %>% 
  select(-AmtrakLine, -Notes) %>% 
  pivot_longer(-CrossingID, values_to = "Benefit") %>% 
  mutate(BC_ID = paste0(CrossingID, "_", str_sub(name, 0, 5))) %>% 
  select(BC_ID, Benefit)

BCR_Desc <- 
  readxl::read_excel("Benefit_Cost_Raw.xlsx", sheet = "Costs") %>% 
  select(CrossingID, Tier1Desc, Tier2Desc, Tier3Desc) %>% 
  pivot_longer(-CrossingID, values_to = "Description") %>% 
  mutate(BC_ID = paste0(CrossingID, "_", str_sub(name, 0, 5))) %>% 
  select(BC_ID, Description)

BCR_Costs <- 
  readxl::read_excel("Benefit_Cost_Raw.xlsx", sheet = "Costs") %>% 
  select(CrossingID, Tier1Cost, Tier2Cost, Tier3Cost) %>% 
  pivot_longer(-CrossingID, values_to = "Cost") %>% 
  mutate(BC_ID = paste0(CrossingID, "_", str_sub(name, 0, 5))) %>% 
  select(BC_ID, Cost)


BCR_Combo <- 
  BCR_Desc %>% 
  left_join(BCR_Costs) %>% 
  left_join(BCR_Benefits) %>% 
  filter(!is.na(Description)) %>% 
  mutate(
    Benefit = as.numeric(Benefit),
    Ratio = Benefit / Cost
    )


