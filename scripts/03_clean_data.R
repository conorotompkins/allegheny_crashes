library(tidyverse)
library(lubridate)

#df_combined_allegheny_county_crash_data_2004_2017_raw <- read_csv("https://raw.githubusercontent.com/conorotompkins/allegheny_crashes#/master/data/df_combined_allegheny_county_crash_data_2004_2017_raw.csv")

df_combined_allegheny_county_crash_data_2004_2017_raw %>% 
  mutate(crash_year = as.integer(as.double(crash_year)),
         crash_month = month(as.integer(crash_month), label = TRUE),
         day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
         time_of_day = as.integer(time_of_day),
         hour_of_day = as.integer(hour_of_day)) -> df

df %>% 
  mutate_at(vars(contains("count")), as.integer) %>% 
  mutate_at(vars(contains("units")), as.integer) %>% 
  mutate(sch_bus_ind = case_when(sch_bus_ind == "N" ~ FALSE,
                                 sch_bus_ind == "Y" ~ TRUE),
         sch_zone_ind = case_when(sch_zone_ind == "N" ~ FALSE,
                                  sch_zone_ind == "Y" ~ TRUE),
         tfc_detour_ind = factor(tfc_detour_ind, levels = c("Y", "N", "U")),
         work_zone_ind = case_when(work_zone_ind == "N" ~ FALSE,
                                   work_zone_ind == "Y" ~ TRUE),
         wz_law_offcr_ind = factor(wz_law_offcr_ind, levels = c("Y", "N", "U")),
         ntfy_hiwy_maint = case_when(ntfy_hiwy_maint == "N" ~ FALSE,
                                     ntfy_hiwy_maint == "Y" ~ TRUE)) -> df

df %>% 
  str()

df %>% 
  colnames()

df %>%
  mutate_at(vars(wet_road:curved_road), case_when(. == 0 ~ FALSE,
                                                  . == 1 ~ TRUE)) %>% 
  str()

df %>% 
  distinct(ntfy_hiwy_maint)


df %>% 
  filter(hour_of_day >= 23)
