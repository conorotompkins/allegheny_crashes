#library(tidyverse)
#library(lubridate)

#df_combined_allegheny_county_crash_data_2004_2017_factorized <- read_csv("data/df_combined_allegheny_county_crash_data_2004_2017_factorized.csv", col_types = cols(.default = "c"), progress = FALSE)

#glimpse(df_combined_allegheny_county_crash_data_2004_2017_factorized)


df_combined_allegheny_county_crash_data_2004_2017_factorized %>% 
  mutate(fatal = as.integer(str_sub(fatal, 1)),
    crash_year = as.integer(crash_year),
         crash_month = month(as.integer(crash_month), label = TRUE),
         day_of_week_old = day_of_week,
         day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
         time_of_day = as.integer(time_of_day),
         hour_of_day = as.integer(hour_of_day)) %>% 
  mutate(sch_bus_ind = case_when(sch_bus_ind == "N" ~ FALSE,
                                 sch_bus_ind == "Y" ~ TRUE),
         sch_zone_ind = case_when(sch_zone_ind == "N" ~ FALSE,
                                  sch_zone_ind == "Y" ~ TRUE),
         tfc_detour_ind = factor(tfc_detour_ind, levels = c("Y", "N", "U")),
         work_zone_ind = case_when(work_zone_ind == "N" ~ FALSE,
                                   work_zone_ind == "Y" ~ TRUE),
         wz_law_offcr_ind = factor(wz_law_offcr_ind, levels = c("Y", "N", "U")),
         ntfy_hiwy_maint = case_when(ntfy_hiwy_maint == "N" ~ FALSE,
                                     ntfy_hiwy_maint == "Y" ~ TRUE)) %>%
  mutate_at(vars(contains("count")), as.integer) %>% 
  mutate_at(vars(contains("unit")), as.integer) %>% 
  mutate(automobile = case_when(automobile_count >= 1 ~ TRUE,
                                automobile_count < 1 ~ FALSE),
         bus = case_when(bus_count >= 1 ~ TRUE,
                         bus_count < 1 ~ FALSE),
         small_truck = case_when(small_truck_count >= 1 ~ TRUE,
                                 small_truck_count < 1 ~ FALSE),
         heavy_truck = case_when(heavy_truck_count >= 1 ~ TRUE,
                                 heavy_truck_count < 1 ~ FALSE),
         suv = case_when(suv_count >= 1 ~ TRUE, 
                         suv_count < 1 ~ FALSE),
         van = case_when(van_count >= 1 ~ TRUE,
                         van_count < 1 ~ FALSE),
         motorcycle = case_when(motorcycle_count >= 1 ~ TRUE,
                                motorcycle_count < 1 ~ FALSE),
         bicycle = case_when(bicycle_count >= 1 ~ TRUE,
                             bicycle_count < 1 ~ FALSE),
         pedestrian = case_when(ped_count >= 1 ~ TRUE,
                                ped_count < 1 ~ FALSE)) %>% 
  mutate(speeding_related = as.integer(speeding_related),
         speed_limit = as.integer(speed_limit),
         dec_lat = as.double(dec_lat),
         dec_long = as.double(dec_long),
         est_hrs_closed = as.integer(est_hrs_closed),
         lane_closed = as.integer(lane_closed),
         cons_zone_spd_lim = as.integer(cons_zone_spd_lim),
         workers_pres = as.integer(workers_pres),
         fatalities_per_person = fatal_count / person_count) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate_at(vars(interstate:trolley), as.logical) -> data

#glimpse(data)



#data %>% 
#  filter(crash_year == 2015) %>% 
#  count(crash_year, crash_month, day_of_week) -> test
