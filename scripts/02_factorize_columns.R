library(tidyverse)
library(lubridate)

df_combined_allegheny_county_crash_data_2004_2017_raw <- read_csv("data/df_combined_allegheny_county_crash_data_2004_2017_raw.csv", col_types = cols(.default = "c"), progress = FALSE)

df_dictionary <- read_csv("data/df_dictionary_rebuilt.csv") %>% 
  filter(!is.na(code))

df_combined_allegheny_county_crash_data_2004_2017_raw %>% 
  mutate(collision_type = as.character(collision_type)) %>% 
  left_join(df_dictionary %>% filter(column_name == "collision_type") %>% select(code, readable), c("collision_type" = "code")) %>% 
  mutate(collision_type = as.factor(readable),
         collision_type = fct_explicit_na(collision_type)) %>%
  select(-readable) -> df_combined_allegheny_county_crash_data_2004_2017_raw
levels(df_combined_allegheny_county_crash_data_2004_2017_raw$collision_type)

df_combined_allegheny_county_crash_data_2004_2017_raw %>% 
  mutate(crash_county = as.character(crash_county)) %>% 
  left_join(df_dictionary %>% filter(column_name == "county") %>% select(code, readable), c("crash_county" = "code")) %>% 
  mutate(crash_county = as.factor(readable),
         crash_county = fct_explicit_na(crash_county)) %>%
  select(-readable) -> df_combined_allegheny_county_crash_data_2004_2017_raw
levels(df_combined_allegheny_county_crash_data_2004_2017_raw$crash_county)

df_combined_allegheny_county_crash_data_2004_2017_raw %>% 
  mutate(day_of_week_old = day_of_week,
         day_of_week = as.integer(day_of_week),
         day_of_week = as.character(day_of_week)) %>% 
  left_join(df_dictionary %>% filter(column_name == "day_of_week") %>% select(code, readable), c("day_of_week" = "code")) %>% 
  mutate(day_of_week = as.character(readable)) %>% 
  select(-readable) %>% 
  select(crash_crn, district, crash_county, municipality, police_agcy, crash_year, crash_month, day_of_week, day_of_week_old, everything()) -> df_combined_allegheny_county_crash_data_2004_2017_raw
unique(df_combined_allegheny_county_crash_data_2004_2017_raw$day_of_week)
unique(df_combined_allegheny_county_crash_data_2004_2017_raw$day_of_week_old)

df_combined_allegheny_county_crash_data_2004_2017_raw %>% 
  mutate(district = as.character(district)) %>% 
  left_join(df_dictionary %>% filter(column_name == "district") %>% select(code, readable), c("district" = "code")) %>% 
  mutate(district = as.character(readable)) %>% 
  select(-readable) -> df_combined_allegheny_county_crash_data_2004_2017_raw

df_combined_allegheny_county_crash_data_2004_2017_raw %>% 
  mutate(illumination = as.character(illumination)) %>% 
  left_join(df_dictionary %>% filter(column_name == "illumination") %>% select(code, readable), c("illumination" = "code")) %>% 
  mutate(illumination = as.factor(readable),
         illumination = fct_explicit_na(illumination)) %>% 
  select(-readable) -> df_combined_allegheny_county_crash_data_2004_2017_raw
levels(df_combined_allegheny_county_crash_data_2004_2017_raw$illumination)

df_combined_allegheny_county_crash_data_2004_2017_raw %>% 
  mutate(intersect_type = as.character(intersect_type)) %>% 
  left_join(df_dictionary %>% filter(column_name == "intersect_type") %>% select(code, readable), c("intersect_type" = "code")) %>% 
  mutate(intersect_type = as.factor(readable),
         intersect_type = fct_explicit_na(intersect_type)) %>% 
  select(-readable) -> df_combined_allegheny_county_crash_data_2004_2017_raw
levels(df_combined_allegheny_county_crash_data_2004_2017_raw$intersect_type)

df_combined_allegheny_county_crash_data_2004_2017_raw %>% 
  mutate(ln_close_dir = as.character(ln_close_dir)) %>% 
  left_join(df_dictionary %>% filter(column_name == "ln_close_dir") %>% select(code, readable), c("ln_close_dir" = "code")) %>% 
  mutate(ln_close_dir = as.factor(readable),
         ln_close_dir = fct_explicit_na(ln_close_dir)) %>% 
  select(-readable) -> df_combined_allegheny_county_crash_data_2004_2017_raw
levels(df_combined_allegheny_county_crash_data_2004_2017_raw$ln_close_dir)

df_combined_allegheny_county_crash_data_2004_2017_raw %>% 
  mutate(location_type = as.character(location_type)) %>% 
  left_join(df_dictionary %>% filter(column_name == "location_type") %>% select(code, readable), c("location_type" = "code")) %>% 
  mutate(location_type = as.factor(readable),
         location_type = fct_explicit_na(location_type)) %>% 
  select(-readable) -> df_combined_allegheny_county_crash_data_2004_2017_raw
levels(df_combined_allegheny_county_crash_data_2004_2017_raw$location_type)

df_combined_allegheny_county_crash_data_2004_2017_raw %>% 
  mutate(max_severity_level = as.character(max_severity_level)) %>% 
  left_join(df_dictionary %>% filter(column_name == "max_severity_level") %>% select(code, readable), c("max_severity_level" = "code")) %>% 
  mutate(max_severity_level = factor(readable, levels = c("Unknown", "Not", "Injury", "Minor", "Major", "Killed")),
         max_severity_level = fct_explicit_na(max_severity_level)) %>% 
  select(-readable) -> df_combined_allegheny_county_crash_data_2004_2017_raw
levels(df_combined_allegheny_county_crash_data_2004_2017_raw$max_severity_level)

df_combined_allegheny_county_crash_data_2004_2017_raw %>% 
  mutate(rdwy_orient = as.character(rdwy_orient)) %>% 
  left_join(df_dictionary %>% filter(column_name == "rdwy_orient") %>% select(code, readable), c("rdwy_orient" = "code")) %>%
  mutate(rdwy_orient = as.factor(readable),
         rdwy_orient = fct_explicit_na(rdwy_orient)) %>% 
  select(-readable) -> df_combined_allegheny_county_crash_data_2004_2017_raw
levels(df_combined_allegheny_county_crash_data_2004_2017_raw$rdwy_orient)

df_combined_allegheny_county_crash_data_2004_2017_raw %>% 
  mutate(rdwy_surf_type_cd = as.character(rdwy_surf_type_cd)) %>% 
  left_join(df_dictionary %>% filter(column_name == "rdwy_surf_type_cd") %>% select(code, readable), c("rdwy_surf_type_cd" = "code")) %>% 
  mutate(rdwy_surf_type_cd = as.factor(readable),
         rdwy_surf_type_cd = fct_explicit_na(rdwy_surf_type_cd)) %>% 
  select(-readable) -> df_combined_allegheny_county_crash_data_2004_2017_raw
levels(df_combined_allegheny_county_crash_data_2004_2017_raw$rdwy_surf_type_cd)

df_combined_allegheny_county_crash_data_2004_2017_raw %>% 
  mutate(relation_to_road = as.character(relation_to_road)) %>% 
  left_join(df_dictionary %>% filter(column_name == "relation_to_road") %>% select(code, readable), c("relation_to_road" = "code")) %>% 
  mutate(relation_to_road = as.factor(readable),
         relation_to_road = fct_explicit_na(relation_to_road)) %>% 
  select(-readable) -> df_combined_allegheny_county_crash_data_2004_2017_raw
levels(df_combined_allegheny_county_crash_data_2004_2017_raw$relation_to_road)

df_combined_allegheny_county_crash_data_2004_2017_raw %>% 
  mutate(road_condition = as.character(road_condition)) %>% 
  left_join(df_dictionary %>% filter(column_name == "road_condition") %>% select(code, readable), c("road_condition" = "code")) %>% 
  mutate(road_condition = as.factor(readable),
         road_condition = fct_explicit_na(road_condition)) %>% 
  select(-readable) -> df_combined_allegheny_county_crash_data_2004_2017_raw
levels(df_combined_allegheny_county_crash_data_2004_2017_raw$road_condition)

df_combined_allegheny_county_crash_data_2004_2017_raw %>% 
  mutate(road_owner = as.character(road_owner)) %>% 
  left_join(df_dictionary %>% filter(column_name == "road_owner") %>% select(code, readable), c("road_owner" = "code")) %>% 
  mutate(road_owner = as.factor(readable),
         road_owner = fct_explicit_na(road_owner)) %>% 
  select(-readable) -> df_combined_allegheny_county_crash_data_2004_2017_raw
levels(df_combined_allegheny_county_crash_data_2004_2017_raw$road_owner)

df_combined_allegheny_county_crash_data_2004_2017_raw %>% 
  mutate(roadway_county = as.character(roadway_county)) %>% 
  left_join(df_dictionary %>% filter(column_name == "roadway_county") %>% select(code, readable), c("roadway_county" = "code")) %>% 
  mutate(roadway_county = as.factor(readable),
         roadway_county = fct_explicit_na(roadway_county)) %>% 
  select(-readable) -> df_combined_allegheny_county_crash_data_2004_2017_raw
levels(df_combined_allegheny_county_crash_data_2004_2017_raw$roadway_county)

df_combined_allegheny_county_crash_data_2004_2017_raw %>% 
  mutate(spec_juris_cd = as.character(spec_juris_cd)) %>% 
  left_join(df_dictionary %>% filter(column_name == "spec_juris_cd") %>% select(code, readable), c("spec_juris_cd" = "code")) %>% 
  mutate(spec_juris_cd = as.factor(readable),
         spec_juris_cd = fct_explicit_na(spec_juris_cd)) %>% 
  select(-readable) -> df_combined_allegheny_county_crash_data_2004_2017_raw
levels(df_combined_allegheny_county_crash_data_2004_2017_raw$spec_juris_cd)

df_combined_allegheny_county_crash_data_2004_2017_raw %>% 
  mutate(tcd_func_cd = as.character(tcd_func_cd)) %>% 
  left_join(df_dictionary %>% filter(column_name == "tcd_func_cd") %>% select(code, readable), c("tcd_func_cd" = "code")) %>% 
  mutate(tcd_func_cd = as.factor(readable),
         tcd_func_cd = fct_explicit_na(tcd_func_cd)) %>% 
  select(-readable) -> df_combined_allegheny_county_crash_data_2004_2017_raw
levels(df_combined_allegheny_county_crash_data_2004_2017_raw$tcd_func_cd)

df_combined_allegheny_county_crash_data_2004_2017_raw %>% 
  mutate(tcd_type = as.character(as.integer(tcd_type))) %>% 
  left_join(df_dictionary %>% filter(column_name == "tcd_type") %>% select(code, readable), c("tcd_type" = "code")) %>% 
  mutate(tcd_type = as.factor(readable),
         tcd_type = fct_explicit_na(tcd_type)) %>% 
  select(-readable) -> df_combined_allegheny_county_crash_data_2004_2017_raw
levels(df_combined_allegheny_county_crash_data_2004_2017_raw$tcd_type)

df_combined_allegheny_county_crash_data_2004_2017_raw %>% 
  mutate(urban_rural = as.character(urban_rural)) %>% 
  left_join(df_dictionary %>% filter(column_name == "urban_rural") %>% select(code, readable), c("urban_rural" = "code")) %>% 
  mutate(urban_rural = as.factor(readable),
         urban_rural = fct_explicit_na(urban_rural)) %>% 
  select(-readable) -> df_combined_allegheny_county_crash_data_2004_2017_raw
levels(df_combined_allegheny_county_crash_data_2004_2017_raw$urban_rural)

df_combined_allegheny_county_crash_data_2004_2017_raw %>% 
  mutate(weather = as.character(weather)) %>% 
  left_join(df_dictionary %>% filter(column_name == "weather") %>% select(code, readable), c("weather" = "code")) %>% 
  mutate(weather = as.factor(readable),
         weather = fct_explicit_na(weather)) %>% 
  select(-readable) -> df_combined_allegheny_county_crash_data_2004_2017_raw
levels(df_combined_allegheny_county_crash_data_2004_2017_raw$weather)

df_combined_allegheny_county_crash_data_2004_2017_raw %>% 
  mutate(work_zone_loc = as.character(work_zone_loc)) %>% 
  left_join(df_dictionary %>% filter(column_name == "work_zone_loc") %>% select(code, readable), c("work_zone_loc" = "code")) %>% 
  mutate(work_zone_loc = as.factor(readable),
         work_zone_loc = fct_explicit_na(work_zone_loc)) %>% 
  select(-readable) -> df_combined_allegheny_county_crash_data_2004_2017_raw
levels(df_combined_allegheny_county_crash_data_2004_2017_raw$work_zone_loc)

df_combined_allegheny_county_crash_data_2004_2017_raw %>% 
  mutate(work_zone_type = as.character(work_zone_type)) %>% 
  left_join(df_dictionary %>% filter(column_name == "work_zone_type") %>% select(code, readable), c("work_zone_type" = "code")) %>% 
  mutate(work_zone_type = as.factor(readable),
         work_zone_type = fct_explicit_na(work_zone_type)) %>% 
  select(-readable) -> df_combined_allegheny_county_crash_data_2004_2017_raw
levels(df_combined_allegheny_county_crash_data_2004_2017_raw$work_zone_type)

df_combined_allegheny_county_crash_data_2004_2017_factorized <- df_combined_allegheny_county_crash_data_2004_2017_raw
