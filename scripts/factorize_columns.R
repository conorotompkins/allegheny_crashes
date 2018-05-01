library(tidyverse)
library(lubridate)

df <- read_csv("data/combined_allegheny_county_crash_data_2004-2017.csv")

df_dictionary <- read_csv("data/df_dictionary_rebuilt.csv") %>% 
  filter(!is.na(code))

df %>% 
  mutate(collision_type = as.character(collision_type)) %>% 
  left_join(df_dictionary %>% filter(column_name == "collision_type") %>% select(code, readable), c("collision_type" = "code")) %>% 
  mutate(collision_type = as.factor(readable)) %>%
  select(-readable) -> df

levels(df$collision_type)

colnames(df) %in% c("county")

df %>% 
  select(contains("county"))

df %>% 
  mutate(crash_county = as.character(crash_county)) %>% 
  left_join(df_dictionary %>% filter(column_name == "county") %>% select(code, readable), c("crash_county" = "code")) %>% 
  mutate(crash_county = as.factor(readable)) %>%
  select(-readable) -> df

levels(df$crash_county)

df %>% 
  count(day_of_week)

df %>% 
  mutate(day_of_week = as.character(day_of_week)) %>% 
  left_join(df_dictionary %>% filter(column_name == "day_of_week") %>% select(code, readable), c("day_of_week" = "code")) %>% 
  mutate(day_of_week = as.character(readable)) %>% 
  select(-readable) -> df

df %>% 
  mutate(district = as.character(district)) %>% 
  left_join(df_dictionary %>% filter(column_name == "district") %>% select(code, readable), c("district" = "code")) %>% 
  mutate(district = as.character(readable)) %>% 
  select(-readable) -> df

df %>% 
  mutate(illumination = as.character(illumination)) %>% 
  left_join(df_dictionary %>% filter(column_name == "illumination") %>% select(code, readable), c("illumination" = "code")) %>% 
  mutate(illumination = as.factor(readable)) %>% 
  select(-readable) -> df
levels(df$illumination)

df %>% 
  mutate(intersect_type = as.character(intersect_type)) %>% 
  left_join(df_dictionary %>% filter(column_name == "intersect_type") %>% select(code, readable), c("intersect_type" = "code")) %>% 
  mutate(intersect_type = as.factor(readable)) %>% 
  select(-readable) -> df
levels(df$intersect_type)

df %>% 
  mutate(ln_close_dir = as.character(ln_close_dir)) %>% 
  left_join(df_dictionary %>% filter(column_name == "ln_close_dir") %>% select(code, readable), c("ln_close_dir" = "code")) %>% 
  mutate(ln_close_dir = as.factor(readable)) %>% 
  select(-readable) -> df
levels(df$ln_close_dir)

df %>% 
  mutate(location_type = as.character(location_type)) %>% 
  left_join(df_dictionary %>% filter(column_name == "location_type") %>% select(code, readable), c("location_type" = "code")) %>% 
  mutate(location_type = as.factor(readable)) %>% 
  select(-readable) -> df
levels(df$location_type)

max_severity_level

df %>% 
  mutate(max_severity_level = as.character(max_severity_level)) %>% 
  left_join(df_dictionary %>% filter(column_name == "max_severity_level") %>% select(code, readable), c("max_severity_level" = "code")) %>% 
  mutate(max_severity_level = factor(readable, levels = c("Unknown", "Not", "Injury", "Minor", "Major", "Killed"))) %>% 
  select(-readable) -> df
levels(df$max_severity_level)

df %>% 
  mutate(rdwy_orient = as.character(rdwy_orient)) %>% 
  left_join(df_dictionary %>% filter(column_name == "rdwy_orient") %>% select(code, readable), c("rdwy_orient" = "code")) %>% 
  mutate(rdwy_orient = as.factor(readable)) %>% 
  select(-readable) -> df
levels(df$rdwy_orient)

df %>% 
  mutate(rdwy_surf_type_cd = as.character(rdwy_surf_type_cd)) %>% 
  left_join(df_dictionary %>% filter(column_name == "rdwy_surf_type_cd") %>% select(code, readable), c("rdwy_surf_type_cd" = "code")) %>% 
  mutate(rdwy_surf_type_cd = as.factor(readable)) %>% 
  select(-readable) -> df
levels(df$rdwy_surf_type_cd)


df %>% 
  mutate(relation_to_road = as.character(relation_to_road)) %>% 
  left_join(df_dictionary %>% filter(column_name == "relation_to_road") %>% select(code, readable), c("relation_to_road" = "code")) %>% 
  mutate(relation_to_road = as.factor(readable)) %>% 
  select(-readable) -> df
levels(df$relation_to_road)


df %>% 
  mutate(road_condition = as.character(road_condition)) %>% 
  left_join(df_dictionary %>% filter(column_name == "road_condition") %>% select(code, readable), c("road_condition" = "code")) %>% 
  mutate(road_condition = as.factor(readable)) %>% 
  select(-readable) -> df
levels(df$road_condition)

df %>% 
  mutate(road_owner = as.character(road_owner)) %>% 
  left_join(df_dictionary %>% filter(column_name == "road_owner") %>% select(code, readable), c("road_owner" = "code")) %>% 
  mutate(road_owner = as.factor(readable)) %>% 
  select(-readable) -> df
levels(df$road_owner)

df %>% 
  mutate(roadway_county = as.character(roadway_county)) %>% 
  left_join(df_dictionary %>% filter(column_name == "roadway_county") %>% select(code, readable), c("roadway_county" = "code")) %>% 
  mutate(roadway_county = as.factor(readable)) %>% 
  select(-readable) -> df
levels(df$roadway_county)

df %>% 
  mutate(spec_juris_cd = as.character(spec_juris_cd)) %>% 
  left_join(df_dictionary %>% filter(column_name == "spec_juris_cd") %>% select(code, readable), c("spec_juris_cd" = "code")) %>% 
  mutate(spec_juris_cd = as.factor(readable)) %>% 
  select(-readable) -> df
levels(df$spec_juris_cd)

df %>% 
  mutate(tcd_func_cd = as.character(tcd_func_cd)) %>% 
  left_join(df_dictionary %>% filter(column_name == "tcd_func_cd") %>% select(code, readable), c("tcd_func_cd" = "code")) %>% 
  mutate(tcd_func_cd = as.factor(readable)) %>% 
  select(-readable) -> df
levels(df$tcd_func_cd)

df %>% 
  mutate(urban_rural = as.character(urban_rural)) %>% 
  left_join(df_dictionary %>% filter(column_name == "urban_rural") %>% select(code, readable), c("urban_rural" = "code")) %>% 
  mutate(urban_rural = as.factor(readable)) %>% 
  select(-readable) -> df
levels(df$urban_rural)

df %>% 
  mutate(weather = as.character(weather)) %>% 
  left_join(df_dictionary %>% filter(column_name == "weather") %>% select(code, readable), c("weather" = "code")) %>% 
  mutate(weather = as.factor(readable)) %>% 
  select(-readable) -> df
levels(df$weather)

df %>% 
  mutate(work_zone_loc = as.character(work_zone_loc)) %>% 
  left_join(df_dictionary %>% filter(column_name == "work_zone_loc") %>% select(code, readable), c("work_zone_loc" = "code")) %>% 
  mutate(work_zone_loc = as.factor(readable)) %>% 
  select(-readable) -> df
levels(df$work_zone_loc)

df %>% 
  mutate(work_zone_type = as.character(work_zone_type)) %>% 
  left_join(df_dictionary %>% filter(column_name == "work_zone_type") %>% select(code, readable), c("work_zone_type" = "code")) %>% 
  mutate(work_zone_type = as.factor(readable)) %>% 
  select(-readable) -> df
levels(df$work_zone_type)


df %>% 
  mutate(aggressive_driving = as.character(aggressive_driving)) %>% 
  left_join(df_dictionary %>% filter(column_name == "aggressive_driving") %>% select(code, readable), c("aggressive_driving" = "code")) %>% 
  mutate(aggressive_driving = as.factor(readable)) %>% 
  select(-readable) -> df
levels(df$aggressive_driving)

df %>% 
  mutate(alcohol_related = as.character(alcohol_related)) %>% 
  left_join(df_dictionary %>% filter(column_name == "alcohol_related") %>% select(code, readable), c("alcohol_related" = "code")) %>% 
  mutate(alcohol_related = as.factor(readable)) %>% 
  select(-readable) -> df
levels(df$alcohol_related)

df %>% 
  mutate(bicycle = as.character(bicycle)) %>% 
  left_join(df_dictionary %>% filter(column_name == "bicycle") %>% select(code, readable), c("bicycle" = "code")) %>% 
  mutate(bicycle = as.factor(readable)) %>% 
  select(-readable) -> df
levels(df$bicycle)

cell_phone
df %>% 
  mutate(cell_phone = as.character(cell_phone)) %>% 
  left_join(df_dictionary %>% filter(column_name == "cell_phone") %>% select(code, readable), c("cell_phone" = "code")) %>% 
  mutate(cell_phone = as.factor(readable)) %>% 
  select(-readable) -> df
levels(df$cell_phone)

df %>% 
  mutate(cell_phone = as.character(cell_phone)) %>% 
  left_join(df_dictionary %>% filter(column_name == "cell_phone") %>% select(code, readable), c("cell_phone" = "code")) %>% 
  mutate(cell_phone = as.factor(readable)) %>% 
  select(-readable) -> df
levels(df$cell_phone)