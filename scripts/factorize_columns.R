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
