library(tidyverse)
library(janitor)
library(skimr)

theme_set(theme_bw())

df <- read_csv("data/combined_allegheny_county_crash_data_2004-2017.csv")

(df %>% 
    #select(-c(id, crash_crn, flag_crn, roadway_crn, latitude, longitude, dec_lat, dec_long)) %>% 
    summarise_all(funs(n_distinct(.))) %>% 
    gather(column_name, unique_values) %>% 
    arrange(desc(unique_values)) %>% 
    mutate(column_name = fct_reorder(column_name, unique_values))-> df_unique_values)

df_unique_values %>% 
  #filter(unique_values <= 3) %>% 
  ggplot(aes(fct_reorder(column_name, unique_values), unique_values, group = 1)) +
  geom_line() +
  coord_flip()
  

(df %>% 
  #select(-c(id, crash_crn, flag_crn, roadway_crn, latitude, longitude, dec_lat, dec_long)) %>% 
  gather(key = column_name, value = value) %>% 
  count(column_name, value) %>% 
  arrange(column_name, desc(n)) -> df_columns)

(df_columns %>% 
  left_join(df_unique_values) %>% 
  arrange(desc(unique_values), column_name, desc(n)) -> df_columns)

View(df_columns %>% 
  filter(unique_values <= 11))

df_columns %>% 
  filter(unique_values <= 11,
         !str_detect(column_name, "ind|count"),
         unique_values >= 3) %>%
  distinct(column_name) %>% 
  View()
