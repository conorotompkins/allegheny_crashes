library(tidyverse)
library(skimr)
library(janitor)
library(lubridate)
library(viridis)
library(leaflet)
library(leaflet.extras)

theme_set(theme_bw(base_size = 20))

options(scipen = 999, digits = 4)

source("scripts/02_factorize_columns.R")
source("scripts/03_clean_data.R")

df <- data
rm("data", "df_combined_allegheny_county_crash_data_2004_2017_factorized")


df %>% 
  mutate(casualties = fatal_count + injury_count) %>% 
  select(dec_lat, dec_long, casualties) %>% 
  rename(lat = dec_lat, 
         long = dec_long) %>% 
  na.omit() -> df_map


leaflet(df_map_small) %>% 
  setView(lng = -80.000926, lat = 40.441202, zoom = 12) %>% 
  clearBounds() %>% 
  addTiles() %>% 
  addMarkers(
  clusterOptions = markerClusterOptions()
)



df_map %>% 
  mutate(id = row_number()) %>% 
  filter(id <= 100000) -> df_map_small
  
leaflet(df_map) %>% 
  setView(lng = -80.000926, lat = 40.441202, zoom = 12) %>% 
  addTiles() %>% 
  addWebGLHeatmap(lng=~long, lat=~lat, 
                  intensity = ~casualties,
                  size = 80, units = "px", opacity = .5)

leaflet(df_map) %>% 
  setView(lng = -80.000926, lat = 40.441202, zoom = 12) %>% 
  addTiles() %>% 
  addHeatmap()


