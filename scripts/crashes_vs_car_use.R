library(tidyverse)
library(vroom)
library(janitor)
library(lubridate)
library(sf)
library(tigris)
library(tidycensus)
library(crsuggest)
library(patchwork)
library(lehdr)

options(tigris_use_cashe = TRUE)

#get crash data
raw_data <- list.files("data/big/jan_2021", full.names = TRUE) %>% 
  map_dfr(vroom) %>% 
  clean_names()

raw_data %>% 
  glimpse()

raw_data %>% 
  filter(pedestrian == 1)

#filter down to crashes with pedestrians
raw_data <- raw_data %>% 
  mutate(crash_month_new = parse_number(crash_month) %>% month(., label = T)) %>% 
  select(crash_month, crash_year, pedestrian, longitude, latitude) %>% 
  filter(pedestrian == 1)

glimpse(raw_data)

#clean up crash data geography data
crash_data <- raw_data %>% 
  drop_na() %>% 
  mutate(longitude = parzer::parse_lon(longitude) * -1,
         latitude = parzer::parse_lat(latitude)) %>% 
  drop_na() %>% 
  filter(longitude != 0,
         latitude != 0) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = "NAD83")

#download allegheny county polygon
allegheny_county <- counties("PA", cb = TRUE) %>% 
  filter(NAME == "Allegheny") %>% 
  select(NAME, geometry)

#test crash geometries
crash_data %>% 
  st_filter(allegheny_county, .predicate = st_covered_by) %>% 
  ggplot() +
  geom_sf(data = allegheny_county) +
  geom_sf(alpha = .01, size = .3) +
  theme_void()

#get tract polygons for the county
ac_tracts <- tigris::tracts(state = "PA", county = "Allegheny") %>% 
  select(GEOID, geometry)

#filter crash data to those within AC, count by GEOID
tract_crash_count <- crash_data %>% 
  st_join(ac_tracts) %>% 
  st_drop_geometry() %>% 
  count(GEOID, sort = T, name = "pedestrian_crash_count")

tract_crash_count <- tract_crash_count %>% 
  full_join(st_drop_geometry(ac_tracts)) %>% 
  replace_na(list(pedestrian_crash_count = 0))

#map crashes involving pedestrians by tract
tract_crash_count %>% 
  left_join(ac_tracts) %>% 
  st_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = pedestrian_crash_count), size = NA) +
  scale_fill_viridis_c() +
  theme_void()

#get census vars
census_vars <- load_variables(year = 2015, dataset = "acs1") %>% 
  mutate(across(.cols = c(label, concept), str_to_lower))

# census_vars %>% 
#   filter(str_detect(label, "transport") | str_detect(concept, "transport")) %>% 
#   View()
# 
# census_vars %>% 
#   filter(str_detect(label, "vehicle")) %>% 
#   View()

#select vehicle availability questions
vehicle_availability_questions <- census_vars %>% 
  filter(name %in% c("B08014_003", "B08014_004", "B08014_005", "B08014_006", "B08014_007")) %>% 
  pull(name)

# census_vars %>% 
#   filter(name %in% c(vehicle_availability_questions)) %>% 
#   View()

# census_vars %>% 
#   filter(concept == "means of transportation to work") %>% 
#   #distinct(concept) %>% 
#   View()

# census_vars %>% 
#   filter(str_detect(concept, "population"),
#          str_detect(concept, " age")) %>% 
#   distinct(label, concept) %>% 
#   View()

# get_acs(table = "DP05",
#         state = "PA", county = "Allegheny",
#         geography = "tract") %>% 
#   View()

#find total pop variable
census_vars %>% 
  filter(name == "B01003_001")

#get worker and resident totals
residents <- grab_lodes(state = "PA", 
                        year = 2018, 
                        lodes_type = "rac",
                        job_type = "JT00",
                        segment = "S000",
                        agg_geo = "tract") %>% 
  rename(GEOID = h_tract,
         residents = C000) %>% 
  select(GEOID, residents)

workers <- grab_lodes(state = "PA", 
                      year = 2018, 
                      lodes_type = "wac",
                      job_type = "JT00",
                      segment = "S000",
                      agg_geo = "tract") %>% 
  rename(GEOID = w_tract,
         workers = C000) %>% 
  select(GEOID, workers)

lehd_combined <- residents %>% 
  left_join(workers) %>% 
  semi_join(ac_tracts) %>% 
  mutate(workers_and_residents = workers + residents) %>% 
  select(GEOID, workers_and_residents)

lehd_combined %>% 
  filter(workers_and_residents >= 400) %>% 
  ggplot(aes(workers_and_residents)) +
  geom_histogram() +
  scale_x_log10()

lehd_combined <- lehd_combined %>% 
  filter(workers_and_residents >= 400) 

#get census data about who owns cars, total population
vehicle_data <- get_acs("tract", variables = vehicle_availability_questions, summary_var = "B01003_001",
                        state = "PA", county = "Allegheny", geometry = F) %>% 
  left_join(census_vars, by = c("variable" = "name")) %>% 
  rename(residents = summary_est) %>% 
  select(GEOID, variable, concept, label, estimate, residents) 

#summarize by GEOID, calculate pct_with_car
vehicle_data <- vehicle_data %>% 
  group_by(GEOID) %>% 
  summarize(pop_with_a_car = sum(estimate),
            residents = mean(residents)) %>% 
  ungroup() %>% 
  mutate(pct_residents_with_car = pop_with_a_car / residents) %>% 
  select(GEOID, pop_with_a_car, residents, pct_residents_with_car)

vehicle_data %>% 
  ggplot(aes(residents)) +
  geom_histogram() +
  geom_vline(xintercept = 250)

vehicle_data %>% 
  filter(residents >= 250) %>% 
  full_join(ac_tracts) %>% 
  st_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = pct_residents_with_car), size = NA) +
  scale_fill_viridis_c() +
  theme_void()

vehicle_data %>% 
  filter(residents >= 250) %>% 
  ggplot(aes(residents, pop_with_a_car)) +
  geom_point()

vehicle_data %>% 
  filter(residents > 500) %>% 
  ggplot(aes(residents, pct_residents_with_car)) +
  geom_point()

vehicle_data <- vehicle_data %>% 
  filter(residents > 500)

pedestrians_struck_adj <- tract_crash_count %>% 
  full_join(vehicle_data) %>% 
  full_join(lehd_combined) %>% 
  full_join(ac_tracts) %>% 
  st_sf() %>% 
  st_drop_geometry() %>% 
  mutate(pedestrians_struck_per_1k_workers_residents = (pedestrian_crash_count / workers_and_residents) * 1000,
         pct_residents_with_car = pop_with_a_car / residents) %>% 
  select(GEOID, pedestrian_crash_count, pop_with_a_car, residents, pct_residents_with_car, workers_and_residents,  pedestrians_struck_per_1k_workers_residents)

pedestrians_struck_adj %>% 
  full_join(ac_tracts) %>% 
  st_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = pedestrians_struck_per_1k_workers_residents), size = NA) +
  scale_fill_viridis_c() +
  theme_void()

pedestrians_struck_adj %>% 
  ggplot(aes(pct_residents_with_car, pedestrians_struck_per_1k_workers_residents)) +
  geom_point() +
  theme_bw()


#final graph
allegheny_county_tracts <- tracts(state = "PA", county = "Allegheny", cb = TRUE) %>% 
  select(GEOID)

st_erase <- function(x, y) {
  st_difference(x, st_union(y))
}

ac_water <- area_water("PA", "Allegheny", class = "sf")

allegheny_county_tracts <- st_erase(allegheny_county_tracts, ac_water)

where_residents_have_cars <- vehicle_data %>% 
  full_join(allegheny_county_tracts) %>% 
  st_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = pct_residents_with_car,
              color = pct_residents_with_car),
          lwd = 0) +
  scale_fill_viridis_c(labels = scales::percent) +
  scale_color_viridis_c(guide = FALSE) +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(fill = "% of residents with a car") +
  theme_void(base_size = 15) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 8))

where_residents_are_hit_by_cars <- pedestrians_struck_adj %>% 
  full_join(allegheny_county_tracts) %>% 
  st_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = pedestrians_struck_per_1k_workers_residents,
              color = pedestrians_struck_per_1k_workers_residents),
          lwd = 0) +
  scale_fill_viridis_c() +
  scale_color_viridis_c(guide = FALSE) +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(fill = "Pedestrians hit per 1,000 workers + residents") +
  theme_void(base_size = 15) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 8))

scatter_plot <- pedestrians_struck_adj %>% 
  ggplot(aes(pedestrians_struck_per_1k_workers_residents, pct_residents_with_car)) +
  geom_point(alpha = .3) +
  scale_y_continuous(label = scales::percent) +
  #coord_fixed(ratio = 30) +
  labs(x = "Pedestrians hit per 1,000 workers + residents",
       y = "% of residents with a car") +
  theme_bw(base_size = 20)

combined_plot <- where_residents_have_cars + where_residents_are_hit_by_cars

final_plot <- combined_plot + plot_annotation(title = "Where residents own cars vs. where drivers hit pedestrians",
                                              subtitle = "Allegheny County, PA",
                                              caption = "Vehicle access data from 2015-2019 American Community Survey\n
                                              Worker + Resident data from 2018 LODES\n
                                              Crash data from Western Pennsylvania Regional Data Center\n
                                              @conor_tompkins",
                                              theme = theme(plot.title = element_text(size = 24),
                                                            plot.subtitle = element_text(size = 20),
                                                            plot.caption = element_text(size = 15,
                                                                                        hjust = 1)))


final_plot %>% 
  ggsave(filename = "output/pedestrian_crashes.png", height = 12, width = 12, dpi = 300)
