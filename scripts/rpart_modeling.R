library(tidyverse)
library(lubridate)
library(rpart)
library(rpart.plot)
library(broomstick)

theme_set(theme_bw())

#data <- read_csv("data/df_2013_cleaned.csv")
source("scripts/03_clean_data.R")

glimpse(df)

data %>% 
  mutate(crash_year = as.integer(as.double(crash_year)),
         crash_month = month(as.integer(crash_month), label = TRUE),
         day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
         time_of_day = as.integer(time_of_day),
         hour_of_day = as.integer(hour_of_day)) %>% 
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
                                     ntfy_hiwy_maint == "Y" ~ TRUE)) %>% 
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
                                ped_count < 1 ~ FALSE),
         fatal = as.integer(fatal),
         fatal = factor(as.logical(fatal))) %>% 
  filter(hour_of_day <= 23) -> df

glimpse(df)

#model 1
df %>% 
  select(fatal, crash_year, day_of_week, hour_of_day, weather, illumination, road_condition, collision_type, relation_to_road, urban_rural, speed_limit, person_count, automobile_count, motorcycle_count, bus_count, small_truck_count, heavy_truck_count, suv_count, van_count, bicycle_count, ped_count, speeding, cell_phone, alcohol_related, drinking_driver, aggressive_driving, fatigue_asleep, running_red_lt, running_stop_sign, tailgating, cross_median, impaired_driver)  %>% 
  mutate_at(vars(speeding:impaired_driver), as.integer) %>% 
  mutate_at(vars(speeding:impaired_driver), as.logical) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate_if(is.logical, as.factor) %>% 
  mutate(crash_year = crash_year -  2004) %>% 
  na.omit() -> df_model_1



glimpse(df_model_1)


model_1 <- rpart(fatal ~ ., 
               control = rpart.control(cp = .005),
               method = "class",
               data = df_model_1)
model_1_output <- as_tibble(predict(model_1, newdata = df_model_1))
model_1_output
df_model_1 %>% 
  bind_cols(model_1_output) %>% 
  rename(pred_non_fatal = `FALSE`,
         pred_fatal = `TRUE`) %>% 
  select(fatal, pred_fatal, pred_non_fatal, everything()) -> df_model_1
df_model_1
prp(model_1)
tidy(model_1)
summary(model_1)

#model 2
df %>% 
  select(fatal_count, injury_count, crash_year, day_of_week, hour_of_day, weather, illumination, road_condition, collision_type, relation_to_road, urban_rural, person_count, automobile_count, motorcycle_count, bus, small_truck_count, heavy_truck_count, suv_count, van_count, bicycle_count, ped_count, speed_limit,
         speeding, cell_phone, alcohol_related, drinking_driver, aggressive_driving, fatigue_asleep, running_red_lt, running_stop_sign, tailgating, cross_median, impaired_driver) %>%
  mutate_at(vars(speeding:impaired_driver), as.integer) %>% 
  mutate_at(vars(speeding:impaired_driver), as.logical) %>%
  mutate_if(is.character, factor) %>% 
  mutate_if(is.logical, as.factor) %>% 
  mutate(crash_year = crash_year -  2004,
         casualties = fatal_count + injury_count, 
         person_count_greater_2 = case_when(person_count >= 2 ~ TRUE,
                                             person_count < 1 ~ FALSE),
         person_count_greater_2 = factor(person_count_greater_2)) %>% 
  select(-c(fatal_count, injury_count, person_count)) %>% 
  na.omit() -> df_model_2

glimpse(df_model_2)
model_2 <- rpart(casualties ~ ., 
                 control = rpart.control(cp = .005),
                 data = df_model_2)
model_2_output <- predict(model_2, newdata = df_model_2)
head(model_2_output)
df_model_2 %>% 
  mutate(.pred = model_2_output, 
         .resid = .pred - casualties) %>% 
  select(casualties, .pred, .resid, everything()) -> df_model_2
df_model_2
prp(model_2)
tidy(model_2)
summary(model_2)

df_model_2 %>% 
  ggplot(aes(casualties, .pred)) +
  geom_jitter(alpha = .1) +
  geom_smooth()

df_model_2 %>% 
  ggplot(aes(casualties, .resid)) +
  geom_jitter(alpha = .1) +
  geom_smooth() 


df_model_2 %>% 
  ggplot(aes(casualties)) +
  geom_freqpoly()


#model 3
df %>% 
  select(fatal_count, injury_count, person_count, crash_year, day_of_week,
         hour_of_day, weather, illumination, road_condition, collision_type,
         relation_to_road, urban_rural, automobile, motorcycle, bus, small_truck,
         heavy_truck, suv, van, bicycle, pedestrian,
         speed_limit, speeding, cell_phone, alcohol_related, drinking_driver,
         aggressive_driving, fatigue_asleep, running_red_lt, running_stop_sign, tailgating,
         cross_median, impaired_driver) %>%
  mutate_at(vars(speeding:impaired_driver), as.integer) %>% 
  mutate_at(vars(speeding:impaired_driver), as.logical) %>% 
  mutate_if(is.character, factor) %>% 
  mutate_if(is.logical, as.factor) %>%
  mutate(crash_year = crash_year -  2004,
         fatalities_per_persons = fatal_count / person_count) %>%
  select(-c(fatal_count, injury_count, person_count)) %>% 
  #filter(fatalities_per_persons) %>% 
  na.omit() -> df_model_3
glimpse(df_model_3)

model_3 <- rpart(fatalities_per_persons ~ ., 
                 control = rpart.control(cp = .005),
                 data = df_model_3)
model_3_output <- predict(model_3, newdata = df_model_3)
head(model_3_output)
df_model_3 %>% 
  mutate(.pred = model_3_output, 
         .resid = .pred - fatalities_per_persons) %>% 
  select(fatalities_per_persons, .pred, .resid, everything()) -> df_model_3
df_model_3
prp(model_3)
tidy(model_3)
summary(model_3)

df_model_3 %>% 
  ggplot(aes(.pred, fatalities_per_persons)) +
  geom_jitter(alpha = .6) +
  geom_smooth()

