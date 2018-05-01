library(tidyverse)
library(lubridate)

df <- read_csv("data/combined_allegheny_county_crash_data_2004-2017.csv")

df_dictionary <- read_csv("data/df_dictionary_rebuilt.csv") %>% 
  filter(!is.na(code))

factorize <- function(dataframe, column){
  column <- enquo(column)
  
  df %>% 
    mutate(!! quo_name(column) := as.character(!! column)) %>% 
    left_join(df_dictionary %>% filter(column_name == !! column) %>% select(code, readable), c(!! quo_name(column) = "code")) %>% 
    mutate(!! quo_name(column) := as.factor(readable)) %>% 
    select(-readable) -> df
}

factorize(df, collision_type)

test <- function(dataframe, column){
  print(column)
}
test(df, column = collision_type)

enquo(factorize)

levels(df$collision_type)




