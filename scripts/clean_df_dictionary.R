library(tidyverse)
library(janitor)

read_csv("data/crashdatadictionary.csv") %>%
  clean_names() %>% 
  fill(column_name, description, data_type, .direction = "down") -> df_dictionary 

(df_dictionary %>% 
  filter(codes == "0 = No, 1 = Yes") -> df_dictionary_boolean_1)

(df_dictionary_boolean_1 %>% 
    mutate(codes = "0 = No") %>% 
    bind_rows(df_dictionary_boolean_1) %>% 
    mutate(codes = str_replace(codes, "0 = No, 1 = Yes", "1 = Yes")) %>% 
    separate(codes, sep = " = ", into = c("code", "readable")) -> df_dictionary_boolean_1)

(df_dictionary %>%
  filter(codes == "Y = Yes N = No") -> df_dictionary_boolean_2)

(df_dictionary_boolean_2 %>% 
  mutate(codes = "N = No") %>% 
  bind_rows(df_dictionary_boolean_2) %>% 
  mutate(codes = str_replace(codes, "Y = Yes N = No", "Y = Yes")) %>% 
  separate(codes, into = c("code", "readable")) -> df_dictionary_boolean_2)


(df_dictionary %>% 
  filter(codes == "1=Y, 0 = N") -> df_dictionary_boolean_3)

(df_dictionary_boolean_3 %>% 
    mutate(codes = "0 = N") %>% 
    bind_rows(df_dictionary_boolean_3) %>% 
    mutate(codes = str_replace(codes, "1=Y, 0 = N", "1 = Y")) %>% 
    separate(codes, into = c("code", "readable")) -> df_dictionary_boolean_3)

(df_dictionary %>% 
  anti_join(df_dictionary_boolean_1) %>% 
  anti_join(df_dictionary_boolean_2) %>% 
  anti_join(df_dictionary_boolean_3)-> df_dictionary_everything_else)

(df_dictionary_everything_else %>% 
  mutate(codes = str_replace(codes, "No code provided", "NA"),
         codes = str_replace(codes, "Degrees.Decimal Degrees", "NA"),
         codes = str_replace(codes, "00 to 23", "NA"),
         codes = str_replace(codes, "Degrees:Min:Sec.Dec Sec", "NA"),
         codes = str_replace(codes, "See additional resource", "NA")) -> df_dictionary_everything_else)

df_dictionary_everything_else$codes[df_dictionary_everything_else$codes == "NA"] <- NA

(df_dictionary_everything_else %>% 
  separate(codes, into = c("code", "readable")) -> df_dictionary_everything_else)

#need to combine dictionary DFs
colnames(df_dictionary_everything_else)
colnames(df_dictionary_boolean_1)
colnames(df_dictionary_boolean_2)
colnames(df_dictionary_boolean_3)


(df_dictionary_everything_else %>% 
  bind_rows(df_dictionary_boolean_1, df_dictionary_boolean_2, df_dictionary_boolean_3) -> df_dictionary_rebuilt)
write_csv(df_dictionary_rebuilt, "data/df_dictionary_rebuilt.csv")
