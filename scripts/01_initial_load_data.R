library(tidyverse)
library(janitor)

urls <- c("https://data.wprdc.org/dataset/3130f583-9499-472b-bb5a-f63a6ff6059a/resource/17012686-77d8-477b-b034-5b4a4715ec53/download/2004alcocrash.csv",
          "https://data.wprdc.org/dataset/3130f583-9499-472b-bb5a-f63a6ff6059a/resource/e6a24c09-c381-430b-b8ff-3ec5611538f5/download/2005alcocrash.csv",
          "https://data.wprdc.org/datastore/dump/01860f81-dd89-465a-ab73-1edc21858303",
          "https://data.wprdc.org/datastore/dump/1c7ce0ad-40d3-47e4-b654-6a6bdbfeca2c",
          "https://data.wprdc.org/dataset/3130f583-9499-472b-bb5a-f63a6ff6059a/resource/92608ca3-3a44-4fe8-af1a-967cfe8bc29d/download/2008alcocrash.csv",
          "https://data.wprdc.org/datastore/dump/4cccbf70-5709-4630-a0cd-cc4a3ed69be7",
          "https://data.wprdc.org/datastore/dump/93b3d18c-680f-4a1c-9896-72b446557505",
          "https://data.wprdc.org/dataset/3130f583-9499-472b-bb5a-f63a6ff6059a/resource/0950f9ac-fe2f-4441-a355-d60ee7653f8c/download/2011alcocrash.csv",
          "https://data.wprdc.org/dataset/3130f583-9499-472b-bb5a-f63a6ff6059a/resource/bcd2e0a7-d059-4ec8-9d5d-3d293aadd4c4/download/2012alcocrash.csv",
          "https://data.wprdc.org/dataset/3130f583-9499-472b-bb5a-f63a6ff6059a/resource/eb54323c-164b-4010-a78f-2dba4d382604/download/2013alcocrash.csv",
          "https://data.wprdc.org/dataset/3130f583-9499-472b-bb5a-f63a6ff6059a/resource/a1d00c8a-18dd-43ee-aa13-c2998ceb76ad/download/2014alcocrash.csv",
          "https://data.wprdc.org/datastore/dump/d90eb4fd-1234-4f3b-ba3d-422769cd3761",
          "https://data.wprdc.org/dataset/3130f583-9499-472b-bb5a-f63a6ff6059a/resource/9ccea350-e062-45e2-ade5-45e9378f40d2/download/reordered2016crashes.csv",
          "https://data.wprdc.org/datastore/dump/bf8b3c7e-8d60-40df-9134-21606a451c1a")

urls

dfs <- list()

#2004
dfs[[1]] <- read_csv(urls[1], col_types = cols(.default = "c"))
#2005
dfs[[2]] <- read_csv(urls[2], col_types = cols(.default = "c"))
#2006
dfs[[3]] <- read_csv(urls[3], col_types = cols(.default = "c"))
#2007
dfs[[4]] <- read_csv(urls[4], col_types = cols(.default = "c"))
#2008
dfs[[5]] <- read_csv(urls[5], col_types = cols(.default = "c"))
#2009
dfs[[6]] <- read_csv(urls[6], col_types = cols(.default = "c"))
#2010
dfs[[7]] <- read_csv(urls[7], col_types = cols(.default = "c"))
#2011
dfs[[8]] <- read_csv(urls[8], col_types = cols(.default = "c"))
#2012
dfs[[9]] <- read_csv(urls[9], col_types = cols(.default = "c"))
#2013
dfs[[10]] <- read_csv(urls[10], col_types = cols(.default = "c"))
#2014
dfs[[11]] <- read_csv(urls[11], col_types = cols(.default = "c"))
#2015
dfs[[12]] <- read_csv(urls[12], col_types = cols(.default = "c"))
#2016
dfs[[13]] <- read_csv(urls[13], col_types = cols(.default = "c"))
#2017
dfs[[14]] <- read_csv(urls[14], col_types = cols(.default = "c"))

bind_rows(dfs) %>%
  clean_names() -> df_combined_allegheny_county_crash_data_2004_2017_raw
dfs[[12]] %>% 
  count(CRASH_YEAR)

df_combined_allegheny_county_crash_data_2004_2017_raw %>% 
  count(crash_year)


#write_csv(df_combined_allegheny_county_crash_data_2004_2017_raw, "data/df_combined_allegheny_county_crash_data_2004_2017_raw.csv")