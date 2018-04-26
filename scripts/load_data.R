library(tidyverse)

urls <- c("https://data.wprdc.org/dataset/3130f583-9499-472b-bb5a-f63a6ff6059a/resource/e6a24c09-c381-430b-b8ff-3ec5611538f5/download/2005alcocrash.csv",
              "https://data.wprdc.org/dataset/3130f583-9499-472b-bb5a-f63a6ff6059a/resource/e6a24c09-c381-430b-b8ff-3ec5611538f5/download/2005alcocrash.csv")


(data <- urls %>%
  map_dfr(read_csv))
