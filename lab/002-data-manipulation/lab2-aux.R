library(tidyverse)



toy <- read_csv("toy_data.csv")


toy %>% 
  filter(name %in% "Anna")



