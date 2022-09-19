library(tidyverse)
library(lubridate)

macro_data <- read_csv("gdp-data.csv")



macro_data %>% 
  ggplot(aes(x = year, y = log(gdp_ppp))) +
  geom_line(size = 1) +
  facet_wrap(~country) +
  geom_smooth(method = "lm", color = "black", linetype = "dashed")



