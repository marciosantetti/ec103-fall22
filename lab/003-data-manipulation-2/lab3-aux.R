library(tidyverse)


# Data from World Bank.
# GDP: real GDP, 2017 US$ PPP
# Population: total population.



macro_data <- read_csv("gdp-data.csv")


#---

macro_data %>% 
  slice_max(gdp_ppp)



macro_data %>% 
  filter(country %in% c("United States", "South Africa"))


macro_data %>% 
  mutate(pop_millions = population / 1000000)


macro_data %>% 
  mutate(gdp_per_capita = gdp_ppp / population)


macro_data %>% 
  mutate(gdp_ppp_billions = gdp_ppp / 1000000000)

macro_data %>% 
  select(year, country, gdp_ppp) %>% 
  filter(year %in% 2021)


#---

macro_data %>% 
  ggplot(aes(x = year, y = log(gdp_ppp))) +
  geom_line(size = 1) +
  facet_wrap(~country) +
  geom_smooth(method = "lm", color = "black", linetype = "dashed")



