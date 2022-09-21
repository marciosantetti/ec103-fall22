
library(tidyverse)
library(kableExtra)

toy <- tibble(
  "Year" = c(2015, 2016, 2017),
  "Current Price (per unit)" = c(1.00, 1.10, 1.15),
  "Quantity sold" = c(200, 250, 230),
  "Nominal GDP" = c(200, 275, 264.5)
)  
  
toy %>% 
  kable()



real_gdp <- tibble(
  "Quarter" = c("2008q1", "2008q2", "2008q3", "2008q4", "2009q1", "2009q2", "2009q3", "2009q4"),
  "Nominal GDP" = c(14373.9, 14497.8, 14546.7, 14347.3, 14178.0, 14151.2, 14242.1, 14453.8), 
  "Real GDP" = c(13366.9, 13415.3, 13324.6, 13141.9, 12925.4, 12901.5, 12973.0, 13149.5)
)

real_gdp <- real_gdp %>% 
  mutate("GDP deflator" = `Nominal GDP`/`Real GDP`)



real_gdp %>% 
  select(-`Real GDP`) %>% 
  kable()


real_gdp %>% 
  mutate(new = `Nominal GDP` / `GDP deflator`)

