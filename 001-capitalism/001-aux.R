library(tidyverse)
library(lubridate)
library(tidyquant)
library(hrbrthemes)
library(scales)
library(readxl)
library(gapminder)

theme_set(theme_ipsum_rc())


####

cpi <- read_csv("cpi.csv")



cpi <- cpi %>% 
  mutate(period = seq(ymd("1947-1-1"), ymd("2022-6-1"), by = "month"))

cpi %>% 
  ggplot(aes(x = period, y = cpi)) +
  geom_line(size = 0.8)



cpi %>% tail()






### Using tidyquant:


fred_raw <- tq_get("CPIAUCSL",
                   get = "economic.data",
                   from = "1947-01-01")



fred_raw %>% 
  ggplot(aes(x = date, y = price)) +
  geom_line(size = 0.8)


fred_raw %>% 
  filter(date >= "2020-01-01") %>% 
  ggplot(aes(x = date, y = price)) +
  geom_line(size = 0.8)




###


dat <- read_xlsx("maddison.xlsx", sheet = 3)

dat_sample <- dat %>% 
  filter(country %in% c("United Kingdom", "China", "Japan", "India", "Italy"))

dat_sample %>% 
  ggplot(aes(x = year, y = gdppc, color = country)) +
  geom_line()



co2 <- read_csv("co2.csv")


co2 %>% 
  filter(country %in% c("United Kingdom", "United States")) %>% 
  ggplot(aes(x = year, y = co2_per_capita, color = country)) +
  geom_line()
