library(tidyverse)
library(ggthemes)
library(janitor)
library(lubridate)
library(tsibble)


eff <- read_csv("fedfunds.csv")


eff %>% 
  clean_names() %>% 
  ggplot(aes(x = date, y = dff)) +
  geom_line() +
  geom_point()



## for rename(), new_name = old_name


eff <- eff %>% 
  clean_names() %>% 
  rename(fed_funds_rate = dff)

eff %>% 
  mutate(month = floor_date(date, "month")) %>% 
  group_by(month) %>% 
  summarize(avg_fed_funds = mean(fed_funds_rate)) %>% 
  ggplot(aes(x = month, y = avg_fed_funds)) +
  geom_line()



eff %>% 
  mutate(quarter = floor_date(date, unit = "quarter")) %>% 
  group_by(quarter) %>% 
  summarize(avg_fed_funds = mean(fed_funds_rate)) %>% 
  ggplot(aes(x = quarter, y = avg_fed_funds)) +
  geom_line() +
  #scale_x_yearquarter()
  scale_x_date(date_labels = "%Y %m")

eff %>% 
  mutate(year = floor_date(date, unit = "year")) %>% 
  group_by(year) %>% 
  summarize(avg_fed_funds = mean(fed_funds_rate)) %>% 
  ggplot(aes(x = year, y = avg_fed_funds)) +
  geom_line() +
  scale_x_yearmonth(date_breaks = "10 years", date_labels = "%Y %b")


eff %>% 
  filter(date >= "2019-07-01") %>% 
  ggplot(aes(x = date, y = fed_funds_rate)) +
  geom_line() +
  scale_x_yearmonth(date_breaks = "8 months") +
  scale_y_continuous(breaks = c(0.5, 1, 1.5, 2))

# or seq(from = 0, to = 4, by = 0.5)


unrate <- read_csv("unrate.csv")


unrate %>% 
  clean_names() %>% 
  mutate(quarter = floor_date(date, unit = "quarter")) %>% 
  group_by(quarter) %>% 
  summarize(avg_unrate_qtr = mean(unrate)) %>% 
  ggplot(aes(x = quarter, y = avg_unrate_qtr)) +
  geom_line() +
  scale_x_yearquarter(date_breaks = "70 months")
