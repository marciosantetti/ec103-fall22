library(tidyverse)
library(lubridate)
library(tidyquant)
library(hrbrthemes)
library(scales)
library(readxl)
library(ggeasy)
library(viridis)
library(MetBrewer)

theme_set(theme_ipsum_rc())


####







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
  filter(country %in% c("United Kingdom", "China", "Japan", "India", "Italy")) %>% 
  filter(year >= 1000)

dat_sample %>% 
  ggplot(aes(x = year, y = gdppc, color = country)) +
  geom_line(size = 0.8) +
  scale_y_continuous(labels=scales::dollar_format()) +
  scale_color_met_d("Homer2") +
  labs(x = "", y = "GDP per capita", title = "Gross domestic product per capita, selected countries (1000-2018)") +
  easy_legend_at("bottom") +
  easy_remove_legend_title() +
  easy_plot_legend_size(13) +
  easy_y_axis_title_size(13)


###



co2 <- read_csv("co2.csv")


co2 %>% 
  filter(country %in% c("United Kingdom", "United States", "China", "Brazil")) %>% 
  ggplot(aes(x = year, y = co2_per_capita, color = country)) +
  geom_line(size = .8, alpha = 0.6) +
  easy_legend_at(to = "bottom") +
  labs(x = "", y = "Emissions per capita", title = "Carbon emissions per capita, 1800-2020") +
  easy_remove_legend_title() +
  easy_y_axis_title_size(13) +
  easy_plot_legend_size(13) +
  scale_color_met_d("Degas")
