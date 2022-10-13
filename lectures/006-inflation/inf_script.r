library(tidyverse)
library(ggrepel)


dat <- read_csv("inf_data.csv")



####

brazil <- dat %>% 
  filter(country %in% "Brazil") %>% 
  mutate(change_inflation = inflation_rate - lag(inflation_rate, 1))


brazil %>% 
  write_csv("brazil.csv")


japan <- dat %>% 
  filter(country %in% "Japan") %>% 
  mutate(change_inflation = inflation_rate - lag(inflation_rate, 1))


japan %>% 
  write_csv("japan.csv")


nigeria <- dat %>% 
  filter(country %in% "Nigeria") %>% 
  mutate(change_inflation = inflation_rate - lag(inflation_rate, 1))



nigeria %>% 
  write_csv("nigeria.csv")


sweden <- dat %>% 
  filter(country %in% "Sweden") %>% 
  mutate(change_inflation = inflation_rate - lag(inflation_rate, 1))



sweden %>% 
  write_csv("sweden.csv")



india <- dat %>% 
  filter(country %in% "India") %>% 
  mutate(change_inflation = inflation_rate - lag(inflation_rate, 1))



india %>% 
  write_csv("india.csv")


####

dat %>% 
  filter(country %in% "Brazil") %>% 
  ggplot(aes(x = unemployment_rate, y = inflation_rate)) +
  geom_point() +
  geom_text_repel(aes(label = year))

dat %>% 
  filter(country %in% "Brazil") %>% 
  ggplot(aes(x = unemployment_rate, y = change_inflation)) +
  geom_point() +
  geom_text_repel(aes(label = year))



dat %>% 
  filter(country %in% "Japan") %>% 
  ggplot(aes(x = unemployment_rate, y = inflation_rate)) +
  geom_point()

dat %>% 
  filter(country %in% "Japan") %>% 
  ggplot(aes(x = unemployment_rate, y = change_inflation)) +
  geom_point() +
  geom_text_repel(aes(label = year))


dat %>% 
  filter(country %in% "Nigeria") %>% 
  ggplot(aes(x = unemployment_rate, y = inflation_rate)) +
  geom_point()


dat %>% 
  filter(country %in% "Sweden") %>% 
  ggplot(aes(x = unemployment_rate, y = inflation_rate)) +
  geom_point()


dat %>% 
  filter(country %in% "India") %>% 
  ggplot(aes(x = unemployment_rate, y = inflation_rate)) +
  geom_point()

