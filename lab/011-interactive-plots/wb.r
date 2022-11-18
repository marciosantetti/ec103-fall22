library(tidyverse)
library(ggthemes)
library(plotly)
library(janitor)
library(tsibble)
library(hrbrthemes)
library(htmlwidgets)

gdp <- read_csv("gdpc1.csv")


gdp <- gdp %>% 
  clean_names() 

gdp %>% 
  mutate(date = yearquarter(date)) %>% 
  ggplot(aes(x = date, y = gdpc1)) +
  geom_line() +
  theme_light() 


p1 <- gdp %>% 
  mutate(date = yearquarter(date)) %>% 
  ggplot(aes(x = date, y = gdpc1)) +
  geom_line() +
  geom_area(alpha = 0.4, fill="#69b3a2") +
  theme_ipsum() 


p2 <- p1 %>% 
  ggplotly() %>% 
  layout(xaxis = list(rangeslider = list(visible = T)))


saveWidget(p2, "plotly.html") 
