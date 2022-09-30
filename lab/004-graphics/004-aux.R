library(tidyverse)
library(gapminder)

data("gapminder")


gapminder %>% 
  group_by(continent) %>% 
  summarize(avg_lifeExp = mean(lifeExp)) %>% 
  ggplot(aes(x = avg_lifeExp, y = fct_reorder(continent, avg_lifeExp), fill = continent, color = continent)) +
  geom_col()


macro_data <- read_csv("gdp-data.csv")


macro_data %>% 
  group_by(country) %>% 
  summarise(avg_gdp = mean(gdp_ppp)) %>% 
  arrange(desc(avg_gdp)) %>% 
  ggplot(aes(x = avg_gdp, y = country, fill = country)) +
  geom_col()
