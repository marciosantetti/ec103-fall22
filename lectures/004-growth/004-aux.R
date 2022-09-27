library(tidyverse)

data <- read_csv("epwt.csv")


data %>% 
  filter(Country %in% c("United States", "Korea Republicof", "China")) %>% 
  ggplot(aes(y = x, x = Year, color = Country)) +
  geom_line()



data %>% 
  filter(Country %in% c("United States", "Korea Republicof", "China")) %>% 
  ggplot(aes(y = log(x), x = Year)) +
  geom_line() +
  geom_line(aes(y = log(rho), x = Year)) +
  facet_wrap(~Country)



#----


dat <- read_xlsx("maddison.xlsx", sheet = 3)

dat_sample2 <- dat %>% 
  filter(country %in% c("United States")) %>% 
  filter(year >= 1940)


dat_sample2 %>% 
  mutate(gdppc_growth = ( ( gdppc - lag(gdppc, n = 1) ) / lag(gdppc, n = 1) ) * 100 ) %>% 
  ggplot(aes(x = year, y = gdppc_growth)) +
  geom_line() +
  geom_point()
