library(tidyverse)
library(tsibble)

dat <- read_csv("dexchus.csv")


dat %>% 
  mutate(period = as.Date(date, format = "%m/%d/%Y"),
         exch = as.double(dexchus),
         correct_dates = seq(as.Date("1981-02-01"), by="day", length.out = 10906))


dates <- seq(from = as.Date())


unemployment_rate <- read_csv("unrate.csv")

unemployment_rate


net_exports <- read_csv("netexp.csv")

net_exports %>% 
  mutate(period = seq(as.Date("1947-01-01"), by = "quarter", length.out = 302)) %>% 
  ggplot(aes(x = period, y = netexp)) +
  geom_line() +
  scale_x_yearquarter()
