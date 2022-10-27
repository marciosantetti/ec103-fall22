library(tidyverse)


dat <- read_csv("dexchus.csv")


dat %>% 
  mutate(period = as.Date(date, format = "%m/%d/%Y"),
         exch = as.double(dexchus),
         correct_dates = seq(as.Date("1981-02-01"), by="day", length.out = 10906))


dates <- seq(from = as.Date())