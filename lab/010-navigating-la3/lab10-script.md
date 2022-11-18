library(tidyverse)
library(wbstats)





- Obtain data:

df2 <- wb_data(indicator = c("NY.GDP.PCAP.PP.KD", "SP.DYN.LE00.FE.IN", "SL.TLF.CACT.FE.ZS"), 
              start_date = 1990, 
              end_date = 2020,
              country = c("Cambodia", "South Africa", "China", "Nigeria"))



- Prep:

df2 <- df2 %>% 
  rename(gdp_per_capita = NY.GDP.PCAP.PP.KD,
         fem_life_exp = SP.DYN.LE00.FE.IN,
         fem_lfp_rate = SL.TLF.CACT.FE.ZS) %>% 
  select(date, country, fem_lfp_rate, fem_life_exp, gdp_per_capita) %>% 
  filter(date %in% c(1990, 2000, 2010, 2020))



- LFP:

df2 %>% 
  ggplot(aes(x = date, y = fem_lfp_rate)) +
  geom_col(alpha = 0.4, fill = "#f75882") +
  geom_line() +
  geom_point() +
  facet_wrap(~country)



- LE:

df2 %>% 
  ggplot(aes(x = date, y = fem_life_exp)) +
  geom_col(alpha = 0.4, fill = "#f75882") +
  geom_line() +
  geom_point() +
  facet_wrap(~country)



- GDP:

df2 %>% 
  ggplot(aes(x = date, y = gdp_per_capita)) +
  geom_col(alpha = 0.4, fill = "#f75882") +
  geom_line() +
  geom_point() +
  facet_wrap(~country)