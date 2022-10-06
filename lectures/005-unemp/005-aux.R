library(tidyverse)
library(hrbrthemes)
library(ggeasy)
library(kableExtra)
library(janitor)
library(ggrepel)

theme_set(theme_ipsum_rc())


###


unrate <- read_csv("unrate.csv")
open <- read_csv("job-openings-rate.csv")


unrate <- unrate %>% 
  filter(DATE >= "2000-12-01" & DATE <= "2022-06-01")


unrate <- unrate %>% select(UNRATE)

open <- open %>% 
  add_column(unrate) %>% 
  rename(openings = JTSJOR,
         unrate = UNRATE)


open %>% 
  filter(DATE >= "2020-05-01") %>% 
  ggplot(aes(y = openings, x = unrate)) +
  geom_point(alpha = 0.5, size = 1.2) +
  labs(title = "The US Beveridge Curve", subtitle = "May 2020 - June 2022")


#####


un_data <- read_csv("unemp-data.csv")


un_data <- un_data %>% 
  mutate(emp_pop_ratio = (employed / non_inst_pop) * 100,
         unemp_rate = (unemployed / labor_force) * 100,
         pct_lf = (labor_force / non_inst_pop) * 100)


cols <- c("Employed" = "#30845e", "Unemployed" = "#c76161", "Labor force" = "#528ff5")


un_data %>% 
  ggplot() +
  geom_line(aes(x = period, y = employed, color = "Employed"), size = 0.8) +
  geom_line(aes(x = period, y = unemployed, color = "Unemployed"), size = 0.8) +
  labs(y = "Number of persons", x = "", color = "Measure",
       title = "Labor market data: US, 1950–2021") +
  easy_legend_at("bottom") +
  easy_y_axis_title_size(13) +
  easy_plot_legend_size(13) +
  scale_y_continuous(labels = scales::comma)


un_data %>% 
  ggplot() +
  geom_line(aes(x = period, y = employed, color = "Employed"), size = 0.8) +
  geom_line(aes(x = period, y = unemployed, color = "Unemployed"), size = 0.8) +
  geom_line(aes(x = period, y = employed + unemployed, color = "Labor force"), size = 0.8) +
  labs(y = "Number of persons", x = "", color = "Measure") +
  easy_legend_at("bottom") +
  easy_y_axis_title_size(13) +
  easy_plot_legend_size(13) +
  scale_y_continuous(labels = scales::comma)


un_data %>% 
  ggplot() +
  geom_line(aes(x = period, y = unemp_rate)) +
  geom_line(aes(x = period, y = not_in_lf/non_inst_pop*100), color = "#30845e")





colors = c("Employment to population ratio" = "#30845e", "Labor force participation rate" = "#c76161")

un_data %>% 
  ggplot(aes(x = period)) +
  geom_point(aes(y = emp_pop_ratio, color = "Employment to population ratio")) +
  geom_line(aes(y = emp_pop_ratio, color = "Employment to population ratio")) +
  geom_point(aes(y = pct_lf, col = "Labor force participation rate")) +
  geom_line(aes(y = pct_lf, color = "Labor force participation rate")) +
  scale_color_manual(values = colors) +
  labs(color = "Measure") +
  easy_legend_at("bottom")


#####

un_lab <- read_csv("unemployment-lab.csv")

un_lab %>% 
  ggplot(aes(y = employment/population, x = period)) +
  geom_line()


#####


ex <- read_csv("example.csv")

ex %>% 
  select(Year, `Adult Population`, Employed, Unemployed)
  kable()
  
  
ex %>% 
  mutate(un_rate = Unemployed / `Labor Force` * 100,
         labor_force = Employed + Unemployed,
         not_lf = `Adult Population` - `Labor Force`,
         emp_pop_ratio = Employed / `Adult Population` * 100,
         part_rate = `Labor Force` / `Adult Population` * 100)




#####


dat <- read_csv("unrate_annual.csv")


dat <- dat %>% 
  clean_names() %>% 
  mutate(delta_u = c(NA, diff(unrate)),
         gdp_growth = ( ( gdp - lag(gdp, n = 1) ) / lag(gdp, n = 1) ) * 100 )


dat %>% 
  ggplot(aes(x = gdp_growth, y = delta_u)) + 
  geom_point(size = 2, color = "#800000", shape = 5) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(x = "GDP growth (%)",
       y = "Change in unemployment (p.p.)",
       title = "Okun's law: United States, 1949-2021") +
  easy_y_axis_title_size(13) +
  easy_x_axis_title_size(13)



###


years_num <- c("1970", "2008", "2019", "2000", "2021")

dat %>% 
  mutate(years = as.character(period)) %>% 
  filter(years %in% c("1970", "2008", "2019", "2000", "2007", "2021")) %>% 
  ggplot(aes(x = gdp_growth, y = delta_u)) + 
  geom_point(size = 2, color = "#800000", shape = 5) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(x = "GDP growth (%)",
       y = "Change in unemployment (p.p.)",
       title = "Okun's law: United States, selected years") +
  easy_y_axis_title_size(13) +
  easy_x_axis_title_size(13) +
  geom_text_repel(aes(label = years))





###









dat_filter <- dat %>% 
  filter(period <= 2010)


dat_filter %>% 
  ggplot(aes(x = gdp_growth, y = delta_u)) + 
  geom_point(size = 2, color = "#800000", shape = 5) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)

summary(lm(delta_u ~ gdp_growth, data = dat))
