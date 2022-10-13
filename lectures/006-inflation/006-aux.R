library(tidyverse)
library(scales)
library(lubridate)
library(ggeasy)
library(ggrepel)


cex <- tibble(
  "Shelter" = 32.39 + 3.77 + 0.90,
  "Food" = 13.99 + 0.99,
  "Transportation" = 7.98 + 5.05,
  "Energy" = 7.54,
  "Medical care" = 6.99 + 1.49,
  "Education and Communication" = 6.01 + 0.47,
  "Apparel" = 2.67,
  "Recreation"= 3.67 + 1.95,
  "Other goods and services" = 1.07 + 1.63 + 1.45
)


cex <- cex %>% 
  pivot_longer(cols = Shelter:`Other goods and services`,
               names_to = "components",
               values_to = "share")

cex %>% 
  mutate(components = fct_reorder(components, share)) %>% 
  ggplot(aes(x = share, y = components)) +
  geom_col(alpha = 0.7) +
  scale_x_continuous(labels = percent_format(scale = 1),
                     breaks = seq(5, 50, 5)) +
  labs(y = "Component",
       x = "",
       title = "Consumer Price Index: Basket of goods",
       subtitle = "% share (November 2021)")



####


inf <- read_csv("inflation_data.csv")

period <- seq(as.Date("1958/01/01"), by = "quarter", length.out = 258)

colors = c("CPI" = "#0057e7", "CPI Core index" = "#d62d20", "PCE" = "#232b2b")

inf %>% 
  add_column(period) %>% 
  ggplot(aes(x = period)) +
  geom_line(aes(y = pce, color = "PCE"), size = 1, alpha = 0.6) +
  geom_point(aes(y = pce, color = "PCE"), size = 1, alpha = 0.6) +
  geom_line(aes(y = cpi_core, color = "CPI Core index"), size = 1, alpha = 0.6) +
  geom_point(aes(y = cpi_core, color = "CPI Core index"), size = 1, alpha = 0.6) +
  geom_line(aes(y = cpi, color = "CPI"), size = 1, alpha = 0.6) +
  geom_point(aes(y = cpi, color = "CPI"), size = 1, alpha = 0.6) +
  scale_color_manual(values = colors) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.4) +
  scale_x_yearquarter() +
  labs(x = "",
       y = "% change from a year ago",
       title = "Three different inflation measures",
       subtitle = "US: 1958 Q1 - 2022 Q2",
       caption = "Source: US FRED") +
  easy_add_legend_title("Measure") +
  easy_y_axis_title_size(13) +
  easy_plot_legend_size(12)
  
  
####

data <- read_csv('data_set.csv')



red_pink <- "#e64173"
met_slate <- "#23373b"
grey_mid <- "grey50"
red <- "#E02C05"
turquoise <- "#20B2AA"



##== Split data into three periods: 1948-1970, 1970-2000, and 2000-2020:


data_filter1 <- data %>% filter(DATE <= 1970)

data_filter2 <- data %>% filter(DATE >= 1970 & DATE <= 2000)

data_filter3 <- data %>% filter(DATE >= 2000)


##== Create a new column (CPI_CHANGE):


data_filter2 <- data_filter2 %>% mutate(CPI_CHANGE = c(NA, diff(CPI)))



## Full period:

data %>% 
  ggplot(aes(y=CPI, x=UNRATE)) + 
  geom_point(shape = 24, fill = red_pink,
             color = red_pink, size=2) +
  geom_hline(yintercept = 0, lty=2) +
  labs(title = 'Phillips Curve: US, 1948–2021',
       y = 'Inflation rate (%)',
       x = 'Unemployment rate (%)') +
  easy_y_axis_title_size(13) +
  easy_x_axis_title_size(13)


## Full period (fit):


data %>% 
  ggplot(aes(y=CPI, x=UNRATE)) + 
  geom_point(shape = 24, fill = red_pink,
             color = red_pink, size=2) +
  geom_smooth(se=F) +
  geom_hline(yintercept = 0, lty=2) +
  labs(title = 'Phillips Curve: US, 1948–2021',
       y = 'Inflation rate (%)',
       x = 'Unemployment rate (%)') +
  easy_y_axis_title_size(13) +
  easy_x_axis_title_size(13)

## 1948 - 1970:

data_filter1 %>% 
  ggplot(aes(y=CPI, x=UNRATE)) + 
  geom_point(shape = 24, fill = red_pink,
             color = red_pink, size=2) +
  geom_hline(yintercept = 0, lty=2) +
  labs(title = 'Phillips Curve: US, 1948–1970',
       y = 'Inflation rate (%)',
       x = 'Unemployment rate (%)') +
  easy_y_axis_title_size(13) +
  easy_x_axis_title_size(13)




## 1948 - 1970 (fit):

data_filter1 %>% 
  ggplot(aes(y=CPI, x=UNRATE)) + 
  geom_point(shape = 24, fill = red_pink,
             color = red_pink, size=2) +
  geom_smooth(formula = 'y ~ x + I(x^2)', method = 'lm', se=F) +
  geom_hline(yintercept = 0, lty=2) +
  labs(title = 'Phillips Curve: US, 1948–1970',
       y = 'Inflation rate (%)',
       x = 'Unemployment rate (%)') +
  easy_y_axis_title_size(13) +
  easy_x_axis_title_size(13)


data %>% 
  filter(DATE <= 1980) %>% 
  ggplot(aes(x = DATE, y = CPI)) +
  geom_line() +
  geom_point() +
  labs(x = "",
       y = "% change in CPI from a year ago",
       title = "Inflation rate: US, 1948–1980") +
  easy_y_axis_title_size(13) 


## 1970s

data %>% 
  filter(DATE >=1970 & DATE < 1981) %>% 
  ggplot(aes(y=CPI, x=UNRATE)) + 
  geom_point(shape = 24, fill = red_pink,
             color = red_pink, size=2) +
  geom_hline(yintercept = 0, lty=2) +
  labs(title = 'Phillips Curve: US, 1970–2000',
       y = 'Inflation rate (%)',
       x = 'Unemployment rate (%)') +
  easy_y_axis_title_size(13) +
  easy_x_axis_title_size(13)



## 1970 - 2000:

years <- c("1970", "1980", "1990", "2000")

data_filter2 %>% 
  ggplot(aes(y=CPI, x=UNRATE, label = ifelse(DATE %in% years, DATE, ""))) + 
  geom_point(shape = 24, fill = red_pink,
             color = red_pink, size=2) +
  geom_hline(yintercept = 0, lty=2) +
  geom_text_repel(family = 'Roboto Condensed', size = 4.5) +
  labs(title = 'Phillips Curve: US, 1970–2000',
       y = 'Inflation rate (%)',
       x = 'Unemployment rate (%)') +
  easy_y_axis_title_size(13) +
  easy_x_axis_title_size(13)


## 1970 - 2000 (fit):

data_filter2 %>% 
  ggplot(aes(y=CPI, x=UNRATE, label = ifelse(DATE %in% years, DATE, ""))) + 
  geom_point(shape = 24, fill = red_pink,
             color = red_pink, size=2) +
  geom_hline(yintercept = 0, lty=2) +
  geom_text_repel(family = 'Roboto Condensed', size = 4.5) +
  geom_smooth(formula = 'y ~ x + I(x^2)', method = 'lm', se=F) +
  labs(title = 'Phillips Curve: US, 1970–2000',
       y = 'Inflation rate (%)',
       x = 'Unemployment rate (%)') +
  easy_y_axis_title_size(13) +
  easy_x_axis_title_size(13)

## Acc

data_filter2 %>% ggplot(aes(y=CPI_CHANGE, x=UNRATE)) + 
  geom_point(shape = 24, fill = red_pink, color = red_pink, size=2) +
  geom_smooth(formula = 'y ~ x', method = 'lm', se = FALSE, color = turquoise) +
  theme_ipsum_rc() + geom_hline(yintercept = 0) +
  labs(title = 'Accelerationist Phillips Curve, US, 1970–2000',
       y = 'Change in inflation rate (%)',
       x = 'Unemployment rate (%)') +
  easy_y_axis_title_size(13) +
  easy_x_axis_title_size(13)

years2 <- c("2001", "2007", "2008", "2009", "2019", "2020", "2021")

data_filter3 %>%
  mutate(CPI_CHANGE = c(NA, diff(CPI))) %>% 
  ggplot(aes(y=CPI_CHANGE, x=UNRATE, label = ifelse(DATE %in% years2, DATE, ""))) + 
  geom_point(shape = 24, fill = red_pink, color = red_pink, size=2) +
  geom_text_repel(family = 'Roboto Condensed', size = 4.5) +
  geom_smooth(formula = 'y ~ x', method = 'lm', se = FALSE, color = turquoise) +
  theme_ipsum_rc() + geom_hline(yintercept = 0) +
  labs(title = 'Accelerationist Phillips Curve, US, 2000–2021',
       y = 'Change in inflation rate (%)',
       x = 'Unemployment rate (%)') +
  easy_y_axis_title_size(13) +
  easy_x_axis_title_size(13)

