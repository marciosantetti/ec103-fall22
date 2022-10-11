library(tidyverse)
library(scales)
library(lubridate)
library(ggeasy)


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
  
  

               