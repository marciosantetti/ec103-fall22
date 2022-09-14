library(tidyverse)
library(kableExtra)
library(hrbrthemes)
library(ggeasy)


theme_set(theme_ipsum_rc())

df <- tibble(
  "# Workers" = c(800, 1000, 1200, 1400),
  `Output (kg)` = c(500000, 570000, 630000, 684000),
  `Average productivity`= c(325, 570, 525, 490)
)


df %>%
  kbl() %>%
  kable_material(c("striped", "hover"))



dat <- tibble(
  x = seq(1:2800),
  y = seq(1:2800),
  lx = log(x),
  xsq = x^2
)



dat %>% 
  ggplot(aes(x = x, y = lx)) +
  geom_line(color = "#408480", alpha = 0.5, size = 0.9) +
  labs(x = 'Number of farmers', y = 'kg of grains (100,000)') +
  scale_x_continuous(labels = scales::comma) +
  easy_x_axis_labels_size(13) +
  easy_x_axis_title_size(13) +
  easy_y_axis_labels_size(13) +
  easy_y_axis_title_size(13)
