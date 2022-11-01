library(tidyverse)
library(ggeasy)

dat <- tibble(
  y = c(0,80,100,200,400,600,800,1000),
  c = c(100, 160, 175, 250, 400, 550, 700, 850)
)

dat %>% 
  ggplot(aes(y = c, x = y)) +
  geom_line(color = "#20b2aa", size = 1.5) +
  scale_y_continuous(breaks = seq(-100, 800, by = 500), labels = NULL) +
  scale_x_continuous(labels = NULL) +
  expand_limits(y = -50) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(y = "Aggregate consumption",
       x = "Aggregate Income",
       title = "C(Y)") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)

