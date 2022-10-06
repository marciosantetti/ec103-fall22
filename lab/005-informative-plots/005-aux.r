library(tidyverse)
library(scales)


unemp_data <- read_csv("unemp-data.csv")



## Evolution:


# 1:

unemp_data %>% 
    ggplot(aes(y = unemployed, x = period)) +
    geom_line() +
    labs(x = "", y = "u")


# 2:

unemp_data %>% 
  ggplot(aes(y = unemployed, x = period)) +
  geom_line() +
  labs(x = "Year", y = "Number of unemployed persons")


# 3:

unemp_data %>% 
  ggplot(aes(y = unemployed, x = period)) +
  geom_line() +
  labs(x = "Year", 
       y = "Number of unemployed persons",
       title = "Number of unemployed persons",
       subtitle = "United States, 1951-2021",
       caption = "Source: U.S. Bureau of Labor Statistics")


# 4: scales

unemp_data %>% 
  ggplot(aes(y = unemployed, x = period)) +
  geom_line() +
  labs(x = "Year", 
       y = "Number of persons",
       title = "Number of unemployed persons",
       subtitle = "United States, 1951-2021",
       caption = "Source: U.S. Bureau of Labor Statistics") +
  scale_y_continuous(labels = comma) 

unemp_data %>% 
  mutate(unemployed_th = unemployed / 1000) %>% 
  ggplot(aes(y = unemployed_th, x = period)) +
  geom_line() +
  labs(x = "Year", 
       y = "Persons (thousands)",
       title = "Number of unemployed persons",
       subtitle = "United States, 1951-2021",
       caption = "Source: U.S. Bureau of Labor Statistics") +
  scale_y_continuous(breaks = seq(2.5, 14, by=1)) +
  scale_x_continuous(breaks = seq(1955, 2020, by = 5))


# 5. scales with percentages

unemp_data %>% 
  mutate(un_rate = unemployed / labor_force) %>% 
  ggplot(aes(y = un_rate, x = period)) +
  geom_line() +
  labs(x = "", y = "") +
  labs(title = "A title") +
  scale_y_continuous(breaks = seq(0.02, 0.10, by = 0.01),
                     labels = percent_format(scale = 100)) +
  scale_x_continuous(breaks = seq(from = 1950, to = 2021, by = 10))
  theme_gray(base_size=16) 
  


# More complex example  


unemp_data <- unemp_data %>% 
  mutate(un_rate = unemployed / labor_force,
         part_rate = labor_force / non_inst_pop,
         emp_pop_ratio = employed / non_inst_pop)


unemp_data %>% 
  summarize(avg_un_rate = mean(un_rate),
            avg_emp_pop_ratio = mean(emp_pop_ratio),
            avg_part_rate = mean(part_rate)) %>% 
  pivot_longer(cols = c(avg_un_rate, avg_emp_pop_ratio, avg_part_rate), 
               names_to = "measure", 
               values_to = "value") %>% 
  ggplot(aes(x = value, y = measure)) +
  geom_col() +
  scale_x_continuous(labels = percent_format(scale = 1))
  
