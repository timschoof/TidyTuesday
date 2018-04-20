# Tidy Tuesday - 10 April 2018
# Average pay for top NFL players per position

# Data source: http://www.spotrac.com/rankings/
# Article with graphic: https://fivethirtyeight.com/features/running-backs-are-finally-getting-paid-what-theyre-worth/
# Tidy Tuesday: https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(here)
library(openxlsx)

# load data
df<-read.xlsx(here("tidy_tuesday_week2.xlsx"),sheet="nfl_salary")

# convert data frame to tibble - because I want to learn about tibbles
t<-as_tibble(df)

# reorganize tibble from wide to long format
longT <- t %>%
  gather(position,salary,-year) 

# select 16 highest-paid players in each position (n = 10), per year (n = 8)
sub16 <- longT %>%
  group_by(year,position) %>%
  top_n(16) %>%
  ungroup()

# compute average salary for this subset
meanSub16 <- sub16 %>%
  mutate(salary = salary/10^6) %>% # salary in millions
  group_by(year,position) %>%
  mutate(mean_salary = mean(salary)) %>%
  ungroup

# for plotting
meanSub16plot <- meanSub16 %>%
  mutate(position = fct_recode(position, "Running Back" = "Running.Back",
                             "Defensive Lineman" = "Defensive.Lineman",
                             "Offensive Lineman" = "Offensive.Lineman",
                             "Special Teamer" = "Special.Teamer",
                             "Tight End" = "Tight.End",
                             "Wide Receiver" = "Wide.Receiver")) # rename / recode some factor levels

# plot
meanSub16plot %>%
  ggplot() +
  geom_point(aes(x = year, y = salary), colour = "gray") +
  geom_line(aes(x = year, y = mean_salary), size = 1.2) + 
  facet_wrap(~position, nrow = 2) +
  labs(x = "", y = "Average salary \n (USD in millions)", 
       title = "Average salary of 16 highest-paid NFL players \n by position") +
  theme(panel.background = element_rect(fill = "#fcfcfc" ),
        plot.background = element_rect(fill = "#fcfcfc" ),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        plot.title = element_text(hjust = 0.5))# center the title

# save
ggsave("NFL.png", width = 10, height = 5)



