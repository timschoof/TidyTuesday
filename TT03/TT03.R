# Tidy Tuesday - 17 April 2018
# Global mortality rates

# Data source: https://ourworldindata.org/
# Article: https://ourworldindata.org/what-does-the-world-die-from
# Tidy Tuesday: https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(here)
library(openxlsx)

# load data
d<-read.xlsx(here("global_mortality.xlsx"),sheet="share-of-deaths-by-cause-2016 (")

# TidyTuesday function of the week (from the stringr package)
# Remove '%' signs from column names
names(d) <- str_trim(str_remove_all(names(d),"[[:punct:]]"))

# select Japan (highest life expectancy) vs. Sierra Leone (lowest life expectancy)
JPN_SLE <- d %>%
  filter(country == "Japan" | country == "Sierra Leone")

# reorganise data frame: wide to long format
JPN_SLE_long <- JPN_SLE %>%
  gather(Cardiovasculardiseases:Terrorism, key = "cause", value="rate")

# relevel causes factor according to mean mortality rate Sierra Leone
causeOrder <- JPN_SLE_long %>%
  filter(country=="Sierra Leone") %>% # select Sierra Leone
  group_by(cause) %>% # group by cause
  summarise(avgMR = mean(rate,na.rm=T)) %>%# take the mean across years
  arrange(avgMR) # order causes according to mean for SLE
# relevel factor (the not so tidy way)
JPN_SLE_long$cause <- factor(JPN_SLE_long$cause,levels=causeOrder$cause)

# plot
JPN_SLE_long %>%
  ggplot(aes(cause, rate, colour=country))+ 
  geom_boxplot()+
  coord_flip()+
  labs(x = "", y = "Mortality rate (%)", 
       title = "Cause of death in Sierra Leone and Japan", 
       subtitle = "1990 - 2016")+
  theme(panel.background = element_rect(fill = "white" ),
        plot.background = element_rect(fill = "white" ),
        plot.title = element_text(hjust = 0.5))# center the title
# save
ggsave("MortalityRateJPNvsSLE.png")

# This plot has so much info, it's hard to read, so let's zoom in a little
# select causes of death that on average account for at least 2% of mortalities
topCauses <- JPN_SLE_long %>%
  group_by(cause) %>% # group by cause
  summarise(avgMR = mean(rate,na.rm=T)) %>%# take the mean across years
  arrange(desc(avgMR)) %>% # order cause according to mean
  filter(avgMR>2)
# select top causes (the not so tidy way)
JPN_SLE_topCauses<- JPN_SLE_long[JPN_SLE_long$cause %in% topCauses$cause,]

# plot: top 10 causes
JPN_SLE_topCauses %>%
  ggplot(aes(cause, rate, colour=country))+ 
  geom_boxplot()+
  coord_flip()+
  labs(x = "", y = "Mortality rate (%)", 
       title = "Cause of death in Sierra Leone and Japan", 
       subtitle = "Top 10 causes of death: 1990 - 2016")+
  theme(panel.background = element_rect(fill = "white" ),
        plot.background = element_rect(fill = "white" ),
        plot.title = element_text(hjust = 0.5))# center the title
# save
ggsave("TopMortalityRateJPNvsSLE.png")


