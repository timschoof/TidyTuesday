# Tidy Tuesday - 3 April 2018
# Average in-state tuition fees in the United States

# Data source: https://onlinembapage.com/average-tuition-and-educational-attainment-in-the-united-states/
# Tidy Tuesday: https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(here)
library(openxlsx)
library(fiftystater) # USA map with nice Alaska and Hawaii insets
library(viridis) # colour mapping suitable for people with colour blindness
library(gganimate)
library(magick)

# HELLO WORLD

# load data
d<-read.xlsx(here("us_avg_tuition.xlsx"),sheet="Table 5")

# restructure data frame (lower case the States, and convert from wide to long format)
dd<- d %>%
  mutate(State = tolower(State)) %>%
  gather(Year, Fee, -State)

# plot map
p <- ggplot(dd,aes(map_id = State, frame = Year)) +
  geom_map(aes(fill = Fee), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  scale_fill_viridis(name="USD", direction = -1) +
  labs(x = "", y = "", title = "Average in-state tuition fees in the United States:") +
  theme(legend.position = "right", 
        panel.background = element_blank()) 

# little hack to get magick (to create the animation/gif) to work
magickPath <- shortPathName("C:/Program Files/ImageMagick-6.9.9-Q16-HDRI/convert.exe")
ani.options(convert=magickPath)

# animate plot
gganimate(p,interval = 1)

# save as gif
gganimate(p, here("US_tuition_map.gif"))
