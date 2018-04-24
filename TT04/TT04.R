# Tidy Tuesday - 24 April 2018
# Gender differences in Australian salaries

# Data source: https://data.gov.au/dataset/taxation-statistics-2013-14/resource/c506c052-be2f-4fba-8a65-90f9e60f7775?inner_span=True
# Article: http://www.womensagenda.com.au/latest/eds-blog/australia-s-50-highest-paying-jobs-are-paying-men-significantly-more/
# Tidy Tuesday: https://github.com/rfordatascience/tidytuesday

# Figure inspired by https://www.theguardian.com/news/ng-interactive/2018/apr/05/women-are-paid-less-than-men-heres-how-to-fix-it

require(tidyverse)

url_data <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/"
data_file <- "week4_australian_salary.csv"

data <- read_csv(str_c(url_data,data_file))

# Calculate gender pay gap: https://www.gov.uk/guidance/gender-pay-gap-reporting-make-your-calculations
d <- data %>%
  select(-c(X1,gender_rank,individuals)) %>%
  spread(key = gender, value = average_taxable_income) %>%
  na.omit() %>% # exclude occupations with no data without salary details for both men and women
  mutate(payGap = 100*((Male-Female)/Male))

# add column indicating whether women get paid more or less than  men
d <- d %>%
  mutate(paySkew = ifelse(payGap > 0, 'Women paid less','Women paid more'))

# beeswarm-like histogram
d %>%
ggplot(aes(x = payGap, fill = paySkew)) +
  geom_dotplot(method="histodot", binwidth = 3,colour = NA) +
  xlim(-100,100)+
  labs(x = "Mean gender pay gap as a percentage of men's pay", y = "", 
       title = "Gender pay gap in Australia", 
       subtitle = "2013-14 income year") +
  theme(panel.background = element_rect(fill = "white" ),
        plot.background = element_rect(fill = "white" ),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# save
ggsave("PayGap.png")



