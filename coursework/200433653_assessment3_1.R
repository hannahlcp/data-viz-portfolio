# BIO319 Assessment 3 Part 1
# 200433653 Hannah Chaudhry-Phipps
# 06/04/2023

# Installation of required packages
install.packages("tidyverse")
install.packages("patchwork")

# Load packages
library(tidyverse)
library(patchwork)


# 1 Section (a) --------------------------------------------------------


# Importing the data for the plot
cases_modelling <- read.table('/cloud/project/wmr2022-clean-annexes/wmr_cases_deaths_modelling.txt', 
                              sep = '\t', 
                              header = T)

# Subsetting the high burden countries where deaths in 2020 are greater than 20,000
high_burden <- cases_modelling$country[cases_modelling$year == 2020 & cases_modelling$deaths > 20000]
cases_modelling_hb <- subset(cases_modelling, country %in% high_burden)

# Total deaths for each given year, divided by 1000
# For high burden countries:
cases_modelling_hb_sum <- cases_modelling_hb %>% 
  group_by(year) %>% 
  summarise(sum_hb = sum(deaths)/1000)

# For all the countries:
cases_modelling_sum <- cases_modelling %>%
  group_by(year) %>%
  summarise(sum_all = sum(deaths)/1000)

# Merging total deaths for high burden countries and total countries
cases_modelling_merge <- merge(cases_modelling_hb_sum, 
                               cases_modelling_sum,
                              by = 'year')

# Plotting the data as a ribbon plot
plot_a <- cases_modelling_merge %>%
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = 0, ymax = sum_all, fill = 'low burden countries')) +
  geom_ribbon(aes(ymin = 0, ymax = sum_hb, fill = 'high burden countries')) +
  labs(y = 'deaths (x1000)', x = '') +
  scale_y_continuous(breaks = seq(0, 900, by = 200)) +
  theme(axis.text.x = element_blank()) +
  scale_fill_manual(name = '', values = c('low burden countries' = 'grey', 'high burden countries' = 'darkred')) +
  guides(fill = guide_legend(reverse = T))


# 2 Section (b) --------------------------------------------------------


# Calculate mortality value for high burden countries
cases_modelling_hb$mortality <- (cases_modelling_hb$deaths/cases_modelling_hb$population) * 100000

# Convert the country column to a factor 
cases_modelling_hb <- cases_modelling_hb %>% 
  mutate('country' = as.factor(country))

# Order the country factor levels
cases_modelling_hb$country <- factor(cases_modelling_hb$country, 
                                     levels = c('Uganda', 'Mali', 'Burkina Faso', 
                                                'Mozambique', 'Niger', 'United Republic of Tanzania', 
                                                'Democratic Republic of the Congo', 'Nigeria'))

# Plotting the data as a heatmap
plot_b <- cases_modelling_hb %>%
  ggplot(aes(x = year, y = country, fill = mortality)) +
  geom_tile() +
  labs(y = "mortality (deaths/100k)") +
  theme(legend.key.height = unit(3.5, "mm")) +
  scale_fill_gradient(breaks = c(200, 300))


# 3 Section (c) --------------------------------------------------------


# Calculating mean population size for high burden countries
# From 2000-2021 (22 years), divided by 1 million
cases_modelling_hb_mean <- cases_modelling_hb %>% 
  group_by(country) %>% 
  summarise(mean_pop = sum(population)/22000000)

# Plotting the data as a barplot
plot_c <- cases_modelling_hb_mean %>%
  ggplot(aes(x = mean_pop, y = country)) +
  geom_col() +
  labs(y = '', x = 'population (m)') +
  theme(axis.text.y = element_blank())


# 4 Patchwork --------------------------------------------------------


# Putting the plots together
# Gathering the guides into the remaining top-right quadrant
part_1 <- plot_a + plot_b + guide_area() + plot_c +
  plot_layout(widths = c(2, 1), heights = c(1,2), byrow = F, guides = 'collect') +
  plot_annotation(title = 'Total malaria burden in Africa 2000-21',
                  subtitle = 'Despite falling mortality, transmission remains dominated by 8 high burden countries',
                  caption = 'Source: World Malaria Report 2022')

# Save plot
ggsave(filename = '200433653_assessment3_1.png', 
       plot = part_1,
       width = 24,
       height = 16,
       units = 'cm')
