# BIO319 Assessment 3 Part 2
# 200433653 Hannah Chaudhry-Phipps
# 06/04/2023

# Installation of required packages
install.packages('tidyverse')
install.packages('countrycode')
install.packages('ggpubr')
install.packages('patchwork')

# Load packages
library(tidyverse)
library(countrycode)
library(ggpubr)
library(patchwork)


# 1 Import, clean, and tidy the data --------------------------------------


# Importing the malaria data, containing the number of cases by country, by species, from 2010-2021
cases_species <- read.table('/cloud/project/wmr2022-clean-annexes/wmr_cases_by_species.txt', 
                            sep = '\t', 
                            header = T)

# Make the country column a factor, the year column numeric, and select the country, year, and p_vivax columns
malaria_cases <- cases_species %>% 
  mutate('country' = as.factor(country)) %>%
  mutate('year' = as.numeric(year)) %>%
  select(country, year, p_vivax)

# Importing the cutaneous leishmaniasis data, containing the number of cases per country from 2005-2021
leishmaniasis_cases <- read.csv('/cloud/project/who2022_cutaneous_leishmaniasis.csv', 
                          header = T, 
                          na.strings = ('NA'))

# Tidy the data
# Clean country names, make the country column a factor, make the year column numeric, filter the year to 
# 2010-2021 (same as malaria cases), and order the dataset by country in alphabetical order and year in 
# descending order
leishmaniasis_cases <- leishmaniasis_cases %>% 
  mutate('country' = gsub('Côte', 'Cote', country)) %>%
  mutate('country' = gsub('Türkiye', 'Turkey', country)) %>%
  mutate('country' = as.factor(country)) %>%
  mutate('year' = as.numeric(year)) %>%
  filter(year %in% 2010:2021) %>%
  arrange(country, -desc(year))


# 2 Group data by continent -------------------------------------------


# Group the P. vivax malaria cases by continent
malaria_cases$continent <- countrycode(sourcevar = malaria_cases$country, 
                                       origin = "country.name", 
                                       destination = "continent")

# Remove NA values, and summarise the total cases for each continent
# As there are no cases for Europe, add a row with value 0, and convert the continent column to a factor
malaria_continent <- malaria_cases %>% 
  group_by(continent) %>% 
  summarize(sum_cases = sum(p_vivax, na.rm = T)) %>%
  add_row(continent = 'Europe', sum_cases = 0) %>%
  mutate('continent' = as.factor(continent)) 

# Order the continent factor levels, with the 0 continent value at the beginning, and 
# then from largest to smallest sum_cases value
malaria_continent$continent <- factor(malaria_continent$continent, 
                                            levels = c('Europe', 'Asia', 'Africa', 'Americas', 'Oceania'))

# Group the cutaneous leishmaniasis cases by continent
leishmaniasis_cases$continent <- countrycode(sourcevar = leishmaniasis_cases$country, 
                                             origin = 'country.name', 
                                             destination = 'continent')

# Remove NA values, and summarise the total cases for each continent
# As there are no cases for Oceania, add a row with value 0, and convert the continent column to a factor
leishmaniasis_continent <- leishmaniasis_cases %>% 
  group_by(continent) %>% 
  summarise(sum_cases = sum(cases, na.rm = TRUE)) %>%
  add_row(continent = 'Oceania', sum_cases = 0) %>%
  mutate('continent' = as.factor(continent)) 

# Order the continent factor levels, with the 0 continent value at the beginning, and 
# then from largest to smallest sum_cases value
leishmaniasis_continent$continent <- factor(leishmaniasis_continent$continent, 
                                            levels = c('Oceania', 'Asia', 'Americas', 'Africa', 'Europe'))


# 3 Malaria proportional circle graph plot ------------------------------------------------------------------


# Colour palette
tol_light <- c('#BBCC33', '#AAAA00', '#77AADD', '#EE8866', '#EEDD88', '#FFAABB', '#99DDFF', '#44BB99', '#DDDDDD')

# Plot
malaria_plot <- malaria_continent %>%
  ggplot(aes(fill = continent, y = sum_cases)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = sqrt(sum_cases/pi))) + # radius is proportional to sum_cases for each continent
  xlim(-2000, 2100) + # set the x axis limits
  labs(title = 'Malaria Cases') + 
  theme_void() + # remove the background
  coord_fixed() + # fix the shape as a circle
  scale_fill_manual(values = c(Europe = '#BBCC33', Asia = '#FFAABB', Africa = '#EE8866', 
                               Americas = '#99DDFF', Oceania = '#EEDD88')) + # fill in continent colours manually
  theme(plot.title = element_text(hjust = 0.5), # centralise the heading
        legend.position = 'none') # remove the legend

# Add labels to the plot
malaria_plot <- malaria_plot + 
  geom_label_repel(aes(x = sqrt(sum_cases/pi), # x coordinate of the label is related to the radius value of each geom_circle
                 y = 0.3*sqrt(sum_cases/pi)), # y coordinate of the label is related to the 0.3*radius value of each geom_circle
             label = ifelse(malaria_continent$sum_cases > 0, 
                            malaria_continent$sum_cases, 
                            NA), # do not show labels for Europe (value = 0)
             size = 3, # make label text smaller
             segment.color = 'transparent') # remove line joining label to point


# 4 Leishmaniasis proportional circle graph plot ----------------------------


leishmaniasis_plot <- leishmaniasis_continent %>%
  ggplot(aes(fill = continent, y = sum_cases)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = sqrt(sum_cases/pi))) + # radius is proportional to sum_cases for each continent
  xlim(-1100, 1100) + # set the x axis limits
  labs(title = 'Leishmaniasis Cases', 
       fill = 'Continent') + # edit legend title
  theme_void() + # remove the background
  coord_fixed() + # fix the shape as a circle
  guides(fill = guide_legend(override.aes = aes(label = ""))) + # remove 'a' appearing in the legend when labels are added
  scale_fill_manual(values = c(Europe = '#BBCC33', Asia = '#FFAABB', Africa = '#EE8866', 
                               Americas = '#99DDFF', Oceania = '#EEDD88')) + # fill in continent colours manually
  theme(plot.title = element_text(hjust = 0.5), # centralise the heading
        legend.position = 'right', # place legend on the right
        legend.text = element_text(size = 9), # increase legend font sizes
        legend.title = element_text(size = 11)) 

# Add labels to the plot
leishmaniasis_plot <- leishmaniasis_plot +
  geom_label_repel(aes(x = sqrt(sum_cases/pi), # x coordinate of the label is related to the radius value of each geom_circle
                 y = 0.1*sqrt(sum_cases/pi)), # y coordinate of the label is related to the 0.1*radius value of each geom_circle
             label = ifelse(leishmaniasis_continent$sum_cases > 0, 
                            leishmaniasis_continent$sum_cases, 
                            NA), # do not show labels for Oceania (value = 0)
             size = 3, # make label text smaller
             segment.color = 'transparent') # remove line joining label to point


# 5 Final plot ------------------------------------------------------------


# Using ggpubr::ggarrange to arrange the two plots together, in order to keep the circles proportional to each other
combined_plot <- ggarrange(malaria_plot, leishmaniasis_plot)

# Italicised subtitle
subtitle <- expression(paste('Total ', italic('Plasmodium vivax'), ' and cutaneous leishmaniasis cases from 2010-21'))

# Using patchwork to annotate the plot
final_plot <- combined_plot +
  plot_annotation(title = 'Malaria and Leishmaniasis Cases by Continent',
                  subtitle = subtitle,
                  caption = 'Source: WMR 2022 and WHO Leishmaniasis Data 2021 | Graphic: H. Chaudhry-Phipps')

# Save plot
ggsave(filename = '200433653_assessment3_2.png', 
       plot = final_plot,
       width = 25,
       height = 16,
       units = 'cm')
