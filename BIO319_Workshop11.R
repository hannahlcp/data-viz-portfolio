# BIO319 Workshop 11 Script
# Hannah Chaudhry-Phipps
# 03/04/2023


# 1 Introduction -----------------------------------------------------------


install.packages('tidyverse')
library(tidyverse)
install.packages('RColorBrewer')
library(RColorBrewer)
install.packages('patchwork')
library(patchwork)


# 2 Summary Tables --------------------------------------------------------


beetles <- read.csv("dung_beetles.csv") %>%
  rename_with(tolower, everything()) %>%
  select(!starts_with("X")) %>%
  rename_with( ~ gsub("opis", "opris", .x), matches("Copis")) %>%
  pivot_longer(cols = c(3:68), names_to = 'spp', values_to = 'count') %>%
  separate_wider_delim("spp", "_",
                       names = c("genus",
                                 "species"))

# Group by and summarise

# Group by adds an underlying flag, to say that the data can be grouped by month
beetles %>%
  group_by(month)

# When group_by is then added to the summarise function, summarising 
# functions can be applied
total_pcm <- beetles %>%
  group_by(month) %>%
  summarize(total = sum(count))

# If you run the code below, the overall sum is shown, as the code doesn't know
# what to group the data by
beetles %>%
  summarize(total = sum(count))

# Plot a barplot
# Order the months correctly
total_pcm$month <- as.factor(total_pcm$month)
total_pcm$month <- factor(total_pcm$month, levels = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))

# geom_bar uses the COUNT (i.e., frequency of values in a column)
# We want geom_col (or could use geom_bar with stat = 'identity')
ggplot(data = total_pcm) +
  geom_col(aes(x = month, y = total))


# 3 Heatmaps --------------------------------------------------------------

beetles_group <- beetles %>%
  group_by(month, genus) %>%
  summarise(mean_count = mean(count))

beetles_group$month <- as.factor(beetles_group$month)
beetles_group$month <- factor(beetles_group$month, levels = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))

beetles_group %>% 
  ggplot(aes(x = month, y = genus, fill = mean_count)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7))

# Limit the fill scale, and set na.value for colours
beetles_group %>% 
  ggplot(aes(x = month, y = genus, fill = mean_count)) +
  geom_raster() + 
  scale_fill_continuous(limits = c(0, 10), na.value = '#56B1F7') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7))


# 4 Axes ------------------------------------------------------------------


guinea_world <- read.csv("number-of-reported-guinea-worm-dracunculiasis-cases.csv") %>% 
  rename(cases = Reported.cases.of.Guinea.worm.disease,
         country = Entity) %>%
  rename_with(tolower) %>% 
  filter(country == "World")

# Logarithmic scale
guinea_world %>%
  ggplot(aes(x = year, y = cases)) +
  geom_line() +
  geom_point() +
  scale_y_log10()
# The above scale stops at the lower limit of 10

# Make the scale run from 1 to 1 million and change the y 
# axis labels to more 'human readable'
guinea_world %>%
  ggplot(aes(x = year, y = cases)) +
  geom_line() +
  geom_point() +
  scale_y_log10(limits = c(1,1000000),
                breaks = c(10, 100, 1000, 10000, 100000, 1000000),
                labels = c(10, 100, '1,000', '10,000', '100,000', '1,000,000'))


# 5 Finding a Story to Tell -----------------------------------------------


guinea_countries <- read.csv("number-of-reported-guinea-worm-dracunculiasis-cases.csv") %>% 
  rename(cases = Reported.cases.of.Guinea.worm.disease,
         country = Entity) %>%
  rename_with(tolower) %>%
  mutate(country = gsub("Central African Republic",
                        "C.A.R.",
                        country)) %>% 
  filter(country != "World")

# Filter to countries with any cases of guinea worm
endemic_countries <- c("Angola","C.A.R.","Kenya","Yemen",                   
                       "Pakistan","Chad","Cameroon","Senegal","Ethiopia",
                       "Mauritania","Cote d'Ivoire","Mali","Togo","India",
                       "Benin","Niger","Burkina Faso","Uganda","Ghana",
                       "Sudan","South Sudan","Nigeria")

guinea_countries <- guinea_countries %>% filter(country %in% endemic_countries)

guinea_countries$country <- factor(guinea_countries$country,levels=endemic_countries)

guinea_countries %>%
  ggplot(aes(x = year, y = cases)) +
  geom_col(aes(fill = country)) +
  labs(title = 'Guinea worm worldwide cases 1986-2021')

# Let's look at high burden countries (those with more than 100,000 cases ever)

high_burden_countries <- c("Burkina Faso","Ghana","Niger","Nigeria",
                           "South Sudan","Sudan","Uganda")

# Highlighting the high burden countries
ggplot(guinea_countries,aes(year,cases,group=country)) + 
  geom_col(fill="light grey") +
  geom_col(fill="dark red",
           data=guinea_countries %>% filter(country %in% high_burden_countries)) +
  ggtitle("Guinea worm worldwide cases 1986-2021 (high-burden countries)")

# Highlighting each high burden country
ggplot(guinea_countries,aes(year,cases,group=country)) + 
  geom_col(fill="light grey") +
  geom_col(data=guinea_countries %>% filter(country %in% high_burden_countries),
           aes(fill = country[country %in% high_burden_countries])) +
  labs(title = 'Guinea worm worldwide cases 1986-2021', 
       fill = 'country')

# Limit x axis to view years since 2013 for high burden countries
ggplot(guinea_countries,aes(year,cases,group=country)) + 
  geom_col(fill="light grey") +
  geom_col(data=guinea_countries %>% filter(country %in% high_burden_countries),
           aes(fill = country[country %in% high_burden_countries])) +
  labs(title = 'Guinea worm worldwide cases 1986-2021', 
       fill = 'country') +
  xlim(2012.5,NA)

# Limit x axis to view years since 2013 for all countries
ggplot(guinea_countries,aes(year,cases,group=country,fill=country)) + 
  geom_col() + xlim(2012.5,NA)+
  ggtitle("Guinea worm worldwide cases 2012-2021")

# It appears a different set of countries are being affected since 2013
# So let's look at those
# Countries with any cases after 2011
persistent_countries <- c("Angola","Cameroon","Chad","Ethiopia",
                          "Mali","South Sudan","Sudan")


# 6 Telling your Story Well -----------------------------------------------


# Let's use manual colour scales to tell your story
# Assign the colour 'grey' to each country with guinea worm cases
gwcols <- rep("grey",length(endemic_countries))
# Now let us assign each grey to a name of country in gwcols vector
names(gwcols) <- endemic_countries

# Remove persistent countries that are also high-burden countries 
persistent_countries <- persistent_countries[!persistent_countries %in% high_burden_countries]

# Get a vector of 'blues' the same length as persistent_countries:
blues <- brewer.pal(length(persistent_countries),name="Blues")

# Shuffle blue colours so similar colours aren't next to each other
blues <- sample(blues)

# Assign blue colours (instead of grey) to the persistent_countries in gwcols
gwcols[persistent_countries] <- blues
# So now gwcols has all the endemic countries as grey, APART from the persistent countries
# which are blue

# View our plot with the persistent_countries as blue
ggplot(guinea_countries,aes(year,cases,group=country,fill=country)) + 
  geom_col() + 
  scale_fill_manual(values=gwcols) +
  ggtitle("Guinea worm worldwide cases 1986-2021")

# Now we want to do the same for the high burden countries, using the orange scales

# Get a vector of 'oranges' the same length as high burden countries
oranges <- brewer.pal(length(high_burden_countries),name="Oranges")

# Shuffle the oranges
oranges <- sample(oranges)

# Assign orange colours (instead of grey) to the high_burden_countries in gwcols
gwcols[high_burden_countries] <- oranges

# View our plot with the persistent countries as blue, and
# high burden countries as orange
LT_plot <- ggplot(guinea_countries,aes(year,cases,group=country,fill=country)) + 
  geom_col() + 
  scale_fill_manual(values=gwcols) +
  labs(x = '', y = '') +
  xlim(1985.5,2012.5)

ST_plot <- ggplot(guinea_countries,aes(year,cases,group=country,fill=country)) + 
  geom_col() + 
  scale_fill_manual(values=gwcols) +
  xlim(2012.5, NA) +
  scale_y_continuous(position = "right") +
  labs(x = '', y = '') +
  theme(legend.position = 'none')

scatter_plot <- guinea_world %>%
  ggplot(aes(x = year, y = cases)) +
  geom_line() +
  geom_point() +
  scale_y_log10(limits = c(1,1000000),
                breaks = c(10, 100, 1000, 10000, 100000, 1000000),
                labels = c(10, 100, '1,000', '10,000', '100,000', '1,000,000'))

# Patchwork
layout <- '
AAD
BCD
'

# Can use either of the following lines of code
# The order of plot input implicitly specifies the letters:
scatter_plot + LT_plot + ST_plot + guide_area() + 
  plot_layout(guides = 'collect', design = layout, widths = c(3,1)) +
  plot_annotation(title = 'Despite elimination in high-burden countries, low level transmission persists',
                  subtitle = 'Recorded Guinea worm case numbers, 1986-2021',
                  caption = 'Source: https://ourworldindata.org/guinea-worm-path-eradication')

# Or can explicity label plots
wrap_plots(A = scatter_plot, B = LT_plot, C = ST_plot, D = guide_area(), design = layout, guides = 'collect')







