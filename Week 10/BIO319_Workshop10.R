# BIO319 Workshop 10 Script
# Hannah Chaudhry-Phipps
# 27/03/2023


# 2 Annotating Data Points ------------------------------------------------


install.packages("palmerpenguins")
library(palmerpenguins)
install.packages("tidyverse")
library(tidyverse)

# Subset penguins dataframe to the the five heaviest (Gentoo) penguins
big_penguins <- penguins %>%
  filter(species == "Gentoo",!is.na(body_mass_g)) %>% 
  arrange(body_mass_g) %>% 
  tail(n = 5L)

# Add a column with names to big_penguins
big_penguins$names <- c("Dwayne", "Hulk", "Giant", "Gwendoline", "Usain")

# Plot all Gentoo penguins and use big_penguins dataframe for labels
penguins %>% filter(species == "Gentoo") %>%
  ggplot(aes(x = body_mass_g, y = flipper_length_mm)) +
  geom_point(aes(colour = flipper_length_mm)) +
  geom_text(
    data = big_penguins,
    mapping = aes(label = names),
    nudge_x = -1.5,
    nudge_y = -0.5,
    colour = "red"
  ) +
  xlim(3900, 6400)
# Above, note that geom_text() is switching to different data (the big_penguins
# dataframe). However, it inherits the position mappings from ggplot()
# xlim specifies the x axis length

# If the desired labels are already in the dataframe (as opposed to another
# dataframe), we can filter the data argument to geom_text()
penguins %>% filter(species == "Adelie") %>%
  ggplot(aes(x = body_mass_g, y = flipper_length_mm)) +
  geom_point() +
  geom_text(
    data = filter(penguins, species == "Adelie" &
                    flipper_length_mm > 200),
    mapping = aes(label = island),
    nudge_y = -0.7
  )

# Produce the plot on the worksheet

penguins %>% filter(island == "Biscoe") %>%
  ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point(mapping = aes(colour = species)) +
  geom_text(
    data = filter(penguins, island == "Biscoe" &
                    bill_depth_mm > 20.0 |
                    island == "Biscoe" &
                    bill_length_mm >= 54),
    mapping = aes(label = sex),
    nudge_y = -0.5
  )


# 3 Facets ----------------------------------------------------------------


# Faceting produces several small plots separated by the same categorical variables
# There are two types of faceting, facet_wrap() and facet_grid()

# Firstly, facet_wrap() takes a number of plots and 'wraps' them into a panel

# Reading in data
modeltab <- read.table("wmr_modelling.txt",sep="\t",header=T)

# Subsetting to the first half or so for readability
modeltab_short <- head(modeltab, n = 506L)

# Plotting deaths in years 2019-2021 faceted by country
modeltab_short %>% drop_na(cases_low) %>% filter(year >2018) %>%
  ggplot(aes(x = year, y = deaths)) +
  geom_col(fill = "firebrick") +
  facet_wrap(~country, ncol = 5, dir = "v")
# drop_na() drops rows where any column specified contains NA values

# Secondly, facet_grid() forms a matrix of panels, defined by row and column 
# faceting variables (e.g., sex and species in the below example)
# All combinations of the variables exist in the data

penguins %>% drop_na() %>% ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point() +
  facet_grid(sex ~ species)
# The facet_grid() formula determines first the rows, and then the columns

# Therefore, we can control how we want plots laid out, which are separated
# by only just one variable
p_plot <- penguins %>% drop_na() %>%
  ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point()

# Below, the columns are species
p_plot + facet_grid(. ~ species)

# Below, the rows are species
p_plot + facet_grid(species ~ .)

# Produce the plot on the worksheet

modeldeaths <- read.table("wmr_cases_deaths_modelling_summaries.txt",sep="\t",header=T)

modeldeaths %>% filter(!(region == "Total")) %>%
  ggplot(aes(x = year, y = deaths)) +
  geom_col(fill = "darkblue") +
  facet_wrap(~region, ncol = 3, scales = "free")

# Alternatively, we can use the %in% operator, to subset the data
modeldeaths %>% filter(!region %in% "Total") %>%
  ggplot(aes(x = year, y = deaths)) +
  geom_col(fill = "darkblue") +
  facet_wrap(~region, ncol = 3, scales = "free")


# 4 Patchwork -------------------------------------------------------------


# Patchwork combines several panels into a larger figure
install.packages("patchwork")
library(patchwork)

p1 <- penguins %>% drop_na() %>%
  ggplot(aes(x = bill_depth_mm, y = bill_length_mm, colour = species)) +
  geom_point() + 
  facet_grid(. ~ species)

p2 <- penguins %>%  drop_na() %>%
  ggplot(aes(x = flipper_length_mm)) +
  geom_histogram(aes(fill = species), position = "identity")

p3 <- penguins %>% drop_na() %>% 
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_violin(aes(fill = sex))

# Using patchwork:
p1/(p2+p3) 
p2 | (p1/p3)
(p1/p3) | p2
p1 / p2 / p3
(p2+p3) / p1

# We cab add annotations
p1/(p2+p3) + plot_annotation(tag_levels = "a",
                             title = "Plenty of penguin plots")

# Note the %in% operator for subsetting
# We can align plots with the same x- or y- axis
p_deaths <- modeltab %>% filter(country %in% c("Angola", "Burkina Faso", "Chad")) %>% 
  ggplot(aes(x = year, y = deaths, colour = country)) +
  geom_point() +
  geom_line() +
  xlim(1999,2022)

p_pop <- modeltab %>% filter(country %in% c("Angola", "Burkina Faso", "Chad")) %>% 
  ggplot(aes(x = year, y = population, fill = country)) +
  geom_col(position = "dodge") +
  xlim(1999,2022)

p_deaths/p_pop


# 5 Colours ---------------------------------------------------------------


# We use colour in three ways:
# a) To map categorical variables to a colour
# b) To map continous variables to a colour
# c) To manually colour geometric elements independently from a variable 
# (e.g., the penguin species on the scatterplot)

# Discrete colour scales
s_counts <- penguins %>% ggplot(aes(x = species, fill = species)) +
  geom_bar()

s_counts + scale_fill_manual(values = c("yellow2", "magenta", "darkblue"))

# Or we can make a vector of our favourite colours
mypalette <- c("yellow2", "magenta", "darkblue")
s_counts + scale_fill_manual(values = mypalette)

# Or we can use built-in colour palettes
install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()

# RColorBrewer has three types of palettes suitable for:
# a) ranked discrete graphs e.g., a series of years
# b) unranked categorical data
# c) discrete diverging data going from low to high, through 0

# Looking at categorical data
brew_1 <- s_counts + scale_fill_brewer(palette = "Set1")

brew_2 <- s_counts + scale_fill_brewer(palette = "Dark2", direction = -1)

brew_1 + brew_2 

# Viridis scales are designed to be colour-blind friendly
# The scale_fill_viridis_d() is for categorical data
viri_1 <- s_counts + scale_fill_viridis_d() #Uses default option viridis

viri_2 <- s_counts + scale_fill_viridis_d(option = "plasma")
# Go to help page for options argument

viri_1 + viri_2

# Looking at continuous data
# The scale_fill_viridis_c() is for continuous data

con_plot_1 <- penguins %>% drop_na() %>%
  ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point(aes(size = body_mass_g, colour = body_mass_g))

con_plot_2 <- con_plot_1 + scale_colour_viridis_c(option = "magma")

con_plot_1 + con_plot_2

# NA values
penguins %>%
  ggplot(mapping = aes(x = species, y = body_mass_g)) +
  geom_violin(aes(fill = sex)) +
  scale_fill_brewer(palette = "Set2", na.value = "yellow2")
# We can override the palette colour by specifying na.value 

# Experiment with previous graph scripts

practice_plot <- penguins %>%
  ggplot(mapping = aes(x = species, y = body_mass_g)) +
  geom_violin(aes(fill = sex)) +
  labs(title = "Weight distribution among penguins",
       subtitle = "Plot generated by E. Busch-Nentwich, March 2023",
       x = "Species",
       y = "Weight in g",
       caption = "Data from Palmer Penguins package\nhttps://allisonhorst.github.io/palmerpenguins/"
  )

practice_plot + scale_fill_brewer(palette = "Accent", na.value = "yellow2")


# 6 Themes ----------------------------------------------------------------


# The theme determines the colour of the plot, grid lines and axes, 
# and where the legend goes, etc.
con_plot_3 <- con_plot_1 + theme_classic()
con_plot_4 <- con_plot_1 + theme_minimal()

con_plot_1 + con_plot_3 + plot_annotation(title = "Default theme on the left, theme_classic() on the right")

# The elements of a plot are divided into three broad types
# a) element_line() manipulates lines
# b) element_text() manipulates text
# c) element_rect() manipulates rectangles
# We use element_blank() to remove an element entirely

# For example
penguins %>% drop_na() %>%
  ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point(aes(colour = body_mass_g)) +
  labs(title = "My pretty plot") +
  scale_colour_viridis_c(option = "magma") +
  theme(legend.position = "bottom",
        axis.title.x = element_text(colour = "red", size = 14, hjust = 1),
        axis.title.y = element_blank(),
        axis.line.y = element_line(colour = "cornflowerblue", size = 4),
        axis.text.y = element_text(size = 20, angle = 45),
        panel.background = element_rect(colour = "green", fill = "yellow", size = 10),
        plot.title = element_text(family = "Times", face = "italic",  hjust = 0.5, size = 18))

# Moving the legend position
penguins %>%  drop_na() %>%
  ggplot(aes(x = flipper_length_mm)) +
  geom_histogram(aes(fill = species), position = "identity") +
  theme(legend.position = c(0.9,0.85),
        legend.background = element_blank())


# 7 Big Challenge ---------------------------------------------------------

View(modeltab)

# Using the information we have been told about the high burden countries
modeltab_filter <- modeltab %>% 
  filter(country %in% c("Burkina Faso", "Democratic Republic of the Congo", "Mali", "Mozambique", "Niger", "Nigeria", "Uganda", "United Republic of Tanzania"))

# Or, calculating the high burden countries
high_burden <- unique(modeltab$country[modeltab$year == 2020 & modeltab$deaths > 20000])
modeltab_filter <- subset(modeltab, country %in% high_burden)

modeltab_point <- modeltab_filter %>% 
  ggplot(aes(x = year, y = cases)) +
  geom_point(aes(colour = country)) +
  geom_line(aes(colour = country)) +
  ylab("Cases (Millions)") +
  xlab("") +
  theme(legend.position = "none",
        axis.text.x = element_blank())

plot_1 <- modeltab_point + scale_colour_viridis_d(option = "H")

modeltab_col <- modeltab_filter %>%
  ggplot(aes(x = year, y = deaths)) +
  geom_col(aes(fill = country)) +
  ylab("Deaths") +
  xlab("") +
  theme(legend.position = "bottom",
        legend.title=element_blank())

plot_2 <- modeltab_col + scale_fill_viridis_d(option = "H")

final_plot <- plot_1 / plot_2 + plot_annotation(title = "Malaria cases and deaths in high burden countries 2000-2021")


ggsave(plot = final_plot, 
       filename = "modeltab.png",
       width = 300, 
       height = 200,
       units = 'mm')
