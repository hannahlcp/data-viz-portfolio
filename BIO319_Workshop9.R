# BIO319 Workshop 9 Script
# Hannah Chaudhry-Phipps
# 20/03/2023


# 1 Introduction ggplot2 --------------------------------------------------


install.packages("palmerpenguins")
library(palmerpenguins)
install.packages("tidyverse")
library(tidyverse)
install.packages('dplyr')
library('dplyr')


# 2 The Grammar of Graphics -----------------------------------------------


# Geometric objects which display data are called geoms
# Each geom has features, which are called aesthetics
# Features/aesthetics are the position of the geom along the axes, size, colour
# To visualise data, we map variables of our data to these aesthetics

# Tell ggplot which data we are looking at, which geom we would like to use to display the data,
#  and which variables we would like to map to which aesthetics

ggplot(data = penguins) +
  geom_point(mapping = aes(x = bill_length_mm, y = body_mass_g))
# We are looking at the penguins data
# We are using geom_point to display the data
# The aesthetics are variables taken from the data

# Let us map species to the colour of geom_point, and shape to the island variable
ggplot(data = penguins) +
  geom_point(mapping = aes(x = bill_length_mm, 
                           y = body_mass_g, 
                           colour = species,
                           shape = island))


# Now we are plotting additional geoms (geom_point and geom_smooth)
ggplot(data = penguins) +
  geom_point(mapping = aes(x = bill_length_mm, y = body_mass_g)) +
  geom_smooth(mapping = aes(x = bill_length_mm, y = body_mass_g))

# We do not need to repeat the mapping of variables if we are using the same variables
# in different layers/additional geoms
# Instead, we can pass the variables to ggplot(), which means they will be inherited by 
# the geoms which follow
ggplot(data = penguins, mapping = aes(x = bill_length_mm, y = body_mass_g)) +
  geom_point() +
  geom_smooth()

# Map the species to colour for both geoms
ggplot(data = penguins, mapping = aes(x = bill_length_mm, y = body_mass_g)) +
  geom_point(mapping = aes(colour = species)) +
  geom_smooth(mapping = aes(colour = species))

# The plot can be assigned to a variable, and other layers can be added later
pengu_plot <-
  ggplot(data = penguins,
         mapping = aes(x = bill_length_mm, y = body_mass_g)) +
  geom_point(aes(colour = species))

# Using geom_smooth, plot a linear model with no confidence intervals
pengu_plot_linear <- ggplot(data = penguins,
       mapping = aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(colour = species)) +
  geom_smooth(aes(colour = species), method = lm, se = FALSE)


# 3 Saving Plots ----------------------------------------------------------


# Can save plots to a file using 'ggsave' by providing a plot variable:
ggsave(plot = pengu_plot, filename = "penguin_plot_1.png")
# Can switch arguments round, and do not have to specify argument:
ggsave("penguin_plot_1.png", pengu_plot)
# OR ggsave will save the last plot we printed to screen

# Saving our plot with linear model lines
ggsave(plot = pengu_plot_linear, 
       filename = "penguin_plot_2.png", 
       width = 300, 
       height = 200,
       units = 'mm')


# 4 Continuous versus Categorical Variables -------------------------------


# So far we have used continuous data
# We can use geom_boxplot for categorical data
ggplot(data = penguins,
       mapping = aes(x = species, y = body_mass_g)) +
  geom_boxplot(mapping = aes(colour = species))
# Colour argument changes the colour of the lines on the boxplot

# If we want to fill the plots, use fill argument
ggplot(data = penguins,
       mapping = aes(x = species, y = body_mass_g)) +
  geom_boxplot(mapping = aes(fill = species))

# By default, categorical data is displayed in alphanumerical order
# Factors come in handy when determining the order in which we display 
# our data

df_days <-
  data.frame(day = c("Mon", "Tues", "Wed", "Thu"),
             counts = c(3, 8, 10, 5))
df_days$day <- as.factor(df_days$day)
str(df_days)

# So the below plot maps the days alphanumerically
ggplot(data = df_days, mapping = aes(x = day, y = counts)) +
  geom_col()

# We can override this by specifying the levels in the factor function
df_days$day <- factor(df_days$day, levels = c("Mon", "Tues", "Wed", "Thu"))
str(df_days)

# Now the below plot maps the days as we specified
ggplot(data = df_days, mapping = aes(x = day, y = counts)) +
  geom_col()


# Reproduce the plot
# First specify the order of the species categories
penguins$species <- factor(penguins$species, levels = c('Chinstrap', 'Gentoo', 'Adelie'))

# Now use geom_violin in ggplot2
ggplot(data = penguins, mapping = aes(x = species, y = body_mass_g)) +
  geom_violin(mapping = aes(fill = island))


# 5 Statistical Transformations -------------------------------------------


# Calculations of counts, means, etc., can be calculated by the geom

ggplot(data = penguins) +
  geom_bar(mapping = aes(x = species)) +
  coord_flip()


# The height of geom_col bars represent values in the data
# The height of geom_bar bars represent the count/number of cases in each group

# Using geom_histogram
penguins$species <- factor(penguins$species, levels = c('Adelie', 'Chinstrap', 'Gentoo'))

# The following histogram is stacked                           
ggplot(data = penguins) +
  geom_histogram(mapping = aes(x = flipper_length_mm, fill = species), alpha = 0.5)

# The following histogram is overlapped
ggplot(data = penguins) +
  geom_histogram(mapping = aes(x = flipper_length_mm, fill = species), position = 'identity', alpha = 0.5)


# 6 Plotting only a Subset of your Data: Filter () ------------------------


# For example, we only want to look at two species
penguins %>% filter(!species == "Chinstrap") %>%
  ggplot(mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(mapping = aes(colour = species, shape = island))
# Note the difference between %>% and +
# We do not need to tell ggplot the data, as we have piped the dataset into 
# ggplot()

# We can also use the filter() and is.na() to get rid of NAs

# The following plot contains the NA values
penguins %>% ggplot(mapping = aes(x = species, y = body_mass_g)) +
  geom_violin(mapping = aes(fill = sex))

# The below plot does NOT contain the NA values
pengu_plot_sex <- penguins %>% filter(!is.na(penguins$sex)) %>%
  ggplot(mapping = aes(x = species, y = body_mass_g)) +
  geom_violin(mapping = aes(fill = sex))


# 7 Labels ----------------------------------------------------------------


# To manipulate or add labels, we use labs()

penguins %>%
  ggplot(mapping = aes(x = species, y = body_mass_g)) +
  geom_violin(aes(fill = sex)) +
  labs(title = "Weight distribution among penguins",
       subtitle = "Plot generated by E. Busch-Nentwich, March 2023",
       x = "Species",
       y = "Weight in g",
       caption = "Data from Palmer Penguins package\nhttps://allisonhorst.github.io/palmerpenguins/"
  )

# To change the legend title and and labels, we cannot use labs() because
# the legend is part of scales
# The function we need to use depends on the aesthetics and variables
# In this example, we have mapped a categorical variable (sex) to the fill argument
# Thus, the function we need to use is scale_fill_discrete

penguins %>%
  ggplot(mapping = aes(x = species, y = body_mass_g)) +
  geom_violin(aes(fill = sex)) +
  labs(title = "Weight distribution among penguins",
       subtitle = "Plot generated by E. Busch-Nentwich, March 2023",
       x = "Species",
       y = "Weight in g",
       caption = "Data from Palmer Penguins package\nhttps://allisonhorst.github.io/palmerpenguins/"
  ) +
  scale_fill_discrete(name = "Sex",
                      labels = c("Female", "Male", "Unknown"),
                      type = c("yellow3", "magenta4", "grey"))
  
  
# 8 The Big Challenge -----------------------------------------------------


WMR <- read.table('wmr_modelling.txt', header = T)

# Order death column
WMR_decreasing <- WMR[order(WMR$deaths, decreasing = T), ]

# Filter rows which have 2020 only
WMR_2020 <- filter(WMR_decreasing, year == '2020')

# Make the country column the factor class
WMR_2020$country <- factor(WMR_2020$country)

# Order the levels of the factor according to descending order (not alphanumerical)
WMR_2020$year <- reorder(WMR_2020$year, WMR_2020$deaths)

# Plot the data
WMR_plot <- WMR_2020 %>%
  mutate(country = fct_reorder(country, deaths)) %>%
  ggplot(mapping = aes(x = country, y = deaths)) +
  geom_bar(stat = 'identity') +
 coord_flip()
