# PLAN372 - Final Project
# Sarah Followill

# First, we need to load libraries
library(tidyverse)
library(dplyr)

# Next, we'll load our data for NYC Green Infrastructure
infra = read_csv("NYC_Infra.csv")


# Calculate Infrastructure Densities per NYC Borough

# Calculate count of infrastructure records per borough
borough_df <- data.frame(infra$borough)

# Create a frequency table to find count per borough
counts_borough_df <- table(borough_df$infra.borough)

# then assign the corresponding count to each borough
borough_df$count <- counts_borough_df[match(borough_df$infra.borough,  names(counts_borough_df))]

# Make table of count per borough with only one result for each borough
count_by_borough = group_by(borough_df, infra.borough) %>%
  summarize(count_per_borough = mean(count))

view(count_by_borough)



# Add a new column of average infrastructure asset area by borough
count_by_borough$avg_area = group_by(infra, borough) %>%
  summarize(avg_area = mean(`asset_area`))

# Rename Columns
names(count_by_borough)[names(count_by_borough) == "infra.borough"] <- "borough"
names(count_by_borough)[names(count_by_borough) == "avg_area$avg_area"] <- "avg_area"


# Add population columns
count_by_borough$pop_2020 = group_by(infra, borough) %>%
  summarize(pop_2020 = mean(`2020_pop`))

count_by_borough$pop_2040 = group_by(infra, borough) %>%
  summarize(pop_2040 = mean(`2040_pop`))


# Calculate 2020 density per borough, accounting for asset area
count_by_borough$density_2020 = (count_by_borough$avg_area$avg_area * count_by_borough$count_per_borough) / count_by_borough$pop_2020$pop_2020

# Calculate 2040 density per borough, accounting for asset area
count_by_borough$density_2040 = (count_by_borough$avg_area$avg_area * count_by_borough$count_per_borough) / count_by_borough$pop_2040$pop_2040

# Make new consolidated table
table = data.frame(borough = count_by_borough$borough, infra_count_per_borough = count_by_borough$count_per_borough, avg_infra_area = count_by_borough$avg_area$avg_area, pop_2020 = count_by_borough$pop_2020$pop_2020, pop_2040 = count_by_borough$pop_2040$pop_2040, density_2020 = count_by_borough$density_2020, density_2040 = count_by_borough$density_2040)




# Create an equation that will find the increase in green infrastructure that will need
# to be constructed between 2020 and 2040 in order to maintain the 2020 density
# but considering population increase. I will assume that average infrastructure area 
# remains constant

# Write function
# Help in function writing from https://swcarpentry.github.io/r-novice-inflammation/02-func-R/
compute_x <- function(density_2020, avg_infra_area, infra_count_per_borough, pop_2040) {
  x <- ((density_2020 * pop_2040) / avg_infra_area) - infra_count_per_borough
  return(x)
}

table <- table %>% 
  mutate(infra_chg = compute_x(density_2020, avg_infra_area, infra_count_per_borough, pop_2040))


# Make plot of needed infrastructure change by borough
ggplot(table, aes(x=borough, y=infra_chg)) +
  geom_col()


# Make population change column between 2020 and 2040
table <- table %>% 
  mutate(pop_chg = pop_2040 - pop_2020)

# Plot population change by borough from 2020-2040
ggplot(table, aes(x=borough, y=pop_chg)) +
  geom_col()

