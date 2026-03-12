# Hyperparameter tuning using tidymodels
# Julia Silge
# https://juliasilge.com/blog/food-hyperparameter-tune/
# https://www.youtube.com/watch?v=muf3-hrahHs

# Packages ----

library(tidyverse)
library(tidymodels)
library(countrycode)
library(janitor)

theme_set(theme_bw())

# Data ----

food_consumption <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv"
  )

food_consumption |> 
  mutate(continent = countrycode(country,
                                 origin = "country.name",
                                 destination = "continent")) |> 
  select(!co2_emmission) |> 
  pivot_wider(
    names_from = food_category,
    values_from = consumption
  ) |> 
  clean_names()



# Explore data ----


# Build models with recipes ----
# Train models ----
# Evaluate models ----
# Visualise results ----