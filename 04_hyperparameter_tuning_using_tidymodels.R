# Hyperparameter tuning using tidymodels
# Julia Silge
# https://juliasilge.com/blog/food-hyperparameter-tune/
# https://www.youtube.com/watch?v=muf3-hrahHs

# Packages ----

library(tidyverse)
library(tidymodels)
library(countrycode)
library(janitor)
library(GGally)

theme_set(theme_bw())

# Data ----

food_consumption <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv"
  )

# Explore data ----

# Model to predict if a country is in Asia based on the food consumption pattern.

food <- food_consumption |> 
  mutate(continent = countrycode(country,
                                 origin = "country.name",
                                 destination = "continent")) |> 
  select(!co2_emmission) |> 
  pivot_wider(
    names_from = food_category,
    values_from = consumption
  ) |> 
  clean_names() |> 
  mutate(
    asia = case_when(continent == "Asia" ~ "Asia",
                     .default = "Other"),
    .keep = "unused", .before = everything()
  ) |> 
  select(-country) |> 
  mutate(across(where(is.character), factor))

ggscatmat(food, columns = -1, color = "asia", alpha = 0.6)



# Build models with recipes ----
# Train models ----
# Evaluate models ----
# Visualise results ----