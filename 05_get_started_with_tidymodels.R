# Get started with tidymodels using vaccination rate data
# Julia Silge
# https://www.youtube.com/watch?v=E2Ld3QdXYZo

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

# Tune hyperparameters ----

set.seed(1234)

food_boot <- bootstraps(data = food, times = 30)
food_boot

rf_spec <- rand_forest(mode = "classification", 
            mtry = tune(),
            trees = 1000,
            min_n = tune()) |> 
  set_engine("ranger")

rf_spec

rf_grid <- tune_grid(
  rf_spec,
  asia ~ ., 
  resamples = food_boot
)

rf_grid

rf_grid |> 
  collect_metrics()

rf_grid |> 
  show_best(metric = "roc_auc")
