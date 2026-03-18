# Tuning Random Forest hyperparameters
# Julia Silge
# https://www.youtube.com/watch?v=ts5bRZ7pRKQ&t=2s
# https://juliasilge.com/blog/sf-trees-random-tuning/

# Packages ----

library(tidyverse)
library(tidymodels)
tidymodels_prefer()

theme_set(theme_bw())

# library(vip)

# Explore data ----

sf_trees <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv"
  )

sf_trees |> 
  count(legal_status, sort = TRUE)

trees_df <- sf_trees |> 
  mutate(legal_status = case_when(legal_status == "DPW Maintained" ~ legal_status,
                                  .default = "Other"),
         plot_size = parse_number(plot_size)) |> 
  select(!address) |> 
  drop_na() |> 
  mutate(across(where(is.character), factor))

sf_trees |> 
  count(species, sort = TRUE)

# Build model ----

# Train hyperparameters ----