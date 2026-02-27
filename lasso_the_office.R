# LASSO regression using tidymodels and #TidyTuesday data for The Office
# Julia Silge
# https://juliasilge.com/blog/lasso-the-office/
# https://www.youtube.com/watch?v=R32AsuKICAY

# Packages ----

library(tidyverse)
library(tidymodels)
tidymodels_prefer()

# Explore data ----

ratings_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv")


# Train a model ----

# Tune LASSO parameters ----