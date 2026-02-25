# Get started with tidymodels and #TidyTuesday Palmer penguins
# Julia Silge
# https://juliasilge.com/blog/palmer-penguins/
# https://www.youtube.com/watch?v=z57i2GVcdww

# Packages ----

library(palmerpenguins)
library(tidyverse)
library(tidymodels)
tidymodels_prefer()

# Explore the dataset ----

data("penguins", package = "palmerpenguins")

penguins |> 
  count(species)

penguins |> 
  count(island)

penguins |> 
  count(island, species)

penguins |> 
  count(sex)

penguins |> 
  count(year)

penguins |> 
  filter(!is.na(sex)) |> 
  ggplot(aes(flipper_length_mm, bill_length_mm,
             colour = sex, size = body_mass_g)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ species)

penguins_df <- penguins |> 
  filter(!is.na(sex)) |> 
  select(-year, -island)

# Build a model ----

set.seed(123)

penguins_split <- initial_split(penguins_df, strata = sex)
penguins_split

penguins_train <- training(penguins_split)
penguins_test <- testing(penguins_split)

penguins_train
penguins_test

set.seed(234)
penguin_boot <- bootstraps(penguins_train)
penguin_boot

# Logistic regression

glm_spec <- logistic_reg() |> 
  set_engine("glm")

glm_spec

# Random forest

rf_spec <- rand_forest() |> 
  set_mode("classification") |> 
  set_engine("ranger")

rf_spec

penguin_wf <- workflow() |> 
  add_formula(sex ~ .)

penguin_wf

glm_rs <- penguin_wf |> 
  add_model(glm_spec) |> 
  fit_resamples(
    resamples = penguin_boot,
    control = control_resamples(save_pred = TRUE, verbose = TRUE)
  )

glm_rs

rf_rs <- penguin_wf |> 
  add_model(rf_spec) |> 
  fit_resamples(
    resamples = penguin_boot,
    control = control_resamples(save_pred = TRUE, verbose = TRUE)
  )

rf_rs

# Evaluate modeling ----

collect_metrics(rf_rs)

collect_metrics(glm_rs)

glm_rs |> 
  conf_mat_resampled()

glm_rs |> 
  collect_predictions() |> 
  group_by(id) |> 
  roc_curve(sex, .pred_female) |> 
  autoplot()

glm_rs |> 
  collect_predictions() |> 
  group_by(id) |> 
  roc_curve(sex, .pred_female) |> 
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
  coord_equal()

penguin_final <- penguin_wf |> 
  add_model(glm_spec) |> 
  last_fit(penguins_split)

penguin_final

collect_metrics(penguin_final)

collect_predictions(penguin_final) |> 
  conf_mat(sex, .pred_class)

penguin_final$.workflow[[1]] |> 
  tidy(exponentiate = TRUE) |> 
  arrange(estimate)
