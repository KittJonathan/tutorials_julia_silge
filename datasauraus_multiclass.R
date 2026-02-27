# Predicting class membership for the #TidyTuesday Datasauraus Dozen
# Julia Silge
# https://juliasilge.com/blog/datasaurus-multiclass/
# https://www.youtube.com/watch?v=QhAPA_X-ilA

# Packages ----

library(datasauRus)
library(tidyverse)
library(tidymodels)
tidymodels_prefer()

# Explore data ----

datasaurus_dozen

datasaurus_dozen |> 
  ggplot(aes(x, y, color = dataset)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~dataset, ncol = 5)

datasaurus_dozen |> 
  summarise(across(c(x, y), list(mean = mean, sd = sd)),
            x_y_cor = cor(x, y),
            .by = dataset)

datasaurus_dozen |> 
  count(dataset)

# Build a model ----

dino_folds <- datasaurus_dozen |> 
  mutate(dataset = factor(dataset)) |> 
  bootstraps()

dino_folds

rf_spec <- rand_forest(trees = 1000) |> 
  set_mode("classification") |> 
  set_engine("ranger")

dino_wf <- workflow() |> 
  add_model(rf_spec) |> 
  add_formula(dataset ~ x + y)

dino_wf

doParallel::registerDoParallel()
dino_rs <- fit_resamples(
  dino_wf,
  resamples = dino_folds,
  control = control_resamples(save_pred = TRUE)
)

dino_rs

# Evaluate model ----

collect_metrics(dino_rs)

dino_rs |> 
  collect_predictions() |> 
  group_by(id) |> 
  ppv(truth = dataset, estimate = .pred_class)

dino_rs |> 
  collect_predictions() |> 
  group_by(id) |> 
  roc_curve(truth = dataset, .pred_away:.pred_x_shape) |> 
  autoplot()

dino_rs |> 
  collect_predictions() |> 
  conf_mat(dataset, .pred_class)

dino_rs |> 
  collect_predictions() |> 
  conf_mat(dataset, .pred_class) |> 
  autoplot(type = "heatmap")

dino_rs |> 
  collect_predictions() |> 
  filter(.pred_class != dataset) |> 
  conf_mat(dataset, .pred_class) |> 
  autoplot(type = "heatmap")





