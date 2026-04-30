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

# Import data ----

sf_trees <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv"
  )

# Explore data ----

sf_trees |> 
  count(legal_status, sort = TRUE)

sf_trees |> 
  count(species, sort = TRUE)

trees_df <- sf_trees |> 
  mutate(legal_status = case_when(legal_status == "DPW Maintained" ~ legal_status,
                                  .default = "Other"),
         plot_size = parse_number(plot_size)) |> 
  select(!address) |> 
  drop_na() |> 
  mutate(across(where(is.character), factor))

skimr::skim(trees_df)

trees_df |> 
  ggplot(aes(x = longitude, y = latitude, color = legal_status)) +
  geom_point(size = 0.5, alpha = 0.4) +
  labs(color = NULL)

trees_df |> 
  count(legal_status, caretaker) |> 
  add_count(caretaker, wt = n, name = "caretaker_count") |> 
  filter(caretaker_count > 50) |> 
  group_by(legal_status) |> 
  mutate(percent_legal = n / sum(n)) |> 
  ggplot(aes(percent_legal, caretaker, fill = legal_status)) +
  geom_col(position = "dodge")

# Build model ----

# Split data
set.seed(123)
trees_split <- initial_split(trees_df, strata = legal_status)
trees_split

trees_train <- training(trees_split)
trees_test <- testing(trees_split)
trees_train

# Set up recipe
trees_rec <- recipe(legal_status ~ ., data = trees_train) |> 
  update_role(tree_id, new_role = "ID") |> 
  step_other(species, caretaker, site_info, 
             threshold = 0.01)

trees_rec

trees_prep <- prep(trees_rec)

trees_prep

juiced <- juice(trees_prep)

juiced |> 
  count(species, sort = TRUE)

juiced |> 
  count(caretaker, sort = TRUE)

juiced |> 
  count(site_info, sort = TRUE)

trees_rec <- recipe(legal_status ~ ., data = trees_train) |> 
  update_role(tree_id, new_role = "ID") |> 
  step_other(species, caretaker, threshold = 0.01) |> 
  step_other(site_info, threshold = 0.005) |> 
  step_dummy(all_nominal(), -all_outcomes()) |> 
  step_date(date, features = c("year")) |> 
  step_rm(date) |> 
  themis::step_downsample(legal_status)

trees_rec

trees_prep <- prep(trees_rec)
juiced <- juice(trees_prep)

juiced

juiced |> 
  count(legal_status, sort = TRUE)

# Random Forest

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
  ) |>
  set_mode("classification") |> 
  set_engine("ranger")

tune_spec  

# Set up workflow
tune_wf <- workflow() |> 
  add_recipe(trees_rec) |> 
  add_model(tune_spec)

tune_wf

# Train hyperparameters ----

set.seed(234)
trees_folds <- vfold_cv(trees_train)

trees_folds

doParallel::registerDoParallel()
set.seed(345)
tune_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = 20
)

tune_res

tune_res |> 
  collect_metrics()

tune_res |> 
  select_best(metric = "accuracy")

tune_res |> 
  collect_metrics() |> 
  filter(.metric == "roc_auc") |> 
  select(mean, min_n, mtry) |> 
  pivot_longer(min_n:mtry, values_to = "value", names_to = "parameter") |> 
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x")

# Tune again
rf_grid <- grid_regular(
  mtry(range = c(10, 40)),
  min_n(range = c(2, 8)),
  levels = 5
)

set.seed(456)
regular_res <- tune_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = rf_grid
)




