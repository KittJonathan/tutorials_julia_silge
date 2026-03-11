# Modeling hotel bookings in R using tidymodels and recipes
# Julia Silge
# https://juliasilge.com/blog/hotels-recipes/
# https://www.youtube.com/watch?v=dbXDkEEuvCU

# Packages ----

library(tidyverse)
library(tidymodels)
library(skimr)
library(GGally)

theme_set(theme_bw())

# Data ----

hotels <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv"
  )

# Explore data ----

hotels |> 
  count(is_canceled)

hotels |> 
  select(children, babies) |> 
  sample_n(10)

hotel_stays <- hotels |> 
  filter(is_canceled == 0) |> 
  mutate(children = case_when(children + babies > 0 ~ "children",
                              .default = "none"),
         required_car_parking_spaces = case_when(required_car_parking_spaces > 0 ~ "parking",
                                                 .default = "none")) |> 
  select(!is_canceled, !reservation_status, !babies)

hotel_stays |> 
  count(children)

skim(hotel_stays)

hotel_stays |> 
  mutate(arrival_date_month = factor(arrival_date_month,
                                     levels = month.name)) |> 
  count(hotel, arrival_date_month, children) |> 
  mutate(proportion = n / sum(n), .by = c(hotel, children)) |> 
  ggplot(aes(x = arrival_date_month, y = proportion, fill = children)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~hotel, nrow = 2)

hotel_stays |> 
  count(hotel, required_car_parking_spaces, children) |> 
  mutate(proportion = n / sum(n), .by = c(hotel, children)) |> 
  ggplot(aes(x = required_car_parking_spaces, y = proportion, fill = children)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~hotel, nrow = 2)

hotel_stays |> 
  select(children, adr, 
         required_car_parking_spaces, total_of_special_requests) |> 
  ggpairs(mapping = aes(color = children))

# Build models with recipes ----

hotels_df <- hotel_stays |> 
  select(children, hotel, arrival_date_month, meal, adr, adults,
         required_car_parking_spaces, total_of_special_requests,
         stays_in_week_nights, stays_in_weekend_nights) |> 
  mutate(across(where(is.character), factor))

hotels_df

set.seed(1234)
hotel_split <- initial_split(hotels_df)
hotel_split

hotel_train <- training(hotel_split)
hotel_test <- testing(hotel_split)

hotel_rec <- recipe(children ~ ., data = hotel_train) |> 
  themis::step_downsample(children) |> 
  step_dummy(all_nominal(), -all_outcomes()) |> 
  step_zv(all_numeric()) |> 
  step_normalize(all_numeric()) |> 
  prep()

hotel_rec

bake(hotel_rec, new_data = hotel_train)

bake(hotel_rec, new_data = hotel_train) |> 
  count(children)

juice(hotel_rec)

juice(hotel_rec) |> 
  count(children)

test_proc <- bake(hotel_rec, new_data = hotel_test)
test_proc

# Train models ----

knn_spec <- nearest_neighbor() |> 
  set_engine("kknn") |> 
  set_mode("classification")

knn_fit <- knn_spec |> 
  fit(children ~ .,
      data = juice(hotel_rec))

knn_fit

tree_spec <- decision_tree() |> 
  set_engine("rpart") |> 
  set_mode("classification")

tree_spec

tree_fit <- tree_spec |> 
  fit(children ~ .,
      data = juice(hotel_rec))

tree_fit

# Evaluate models ----

set.seed(1234)

validation_splits <- mc_cv(data = juice(hotel_rec), prop = 0.9, strata = children)

validation_splits

knn_res <- fit_resamples(
  knn_spec,
  children ~ .,
  validation_splits,
  control = control_resamples(save_pred = TRUE)
)

knn_res

knn_res |> 
  collect_metrics()

tree_res <- fit_resamples(
  tree_spec,
  children ~ .,
  validation_splits,
  control = control_resamples(save_pred = TRUE)
)

tree_res

tree_res |> 
  collect_metrics()

# Visualise results ----

# ROC curves

knn_res |> 
  unnest(.predictions) |> 
  mutate(model = "kknn") |> 
  bind_rows(tree_res |> 
              unnest(.predictions) |> 
              mutate(model = "rpart")) |> 
  group_by(model) |> 
  roc_curve(children, .pred_children) |> 
  autoplot()

# Confusion matrix

knn_res |> 
  unnest(.predictions) |> 
  conf_mat(truth = children, estimate = .pred_class) |> 
  autoplot(type = "heatmap")

# Check test data

knn_fit |> 
  predict(new_data = test_proc,
          type = "prob") |> 
  mutate(truth = hotel_test$children) |> 
  roc_auc(truth, .pred_children)





