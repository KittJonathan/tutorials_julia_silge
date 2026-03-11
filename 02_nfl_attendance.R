# Predictive modeling in R with tidymodels and NFL attendance
# Julia Silge
# https://juliasilge.com/blog/intro-tidymodels/
# https://www.youtube.com/watch?v=LPptRkGoYMg

# Packages ----

library(tidyverse)
library(tidymodels)

theme_set(theme_bw())

# Data ----

attendance <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv"
  )

standings <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv"
  )

attendance_joined <- attendance |> 
  left_join(standings,
            by = c("year", "team_name", "team")
  )

attendance_joined

# Explore data ----

attendance_joined |> 
  drop_na(weekly_attendance) |> 
  ggplot(aes(fct_reorder(team_name, weekly_attendance), weekly_attendance,
             fill = playoffs)) +
  geom_boxplot(outlier.alpha = 0.5) +
  coord_flip()

attendance_joined |> 
  distinct(team_name, year, margin_of_victory, playoffs) |> 
  ggplot(aes(margin_of_victory, fill = playoffs)) +
  geom_histogram(position = "identity", alpha = 0.7)

attendance_joined |> 
  mutate(week = factor(week)) |> 
  ggplot(aes(week, weekly_attendance, fill = week)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.4)

# Prep dataset for modeling ----

attendance_df <- attendance_joined |> 
  drop_na(weekly_attendance) |> 
  select(weekly_attendance, team_name, year, week,
         margin_of_victory, strength_of_schedule, playoffs)

attendance_df

# Train models ----

set.seed(1234)

attendance_split <- attendance_df |> 
  initial_split(strata = playoffs)

attendance_split

nfl_train <- training(attendance_split)
nfl_test <- testing(attendance_split)

# OLS linear regression

linear_spec <- linear_reg() |> 
  set_engine(engine = "lm")

linear_spec

lm_fit <- linear_spec |> 
  fit(weekly_attendance ~ ., 
      data = nfl_train)

lm_fit

tidy(lm_fit)

tidy(lm_fit) |> 
  arrange(desc(estimate))

# Random Forest

rf_spec <- rand_forest(mode = "regression") |> 
  set_engine("ranger")

rf_spec

rf_fit <- rf_spec |> 
  fit(weekly_attendance ~ ., 
      data = nfl_train)

rf_fit

# Evaluate models ----

results_train <- lm_fit |> 
  predict(new_data = nfl_train) |> 
  mutate(truth = nfl_train$weekly_attendance,
         model = "lm") |> 
  bind_rows(
    rf_fit |> 
      predict(new_data = nfl_train) |> 
      mutate(truth = nfl_train$weekly_attendance,
             model = "rf")
  )

results_train

results_test <- lm_fit |> 
  predict(new_data = nfl_test) |> 
  mutate(truth = nfl_test$weekly_attendance,
         model = "lm") |> 
  bind_rows(
    rf_fit |> 
      predict(new_data = nfl_test) |> 
      mutate(truth = nfl_test$weekly_attendance,
             model = "rf")
  )

results_train |> 
  group_by(model) |> 
  rmse(truth = truth, estimate = .pred)

results_test |> 
  group_by(model) |> 
  rmse(truth = truth, estimate = .pred)

results_test |> 
  mutate(train = "testing") |> 
  bind_rows(
    results_train |> 
      mutate(train = "training")
  ) |> 
  ggplot(aes(truth, .pred, color = model)) +
  geom_abline(linetype = "dashed", color = "gray80", linewidth = 1.5) +
  geom_point(alpha = 0.5) +
  facet_wrap(~train)

# Let's try again! ----

set.seed(1234)

nfl_folds <- vfold_cv(nfl_train, strata = playoffs)
nfl_folds

rf_res <- fit_resamples(
  rf_spec,
  weekly_attendance ~ .,
  nfl_folds,
  control = control_resamples(save_pred = TRUE)
)

rf_res |> 
  collect_metrics()

rf_res |> 
  unnest(.predictions) |> 
  ggplot(aes(weekly_attendance, .pred, color = id)) +
  geom_abline(linetype = "dashed", color = "gray80", linewidth = 1.5) +
  geom_point(alpha = 0.5)
  
