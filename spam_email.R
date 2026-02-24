# Evaluate multiple modeling approaches for #TidyTuesday spam email
# Julia Silge
# https://juliasilge.com/blog/spam-email/
# https://www.youtube.com/watch?v=5LvTiy9dqrI

# Packages ----

library(tidyverse)
library(tidymodels)
library(discrim)
library(vip)
library(vetiver)
library(plumber)

# Import data ----

spam <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-15/spam.csv')

# Explore the data ----

head(spam)
glimpse(spam)

spam |> 
  count(yesno)

spam |> 
  ggplot(aes(crl.tot, fill = yesno)) +
  geom_density(alpha = 0.5) +
  scale_x_log10()

spam |> 
  pivot_longer(dollar:make) |> 
  mutate(value = if_else(value > 0, "Greater than zero", "Zero"),
         yesno = if_else(yesno == "n", "Not spam", "Spam")) |> 
  ggplot(aes(value, fill = yesno)) +
  geom_bar() +
  facet_wrap(vars(name))

# Build models ----

set.seed(123)

spam_split <- spam |> 
  mutate(yesno = as.factor(yesno)) |> 
  initial_split(strata = yesno)

spam_train <- training(spam_split)
spam_test <- testing(spam_split)

set.seed(234)
spam_folds <- vfold_cv(spam_train, strata = yesno)
spam_folds

nb_spec <- naive_Bayes()
nb_spec_tune <- naive_Bayes(smoothness = tune())

mars_spec <- mars() |> set_mode("classification")
mars_spec_tune <- mars(num_terms = tune()) |> set_mode("classification")

rf_spec <- rand_forest(trees = 1e3) |> set_mode("classification")
rf_spec_tune <- rand_forest(
  trees = 1e3,
  mtry = tune(),
  min_n = tune()
  ) |> 
  set_mode("classification")

spam_models <- 
  workflow_set(
    preproc = list(formula = yesno ~ .),
    models = list(
      nb = nb_spec,
      mars = mars_spec,
      rf = rf_spec,
      nb_tune = nb_spec_tune,
      mars_tune = mars_spec_tune,
      rf_tune = rf_spec_tune
    )
  )

spam_models

doParallel::registerDoParallel()

set.seed(123)

spam_res <- 
  spam_models |> 
  workflow_map(
    resamples = spam_folds,
    metrics = metric_set(accuracy, sensitivity, specificity)
  )

autoplot(spam_res)

rank_results(spam_res, rank_metric = "accuracy")

# Fit final model ----

spam_wf <- workflow(
  yesno ~ .,
  rf_spec |> set_engine("ranger", importance = "impurity")
)

spam_fit <- last_fit(spam_wf, spam_split)
spam_fit

collect_predictions(spam_fit) |> 
  conf_mat(yesno, .pred_class)

collect_predictions(spam_fit) |> 
  roc_curve(yesno, .pred_n) |> 
  autoplot()

extract_workflow(spam_fit)

extract_workflow(spam_fit) |> 
  extract_fit_parsnip() |> 
  vip()

# Build a deployable model ----

v <- extract_workflow(spam_fit) |> 
  vetiver_model("spam-email-rf")

v

pr() |> 
  vetiver_api(v) |> 
  pr_run()




