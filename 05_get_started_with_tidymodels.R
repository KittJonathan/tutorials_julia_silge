# Get started with tidymodels using vaccination rate data
# Julia Silge
# https://www.youtube.com/watch?v=E2Ld3QdXYZo

# Packages ----

library(tidyverse)
library(tidymodels)
library(skimr)
library(rstanarm)

options(mc.cores = parallel::detectCores())

# Explore Data ----

measles <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2020/2020-02-25/measles.csv")

measles_df <- measles |> 
  filter(mmr > 0) |> 
  mutate(mmr_threshold = case_when(mmr > 95 ~ "Above",
                                   .default = "Below"),
         across(where(is.character), factor)) |> 
  select(state, mmr_threshold)

measles_df

measles_df |> 
  count(state)

skim(measles_df)

measles_df |> 
  summarise(mmr = mean(mmr_threshold == "Above"),
            .by = state) |> 
  mutate(state = fct_reorder(state, mmr)) |> 
  ggplot(aes(state, mmr, fill = state)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip()

# Getting started with tidymodels ----

glm_fit <- logistic_reg() |> 
  set_engine("glm") |> 
  fit(mmr_threshold ~ state, data = measles_df)

glm_fit

tidy(glm_fit)

tidy(glm_fit) |> 
  filter(p.value < 0.05)

new_schools <- tibble(
  state = unique(measles_df$state))

mean_pred <- predict(glm_fit, 
                     new_data = new_schools,
                     type = "prob")

mean_pred

conf_int <- predict(glm_fit,
                    new_data = new_schools,
                    type = "conf_int")

conf_int

schools_result <- new_schools |> 
  bind_cols(mean_pred) |> 
  bind_cols(conf_int)

schools_result

schools_result |> 
  mutate(state = fct_reorder(state, .pred_Above)) |> 
  ggplot(aes(state, .pred_Above, fill = state)) +
  geom_col(show.legend = FALSE) +
  geom_errorbar(aes(ymin = .pred_lower_Above,
                    ymax = .pred_upper_Above),
                color = "gray30") +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip()

# Trying another model ----

prior_dist <- student_t(df = 2)

stan_fit <- logistic_reg() |> 
  set_engine("stan", 
             prior = prior_dist, 
             prior_intercept = prior_dist) |> 
  fit(mmr_threshold ~ state, data = measles_df)


bayes_pred <- predict(stan_fit, 
                      new_data = new_schools,
                      type = "prob")

bayes_int <- predict(stan_fit,
                     new_data = new_schools,
                     type = "conf_int")

bayes_result <- new_schools |> 
  bind_cols(bayes_pred) |> 
  bind_cols(bayes_int)

bayes_result

schools_result |> 
  mutate(model = "glm") |> 
  bind_rows(
    bayes_result |> 
      mutate(model = "stan")
  ) |> 
  mutate(state = fct_reorder(state, .pred_Above)) |> 
  ggplot(aes(state, .pred_Above, color = model)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = .pred_lower_Above,
                    ymax = .pred_upper_Above),
                linewidth = 1.5, alpha = 0.7) +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip()