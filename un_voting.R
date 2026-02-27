# Dimensionality reduction of #TidyTuesday United Nations voting patterns
# Julia Silge
# https://juliasilge.com/blog/un-voting/
# https://www.youtube.com/watch?v=ZByO3D7faPs

# Packages ----

library(embed)
library(tidyverse)
library(tidymodels)
tidymodels_prefer()

# Explore data ----

unvotes <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv")
issues <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv")

unvotes
issues

unvotes_df <- unvotes |> 
  select(country, rcid, vote) |> 
  mutate(vote = factor(vote, levels = c("no", "abstain", "yes")),
         vote = as.numeric(vote),
         rcid = paste0("rcid_", rcid)) |> 
  pivot_wider(names_from = rcid, 
              values_from = vote,
              values_fill = 2)

glimpse(unvotes_df)

# Principal Component Analysis ----

pca_rec <- recipe(~ ., data = unvotes_df) |> 
  update_role(country, new_role = "id") |> 
  step_normalize(all_predictors()) |> 
  step_pca(all_predictors())

pca_rec

pca_prep <- prep(pca_rec)

pca_prep

bake(pca_prep, new_data = NULL)

bake(pca_prep, new_data = NULL) |> 
  ggplot(aes(PC1, PC2, label = country)) +
  geom_point(color = "midnightblue", alhpa = 0.7, size = 2) +
  geom_text(check_overlap = TRUE, hjust = "inward")

tidy(pca_prep, 1)

pca_comps <- tidy(pca_prep, 2) |> 
  filter(component %in% paste0("PC", 1:4)) |> 
  left_join(issues |> mutate(terms = paste0("rcid_", rcid))) |> 
  filter(!is.na(issue)) |> 
  group_by(component) |> 
  slice_max(abs(value), n = 8) |> 
  ungroup()

pca_comps |> 
  mutate(value = abs(value)) |> 
  ggplot(aes(value, terms, fill = issue)) +
  geom_col(position = "dodge") +
  facet_wrap(~ component, scales = "free_y") +
  labs(y = NULL, fill = NULL,
       x = "Absolute value of contribution")

# UMAP ----

umap_rec <- recipe(~ ., data = unvotes_df) |> 
  update_role(country, new_role = "id") |> 
  step_normalize(all_predictors()) |> 
  step_umap(all_predictors())

umap_rec

umap_prep <- prep(umap_rec)

umap_prep

bake(umap_prep, new_data = NULL)

bake(umap_prep, new_data = NULL) |> 
  ggplot(aes(UMAP1, UMAP2, label = country)) +
  geom_point(color = "midnightblue", alpha = 0.7, size = 2) +
  geom_text(check_overlap = TRUE, hjust = "inward")


