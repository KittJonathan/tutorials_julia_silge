# Bootstrap resampling
# Julia Silge
# https://www.youtube.com/watch?v=7LGR1sEUXoI
# https://juliasilge.com/blog/beer-production/

# Packages ----

library(tidyverse)
library(tidymodels)
tidymodels_prefer()

theme_set(theme_bw())

# library(vip)

# Import data ----

brewing_materials_raw <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv"
  )

# Explore data ----

brewing_materials_raw

brewing_materials_raw |> 
  count(type)

brewing_materials_raw |> 
  count(type, wt = month_current, sort = TRUE)

brewing_filtered <- brewing_materials_raw |>
  filter(type %in% c("Malt and malt products", "Sugar and syrups",
                     "Hops (dry)"),
         year < 2016,
         !(month == 12 & year %in% 2014:2015)) |> 
  mutate(date = ymd(paste0(year, "-", month, "-01")))

brewing_filtered |> 
  ggplot(aes(date, month_current, color = type)) +
  geom_point()

brewing_materials <- brewing_filtered |> 
  select(date, type, month_current) |> 
  pivot_wider(names_from = type, values_from = month_current) |> 
  janitor::clean_names()

brewing_materials |> 
  ggplot(aes(malt_and_malt_products, sugar_and_syrups)) +
  geom_smooth(method = "lm") +
  geom_point()

beer_fit <- lm(sugar_and_syrups ~ 0 + malt_and_malt_products,
   data = brewing_materials)

summary(beer_fit)

tidy(beer_fit)

# Bootstrap resampling ----

set.seed(123)
beer_boot <- bootstraps(brewing_materials,
           times = 1e3, 
           apparent = TRUE)

beer_boot

beer_models <- beer_boot |> 
  mutate(model = map(splits, ~ lm(sugar_and_syrups ~ 0 + malt_and_malt_products,
                                  data = .)),
         coef_info = map(model, tidy))

beer_coefs <- beer_models |> 
  unnest(coef_info)

# Evaluate results ----

beer_coefs |> 
  ggplot(aes(estimate)) +
  geom_histogram(alpha = 0.7)

int_pctl(beer_models, coef_info)

beer_aug <- beer_models |> 
  sample_n(200) |> 
  mutate(augmented = map(model, augment)) |> 
  unnest(augmented)

beer_aug

beer_aug |> 
  ggplot(aes(malt_and_malt_products, sugar_and_syrups)) +
  geom_line(aes(y = .fitted, group = id),
            alpha = 0.2, color = "cyan3") +
  geom_point()

