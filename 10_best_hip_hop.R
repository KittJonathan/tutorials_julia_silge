# PCA for Hip Hop songs
# Julia Silge
# https://www.youtube.com/watch?v=OvgzIx5mDNM&t=4s
# https://juliasilge.com/blog/best-hip-hop/

# Packages ----

library(tidyverse)
library(tidymodels)
tidymodels_prefer()

theme_set(theme_bw())

# Import data ----

rankings <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv"
  )

# Explore data ----

rankings |> 
  pull(title)

rankings |> 
  ggplot(aes(year, points, color = gender)) +
  geom_jitter(alpha = 0.7) +
  scale_y_log10()



