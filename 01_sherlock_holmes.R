# Topic modeling of Sherlock Holmes stories
# Julia Silge
# https://juliasilge.com/blog/sherlock-holmes-stm/
# https://www.youtube.com/watch?v=evTuL-RcRpc&t=43s

# Packages ----

library(gutenbergr)
library(tidyverse)
library(tidytext)
library(stm)
library(quanteda)

# Data download and prep ----

sherlock_raw <- gutenberg_download(gutenberg_id = 1661)

titles <- sherlock_raw |> 
  slice(8:19) |> 
  select(text) |> 
  mutate(text = str_squish(text),
         text = str_to_upper(text)) |> 
  pull()

sherlock <- sherlock_raw |> 
  mutate(story = ifelse(text %in% titles, text, NA)) |> 
  fill(story) |> 
  drop_na(story) |> 
  filter(text != "") |> 
  mutate(story = factor(story, levels = titles))

tidy_sherlock <- sherlock |> 
  mutate(line = row_number()) |> 
  unnest_tokens(word, text) |> 
  anti_join(stop_words)

tidy_sherlock |> 
  count(word, sort = TRUE) # "holmes" is 3x more frequent than the next word

tidy_sherlock <- tidy_sherlock |> 
  filter(word != "holmes")

tidy_sherlock |> 
  count(word, sort = TRUE)

# Explore tf-idf ----

tidy_sherlock |> 
  count(story, word, sort = TRUE) |>
  bind_tf_idf(word, story, n) |> 
  slice_head(n = 10, by = story) |> 
  mutate(word = fct_reorder(word, tf_idf)) |> 
  ggplot(aes(x = word, y = tf_idf, fill = story)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~story, scales = "free") +
  coord_flip()

# Implement topic modeling ----

sherlock_dfm <- tidy_sherlock |> 
  count(story, word, sort = TRUE) |> 
  cast_dfm(story, word, n)

topic_model <- stm(sherlock_dfm, K = 6, init.type = "Spectral")

summary(topic_model)

td_beta <- tidy(topic_model)
td_beta

td_beta |> 
  slice_max(order_by = beta, n = 10, by = topic) |> 
  mutate(term = fct_reorder(term, beta)) |> 
  ggplot(aes(x = term, y = beta, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = titles)
td_gamma

ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~topic, ncol = 3)



