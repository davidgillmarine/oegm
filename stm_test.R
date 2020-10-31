pacman::p_load(quanteda,stm,rio,tidytext,tidyverse)
library(tidytext)
library(tidyverse)
library(rio)
library(janeaustenr)
# code based on: https://juliasilge.com/blog/sherlock-holmes-stm/

textsample <- as.tibble(read.delim("C:/Users/david/Desktop/mpanews.txt"))
names(textsample) <- "txt"
head(textsample)

tidy_sherlock <- textsample %>% 
  mutate(story = row_number()) %>%
  unnest_tokens(word, txt) %>%
  anti_join(stop_words) %>%
  filter(!word%in%  c("covid","mpas","marine","19","we're","that's"))

tidy_sherlock %>%
  count(word, sort = TRUE)

sherlock_tf_idf <- tidy_sherlock %>%
  count(story, word, sort = TRUE) %>%
  bind_tf_idf(word, story, n) %>%
  arrange(-tf_idf) %>%
  group_by(story) %>%
  top_n(10) %>%
  filter(tf_idf>0.03) %>% 
  ungroup

sherlock_tf_idf %>%
  mutate(word = reorder_within(word, tf_idf, story)) %>%
  ggplot(aes(word, tf_idf, fill = story)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ story, scales = "free", ncol = 3) +
  scale_x_reordered() +
  coord_flip() +
  theme(strip.text=element_text(size=11)) +
  labs(x = NULL, y = "tf-idf",
       title = "Highest tf-idf words in Sherlock Holmes short stories",
       subtitle = "Individual stories focus on different characters and narrative elements")  


sherlock_dfm <- tidy_sherlock %>%
  count(story, word, sort = TRUE) %>%
  cast_dfm(story, word, n)

sherlock_sparse <- tidy_sherlock %>%
  count(story, word, sort = TRUE) %>%
  cast_sparse(story, word, n)

topic_model <- stm(sherlock_dfm, K = 6, 
                   verbose = FALSE, init.type = "Spectral")

td_beta <- tidy(topic_model)

td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")

