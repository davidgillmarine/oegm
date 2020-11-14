pacman::p_load(quanteda,stm,revtools,rio,tidytext,tidyverse)
workdir <- "R:/Gill/research/oegm/"
#workdir <- gsub("git","data",getwd())
inputdir <- paste0(workdir,"tables/raw/overlap/")
outputdir <- paste0(workdir,"output/tables/")
plotdir <- paste0(workdir,"output/plots/")
today.date <- gsub("-","",Sys.Date())
last.file <- function(dir.nam,nam){
  import(paste0(dir.nam,last(sort(grep(nam,list.files(dir.nam), value=T)))))
}

# code based on: https://juliasilge.com/blog/sherlock-holmes-stm/

data <- last.file(outputdir,"_oegm_all.rds")
table(data$batch)
tidy.data_unnest <- data %>% 
  filter(batch%in%c("unique_10_include","unique_10_exclude")) %>% 
  mutate(rec.id=row_number(),
         batch.type=batch,
         across(c(title,abstract,keywords),~replace_na(.,"")),
         comb.var=str_c(title,abstract,keywords),.keep = "used") %>%
  select(-c(title,abstract,keywords)) %>% 
  unnest_tokens(word, comb.var) 
  

tidy.data <- tidy.data_unnest %>% 
  anti_join(stop_words, by = "word") 

tidy.data %>%
  count(word, sort = TRUE) %>% 
  filter(n>1000)

# remove "too popular" words
top.pop.words <- tidy.data %>%
  count(word, sort = TRUE) %>% 
  filter(n>1500) %>% 
  pull(word)
tidy.data <- tidy.data %>% 
  filter(!word%in%top.pop.words)


oegm_tf_idf <- tidy.data %>%
  count(batch.type, word, sort = TRUE) %>%
  bind_tf_idf(word, batch.type, n) %>%
  arrange(-tf_idf) %>%
  group_by(batch.type) %>%
  top_n(15) %>%
 # filter(tf_idf>0.05) %>% 
  ungroup
head(sherlock_tf_idf)

oegm_tf_idf %>%
  mutate(word = reorder_within(word, tf_idf, batch.type)) %>%
  ggplot(aes(word, tf_idf, fill = batch.type)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ batch.type, scales = "free", ncol = 3) +
  scale_x_reordered() +
  coord_flip() +
  theme(strip.text=element_text(size=11)) +
  labs(x = NULL, y = "tf-idf",
       title = "Highest tf-idf words in the included and excluded groups")  
ggsave(str_c(plotdir,today.date,"_exclude_vs_include_unique.png"), width=7, height = 9)


# Topic modelling
oegm_dfm <- tidy.data %>%
  count(batch.type, word, sort = TRUE) %>%
  cast_dfm(batch.type, word, n)

oegm_sparse <- tidy.data %>%
  count(batch.type, word, sort = TRUE) %>%
  cast_sparse(batch.type, word, n)

topic_model <- stm(oegm_dfm, K = 4, 
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
ggsave(str_c(plotdir,today.date,"_stm_4topics_10percent.png"), width=7, height = 9)
