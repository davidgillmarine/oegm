pacman::p_load(quanteda,stm,revtools,rio,tidytext,ggthemes,knitr,tidyverse)
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

# read in data
finaldata <- last.file(outputdir,"_oegm_all.rds")
test<-finaldata[finaldata$batch%in%c("unique_10_include","unique_10_exclude"),]
table(test$batch)

# unnest words
tidy.data_unnest <- test %>%
  mutate(rec.id=row_number(),
         dataid=dataid,
         across(c(title,abstract),~replace_na(.,"")),
         comb.var=str_c(title,abstract), .keep = "used") %>%
  select(-c(title,abstract)) %>%
  unnest_tokens(word, comb.var)

# remove stop words
tidy.data <- tidy.data_unnest %>%
  anti_join(stop_words, by = "word")

# look at most popular words
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


# Identify unique words?
oegm_tf_idf <- tidy.data %>%
  count(dataid, word, sort = TRUE) %>%
  bind_tf_idf(word, dataid, n) %>%
  arrange(-tf_idf) %>%
  group_by(dataid) %>%
  top_n(15) %>%
  # filter(tf_idf>0.05) %>%
  ungroup

# Topic modelling
oegm_dfm <- tidy.data %>%
  count(dataid, word, sort = TRUE) %>%
  cast_dfm(dataid, word, n)
oegm_sparse <- tidy.data %>%
  count(dataid, word, sort = TRUE) %>%
  cast_sparse(dataid, word, n)

# Topic modelling function
my_topic_function <- function(.data,n.topic){

# run topic model
topic_model <- stm(.data, K = n.topic, 
                   verbose = FALSE, init.type = "Spectral")
td_beta <<- tidy(topic_model)
td_gamma<<-tidy(topic_model, matrix = "gamma",document_names = rownames(.data))
td_max_gamma<<-td_gamma %>% group_by(document) %>% slice(which.max(gamma)) # select paper with max gamma
names(td_max_gamma)[names(td_max_gamma) == "document"] <- "dataid"
td_max_gamma$dataid<-as.numeric(td_max_gamma$dataid)
newdata<<-merge(x= test, y = td_max_gamma, by = "dataid", all.x = TRUE) # join with original data
}

# Function to get top words
my_stm_topwords_function <- function(td_beta,td_gamma,n.terms){
  
  top_terms <- td_beta %>%
    arrange(beta) %>%
    group_by(topic) %>%
    top_n(n.terms, beta) %>%
    arrange(-beta) %>%
    select(topic, term) %>%
    summarise(terms = list(term)) %>%
    mutate(terms = map(terms, paste, collapse = ", ")) %>%
    unnest(cols = c(terms))
  
  gamma_terms <- td_gamma %>%
    group_by(topic) %>%
    summarise(gamma = mean(gamma)) %>%
    arrange(desc(gamma)) %>%
    left_join(top_terms, by = "topic") %>%
    mutate(topic = paste0("Topic ", topic),
           topic = reorder(topic, gamma))
  return(gamma_terms)
}

#--- example with 3 topics
# run topic model
my_topic_function(oegm_dfm,3)
# create objects from function outputs
td_beta.3 <- td_beta
td_gamma.3 <- td_gamma
td_max_gamma.3 <- td_max_gamma
newdata.3 <- newdata
# Run script to get top words
top.terms.3 <- my_stm_topwords_function(td_beta.3,td_gamma.3,10)
top.terms.3 %>%
  select(topic, gamma, terms) %>%
  kable(digits = 3,
        col.names = c("Topic", "Expected topic proportion", "Top 7 terms"))
# insert script that assesses include vs. exclude



#--- example with 4 topics
my_topic_function(oegm_dfm,4)
td_beta.4 <- td_beta
td_gamma.4 <- td_gamma
td_max_gamma.4 <- td_max_gamma
newdata.4 <- newdata
top.terms.3 <- my_stm_topwords_function(td_beta.3,td_gamma.3,10)
top.terms.3 %>%
  select(topic, gamma, terms) %>%
  kable(digits = 3,
        col.names = c("Topic", "Expected topic proportion", "Top 7 terms"))



# Get top unique words in each Topic
tmdata.data_unnest <- newdata %>% 
  select(c(title,abstract,keywords,topic)) %>% 
  mutate(rec.id=row_number(),
         across(c(title,abstract,keywords),~replace_na(.,"")),
         comb.var=str_c(title,abstract,keywords)) %>%
  unnest_tokens(word, comb.var) %>% 
  select(-c(title,abstract,keywords))
  
head(tmdata.data_unnest)

tm.data <- tmdata.data_unnest %>% 
  anti_join(stop_words, by = "word") %>% 
  filter(nchar(word)>5)

tm.data %>%
  count(word, sort = TRUE) %>%
  filter(n>1000)

# # remove "too popular" words
# top.pop.words <- tm.data %>%
#   count(word, sort = TRUE) %>% 
#   filter(n>1500) %>% 
#   pull(word)
# tm.data <- tm.data %>% 
#   filter(!word%in%top.pop.words)


oegm_tf_idf <- tm.data %>%
  count(topic, word, sort = TRUE) %>%
  bind_tf_idf(word, topic, n) %>%
  arrange(-tf_idf) %>%
  group_by(topic) %>%
  top_n(15) %>%
  # filter(tf_idf>0.05) %>% 
  ungroup


# top unique
oegm_tf_idf %>%
  mutate(word = reorder_within(word, tf_idf, topic)) %>%
  ggplot(aes(word, tf_idf, fill = topic)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 3) +
  scale_x_reordered() +
  coord_flip() +
  theme(strip.text=element_text(size=11)) +
  labs(x = NULL, y = "tf-idf",
       title = "Highest tf-idf words in the included and excluded groups")  

# top overall
top.tidy.data <- tidy.data %>%
  group_by(topic) %>% 
  mutate(num= n_distinct(word)) %>% 
  top_n(15) 
head(top.tidy.data)

top.tidy.data %>%
  ggplot(aes(word, num)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 3) +
  scale_x_reordered() +
  coord_flip() +
  theme(strip.text=element_text(size=11)) +
  labs(x = NULL, y = "tf-idf",
       title = "Highest tf-idf words in the included and excluded groups")  

ggsave(str_c(plotdir,today.date,"_exclude_vs_include_unique.png"), width=7, height = 9)

oegm_tf_idf %>%
  mutate(word = reorder_within(word, tf_idf, topic)) %>%
  ggplot(aes(word, tf_idf, fill = topic)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 3) +
  scale_x_reordered() +
  coord_flip() +
  theme(strip.text=element_text(size=11)) +
  labs(x = NULL, y = "tf-idf",
       title = "Highest tf-idf words in the included and excluded groups")  
ggsave(str_c(plotdir,today.date,"_exclude_vs_include_unique.png"), width=7, height = 9)



#the probabilities that each document is generated from each topic
td_gamma
gamma_plot<-ggplot(td_gamma,aes(gamma,fill=as.factor(topic)))+
  geom_histogram(show.legend=FALSE)+
  facet_wrap(~topic,ncol=2)
gamma_plot


td_gamma1<-td_gamma %>% group_by(document) %>% slice(which.max(gamma))
names(td_gamma1)[names(td_gamma1) == "document"] <- "dataid"
td_gamma1$dataid<-as.numeric(td_gamma1$dataid)
newdata<-merge(x= test, y = td_gamma1, by = "dataid", all.x = TRUE)
newdata
newdata%>%
  group_by(batch,topic)%>%summarise(gamma=mean(gamma),n=n())

newdata1<-newdata%>%
  group_by(batch,topic)%>%summarise(gamma=mean(gamma),n=n())%>%group_by(batch)%>%mutate(prop=n/sum(n))


topicplot8<-ggplot(newdata1, aes(fill=batch, y=prop, x=topic)) +
  geom_bar(position="stack", stat="identity")
topicplot8














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
