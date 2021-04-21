source("colandr_import.R")

pacman::p_load(rio,hrbrthemes,viridis,revtools,cowplot,janitor,tidyverse)
workdir <- "R:/Gill/research/oegm/"
#workdir <- gsub("git","data",getwd())
inputdir <- paste0(workdir,"tables/raw/efficency_sets/")
outputdir <- paste0(workdir,"output/tables/")
today.date <- gsub("-","",Sys.Date())


# --- Download datasets and check for errors ----
set1a <- colandr_get(2700,"david.gill@duke.edu") # 
table(set1a$data_source_id)
test <- set1a %>% 
  filter(data_source_id%in%c(1240,1241)) %>% 
  select(tags,citation_status,`date_screened_t&a`) %>% 
  mutate(screen.error = ifelse(tags=="{INCLUDED}" & citation_status=="excluded"|tags=="{}" & citation_status=="included",1,0))
table(test$tags,test$citation_status)
filter(test,screen.error==1)

set1b <- colandr_get(2701,"david.gill@duke.edu")
table(set1b$data_source_id)
test <- set1b %>% 
  select(tags,citation_status,`date_screened_t&a`) %>% 
  mutate(screen.error = ifelse(tags=="{INCLUDED}" & citation_status=="excluded"|tags=="{}" & citation_status=="included",1,0))
table(test$tags,test$citation_status)
filter(test,screen.error==1)

set1c <- colandr_get(2702,"david.gill@duke.edu")

set2a <- colandr_get(2703,"david.gill@duke.edu")
table(set2a$data_source_id)
test <- set2a %>% 
  filter(data_source_id%in%c(1226,1227)) %>% 
  select(tags,citation_status,`date_screened_t&a`) %>% 
  mutate(screen.error = ifelse(tags=="{INCLUDED}" & citation_status=="excluded"|tags=="{}" & citation_status=="included",1,0))
table(test$tags,test$citation_status)
filter(test,screen.error==1)


set2b <- colandr_get(2704,"david.gill@duke.edu") 
table(set2b$data_source_id)
test <- set2b %>% 
  select(tags,citation_status,`date_screened_t&a`) %>% 
  mutate(screen.error = ifelse(tags=="{INCLUDED}" & citation_status=="excluded"|tags=="{}" & citation_status=="included",1,0))
table(test$tags,test$citation_status)
filter(test,screen.error==1)

set2c <- colandr_get(2705,"david.gill@duke.edu")

set3a <- colandr_get(2706,"david.gill@duke.edu") # not finished (Colandr ate 19 citations)
table(set3a$data_source_id)
test <- set3a %>% 
  filter(data_source_id%in%c(1245,1247)) %>% 
  select(tags,citation_status,`date_screened_t&a`) %>% 
  mutate(screen.error = ifelse(tags=="{INCLUDED}" & citation_status=="excluded"|tags=="{}" & citation_status=="included",1,0))
table(test$tags,test$citation_status)
filter(test,screen.error==1)

set3b <- colandr_get(2707,"david.gill@duke.edu") # error in one!
table(set3b$data_source_id)
test <- set3b %>% 
  select(tags,citation_status,`date_screened_t&a`) %>% 
  mutate(screen.error = ifelse(tags=="{INCLUDED}" & citation_status=="excluded"|tags=="{}" & citation_status=="included",1,0))
table(test$tags,test$citation_status)
filter(test,screen.error==1)

set3c <- colandr_get(2708,"david.gill@duke.edu")

set4a <- colandr_get(2709,"david.gill@duke.edu")
table(set4a$data_source_id)
test <- set4a %>% 
  filter(data_source_id%in%c(1243,1244)) %>% 
  select(tags,citation_status,`date_screened_t&a`) %>% 
  mutate(screen.error = ifelse(tags=="{INCLUDED}" & citation_status=="excluded"|tags=="{}" & citation_status=="included",1,0))
table(test$tags,test$citation_status)
filter(test,screen.error==1)

set4b <- colandr_get(2710,"david.gill@duke.edu")
test <- set4b %>% 
  select(tags,citation_status,`date_screened_t&a`) %>% 
  mutate(screen.error = ifelse(tags=="{INCLUDED}" & citation_status=="excluded"|tags=="{}" & citation_status=="included",1,0))
table(test$tags,test$citation_status)
filter(test,screen.error==1)

set4c <- colandr_get(2711,"david.gill@duke.edu")

# set5a <- colandr_get(2712,"david.gill@duke.edu")
# table(set5a$data_source_id)
# test <- set5a %>% 
#   filter(data_source_id%in%c(1253,1244)) %>% 
#   select(tags,citation_status,`date_screened_t&a`) %>% 
#   mutate(screen.error = ifelse(tags=="{INCLUDED}" & citation_status=="excluded"|tags=="{}" & citation_status=="included",1,0))
# table(test$tags,test$citation_status)
# filter(test,screen.error==1)
# 
# set5b <- colandr_get(2713,"david.gill@duke.edu")
# test <- set5b %>% 
#   select(tags,citation_status,`date_screened_t&a`) %>% 
#   mutate(screen.error = ifelse(tags=="{INCLUDED}" & citation_status=="excluded"|tags=="{}" & citation_status=="included",1,0))
# table(test$tags,test$citation_status)
# filter(test,screen.error==1)
# 
# set5c <- colandr_get(2714,"david.gill@duke.edu")
export(set4b,paste0(outputdir,"colandr_sample.csv"))


# --- Compile sets ----
dat4a <-  set4a %>% 
  filter(data_source_id%in%c(1243,1244)) %>%  # select correct datasets
  arrange(`date_screened_t&a`) %>% 
  select(citation_status) %>% 
  mutate(samp="active learning_10pct",
         tot.screened=seq(1,nrow(.),1),
         incl=as.integer(citation_status=="included"),
         tot.incl=cumsum(incl)) %>% 
  select(-citation_status)
head(dat4a)

dat4b <-  set4b %>% 
  arrange(`date_screened_t&a`) %>% 
  select(citation_status) %>% 
  mutate(samp="active learning",
         tot.screened=seq(1,nrow(.),1),
         incl=as.integer(citation_status=="included"),
         tot.incl=cumsum(incl)) %>% 
  select(-citation_status)
head(dat4b)

dat4c <-  set4c %>%  
  arrange(`date_screened_t&a`) %>% 
  group_by(id,citation_status) %>%            # group by id
  summarise() %>% 
  ungroup() %>% 
  select(citation_status) %>% 
  mutate(samp='control',
         tot.screened=seq(1,nrow(.),1),
         incl=as.integer(citation_status=="included"),
         tot.incl=cumsum(incl)) %>% 
  select(-citation_status)

dat4 <- rbind(dat4a,dat4b,dat4c)
head(dat4)

# --- Plot results ----
dat4.first <- dat4 %>% 
  group_by(samp,tot.incl) %>% 
  summarise(tot.screened =first(tot.screened )) %>%
  filter(tot.incl>0)


ggplot(dat4, aes(x=tot.screened,y=tot.incl,col=samp)) +
  geom_line()  
  
plot_grid(
  ggplot(dat4, aes(x=tot.screened,y=tot.incl,col=samp)) +
    geom_line()+
    theme_classic(),
    ggplot(dat4.first,aes(x=tot.incl,y=tot.screened,fill=samp)) +
    geom_bar(stat="identity",position = 'dodge') +
    theme_classic(),
  nrow = 2)




# ---- randomization ----
# create dataframe with total screened and included dummy variable
dat <-  set4c %>% 
  group_by(id,citation_status) %>%
  summarise() %>% 
  ungroup() %>% 
  select(citation_status) %>% 
  mutate(samp=1,
         tot.screened=seq(1,nrow(.),1),
         incl=as.integer(citation_status=="included"),
         tot.incl=cumsum(incl)) %>% 
  select(-citation_status)
head(dat)
nrow(dat)==500

# for loop to randomize inclusion
dat.out <- dat
for (i in 2:10){
  dat.rand <- dat %>%
    mutate(samp=i,
           tot.screened=dat$tot.screened,
           incl=sample(dat$incl),
           tot.incl=cumsum(incl))
  dat.out <- rbind(dat.out,dat.rand)
}

test <- dat.out %>% 
  group_by(tot.screened) %>% 
  summarise(avg.incl=mean(tot.incl),se.incl=sd(tot.incl)/sqrt(n()), ci.lower=avg.incl-(1.96*se.incl),ci.upper=avg.incl+se.incl) %>% 
  mutate(samp="random")
test
ggplot(test, aes(x=tot.screened,y=avg.incl,col=samp)) +
  geom_line() +
  geom_smooth(aes(ymin = ci.lower, ymax = ci.upper),stat = "identity") + 
  theme_classic() +
  geom_line(data=dat4, aes(x=tot.screened,y=tot.incl,col=samp)) 


# ---- Compare backward citation to others -----
bwd.cit <- colandr_get(2120,"david.gill@duke.edu") # 
table(bwd.cit$citation_status)
bwd.cit <- filter(bwd.cit,citation_status!="conflict")

comb.incl <- bwd.cit %>% select(citation_status) %>% mutate(samp="backward citation") %>% 
  bind_rows(set1c %>% select(citation_status) %>% mutate(samp="set 1")) %>% 
    bind_rows(set2c %>% select(citation_status) %>% mutate(samp="set 2")) %>% 
    bind_rows(set3c %>% select(citation_status) %>% mutate(samp="set 3")) %>% 
    bind_rows(set4c %>% select(citation_status) %>% mutate(samp="set 4")) %>% 
  group_by(samp,citation_status) %>%
  summarise(num=n()) %>% 
  group_by(samp) %>% 
  mutate(samp.num=sum(num),pct=num/samp.num, pct.val=paste0(round(pct*100,1),"%"))
  

ggplot(comb.incl,aes(x=samp, y=pct, fill=citation_status, label=pct.val)) +
  geom_bar(position="stack",stat = "identity") +
  geom_label( )
  theme_classic()
