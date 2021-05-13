source("colandr_import.R")

pacman::p_load(rio,revtools,cowplot,janitor,tidyverse)
workdir <- "R:/Gill/research/oegm/"
#workdir <- gsub("git","data",getwd())
inputdir <- paste0(workdir,"tables/raw/efficency_sets/")
outputdir <- paste0(workdir,"output/tables/")
plotdir <- paste0(workdir,"output/plots/efficiency_tests")

today.date <- gsub("-","",Sys.Date())


# --- Download datasets and check for errors ----
set1a <- colandr_get(2700,"david.gill@duke.edu")  
table(set1a$data_source_id)
source.1 <- c(1240,1241)
test <- set1a %>% 
  filter(data_source_id%in%source.1) %>% 
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
source.2 <- c(1226,1227)
test <- set2a %>% 
  filter(data_source_id%in%source.2) %>% 
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
source.3 <- c(1245,1247)
test <- set3a %>% 
  filter(data_source_id%in%source.3) %>% 
  select(tags,citation_status,`date_screened_t&a`) %>% 
  mutate(screen.error = ifelse(tags=="{INCLUDED}" & citation_status=="excluded"|tags=="{}" & citation_status=="included",1,0))
table(test$tags,test$citation_status)
filter(test,screen.error==1)

set3b <- colandr_get(2707,"david.gill@duke.edu") # error in one!
table(set3b$data_source_id)
test <- set3b %>% 
  group_by(id,tags,citation_status) %>%            # group by id (double screened)
  summarise(`date_screened_t&a`=last(`date_screened_t&a`),.groups = "drop") %>% 
  select(tags,citation_status,`date_screened_t&a`) %>% 
  mutate(screen.error = ifelse(tags=="{INCLUDED}" & citation_status=="excluded"|tags=="{}" & citation_status=="included",1,0))
table(test$tags,test$citation_status)
filter(test,screen.error==1)


set3c <- colandr_get(2708,"david.gill@duke.edu")

set4a <- colandr_get(2709,"david.gill@duke.edu")
table(set4a$data_source_id)
source.4 <- c(1243,1244)
test <- set4a %>% 
  filter(data_source_id%in%source.4) %>% 
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

set5a <- colandr_get(2712,"david.gill@duke.edu")
table(set5a$data_source_id)
source.5 <- c(1282,1283)
test <- set5a %>%
  filter(data_source_id%in%source.5) %>%
  select(tags,citation_status,`date_screened_t&a`) %>%
  mutate(screen.error = ifelse(tags=="{INCLUDED}" & citation_status=="excluded"|tags=="{}" & citation_status=="included",1,0))
table(test$tags,test$citation_status)
filter(test,screen.error==1)

set5b <- colandr_get(2713,"david.gill@duke.edu")
test <- set5b %>%
  select(tags,citation_status,`date_screened_t&a`) %>%
  mutate(screen.error = ifelse(tags=="{INCLUDED}" & citation_status=="excluded"|tags=="{}" & citation_status=="included",1,0))
table(test$tags,test$citation_status)
filter(test,screen.error==1)

set5c <- colandr_get(2714,"david.gill@duke.edu")
#export(set4b,paste0(outputdir,"colandr_sample.csv"))


# ---- function to organize data
func_org.dat <- function(set.a,set.b,set.c,dat.source) {
  dat.a <<-  set.a %>% 
    filter(data_source_id%in%dat.source) %>% 
    arrange(`date_screened_t&a`) %>% 
    select(citation_status) %>% 
    mutate(samp="active learning_10pct",
           tot.screened=seq(1,nrow(.),1),
           incl=as.integer(citation_status=="included"),
           tot.incl=cumsum(incl)) %>% 
    select(-citation_status)

  dat.b <<-  set.b %>% 
    arrange(`date_screened_t&a`) %>% 
    select(citation_status) %>% 
    mutate(samp="active learning",
           tot.screened=seq(1,nrow(.),1),
           incl=as.integer(citation_status=="included"),
           tot.incl=cumsum(incl)) %>% 
    select(-citation_status)

  dat.c <<-  set.c %>%  
    group_by(id,citation_status) %>%            # group by id (double screened)
    summarise(`date_screened_t&a`=last(`date_screened_t&a`)) %>% 
    arrange(`date_screened_t&a`) %>% 
    ungroup() %>% 
    select(citation_status) %>% 
    mutate(samp='control',
           tot.screened=seq(1,nrow(.),1),
           incl=as.integer(citation_status=="included"),
           tot.incl=cumsum(incl)) %>% 
    select(-citation_status)  
  dat.out <<- rbind(dat.a,dat.b,dat.c)
  
  
  # ---- randomization ----
  # for loop to randomize inclusion
  dat.rand.org <- dat.c %>%  
    mutate(samp=1)
  dat.rand <- data.frame()
  for (i in 1:10){
    dat.rand1 <- dat.rand.org %>%
      mutate(samp=i,
             incl=sample(incl),
             tot.incl=cumsum(incl))
    dat.rand <- rbind(dat.rand,dat.rand1)
  }
  
  dat.rand.avg <<- dat.rand %>% 
    filter(incl==1) %>% 
    group_by(tot.incl) %>% 
    summarise(avg.incl=mean(tot.screened),se.incl=sd(tot.screened)/sqrt(n()), ci.lower=avg.incl-(1.96*se.incl),ci.upper=avg.incl+se.incl) %>% 
    mutate(samp="random")
  
  }
  


# --- Compile sets ----
# Set 1
func_org.dat(set1a,set1b,set1c,source.1)
dat1a <-dat.a
dat1b <-dat.b
dat1c <-dat.c
dat1 <-dat.out
dat1.rand <-dat.rand.avg
table(dat1$samp)

# --- Plot results ----


p.set1 <- ggplot(dat1.rand, aes(x=avg.incl,y=tot.incl,col=samp)) +
            geom_line() +
            geom_line(aes(x=ci.lower,y=tot.incl,col=samp), linetype = 2) +
            geom_line(aes(x=ci.upper,y=tot.incl,col=samp), linetype = 2) +
            # geom_smooth(aes(xmin = ci.lower, xmax = ci.upper),stat = "identity") + 
            theme_classic() +
            geom_line(data=dat1, aes(x=tot.screened,y=tot.incl,col=samp)) +
            labs(x="total screened", y="total included", title="Set 1")
  
p.set1.bar <- ggplot(filter(dat1,incl==1),aes(x=tot.incl,y=tot.screened,fill=samp)) +
                geom_bar(stat="identity",position = 'dodge') +
                theme_classic() +
  labs(x="total included", y="total screened", title="Set 1")

plot_grid(p.set1,p.set1.bar, nrow = 2)


# Set 2
func_org.dat(set2a,set2b,set2c,source.2)
dat2a <-dat.a
dat2b <-dat.b
dat2c <-dat.c
dat2 <-dat.out
dat2.rand <-dat.rand.avg
table(dat2$samp)

# --- Plot results ----

p.set2 <- ggplot(dat2.rand, aes(x=avg.incl,y=tot.incl,col=samp)) +
  geom_line() +
  geom_line(aes(x=ci.lower,y=tot.incl,col=samp), linetype = 2) +
  geom_line(aes(x=ci.upper,y=tot.incl,col=samp), linetype = 2) +
  # geom_smooth(aes(xmin = ci.lower, xmax = ci.upper),stat = "identity") + 
  theme_classic() +
  geom_line(data=dat2, aes(x=tot.screened,y=tot.incl,col=samp)) +
  labs(x="total screened", y="total included", title="Set 2")

p.set2.bar <- ggplot(filter(dat2,incl==1),aes(x=tot.incl,y=tot.screened,fill=samp)) +
  geom_bar(stat="identity",position = 'dodge') +
  theme_classic() +
  labs(x="total included", y="total screened", title="Set 2")

plot_grid(p.set2,p.set2.bar, nrow = 2)

# Set 3
set3b.1 <- set3b %>% 
  group_by(id,tags,citation_status) %>%            # group by id (double screened)
  summarise(`date_screened_t&a`=last(`date_screened_t&a`),.groups = "drop") 

func_org.dat(set3a,set3b.1,set3c,source.3)
dat3a <-dat.a
dat3b <-dat.b
dat3c <-dat.c
dat3 <-dat.out
dat3.rand <-dat.rand.avg
table(dat3$samp)

# --- Plot results ----

p.set3 <- ggplot(dat3.rand, aes(x=avg.incl,y=tot.incl,col=samp)) +
  geom_line() +
  geom_line(aes(x=ci.lower,y=tot.incl,col=samp), linetype = 2) +
  geom_line(aes(x=ci.upper,y=tot.incl,col=samp), linetype = 2) +
  # geom_smooth(aes(xmin = ci.lower, xmax = ci.upper),stat = "identity") + 
  theme_classic() +
  geom_line(data=dat3, aes(x=tot.screened,y=tot.incl,col=samp)) +
  labs(x="total screened", y="total included", title="Set 3")

p.set3.bar <- ggplot(filter(dat3,incl==1),aes(x=tot.incl,y=tot.screened,fill=samp)) +
  geom_bar(stat="identity",position = 'dodge') +
  theme_classic() +
  labs(x="total included", y="total screened", title="Set 3")

plot_grid(p.set3,p.set3.bar, nrow = 2)


# Set 4
func_org.dat(set4a,set4b,set4c,source.4)
dat4a <-dat.a
dat4b <-dat.b
dat4c <-dat.c
dat4 <-dat.out
dat4.rand <-dat.rand.avg
table(dat2$samp)

# --- Plot results ----

p.set4 <- ggplot(dat4.rand, aes(x=avg.incl,y=tot.incl,col=samp)) +
  geom_line() +
  geom_line(aes(x=ci.lower,y=tot.incl,col=samp), linetype = 2) +
  geom_line(aes(x=ci.upper,y=tot.incl,col=samp), linetype = 2) +
  # geom_smooth(aes(xmin = ci.lower, xmax = ci.upper),stat = "identity") + 
  theme_classic() +
  geom_line(data=dat4, aes(x=tot.screened,y=tot.incl,col=samp)) +
  labs(x="total screened", y="total included", title="Set 4")

p.set4.bar <- ggplot(filter(dat4,incl==1),aes(x=tot.incl,y=tot.screened,fill=samp)) +
  geom_bar(stat="identity",position = 'dodge') +
  theme_classic()  +
  labs(x="total included", y="total screened", title="Set 4")

plot_grid(p.set4,p.set4.bar, nrow = 2)


# Set 5
func_org.dat(set5a,set5b,set5c,source.5)
dat5a <-dat.a
dat5b <-dat.b
dat5c <-dat.c
dat5 <-dat.out
dat5.rand <-dat.rand.avg
table(dat2$samp)

# --- Plot results ----

p.set5 <- ggplot(dat5.rand, aes(x=avg.incl,y=tot.incl,col=samp)) +
  geom_line() +
  geom_line(aes(x=ci.lower,y=tot.incl,col=samp), linetype = 2) +
  geom_line(aes(x=ci.upper,y=tot.incl,col=samp), linetype = 2) +
  # geom_smooth(aes(xmin = ci.lower, xmax = ci.upper),stat = "identity") + 
  theme_classic() +
  geom_line(data=dat5, aes(x=tot.screened,y=tot.incl,col=samp)) +
  labs(x="total screened", y="total included", title="Set 5")

p.set5.bar <- ggplot(filter(dat5,incl==1),aes(x=tot.incl,y=tot.screened,fill=samp)) +
  geom_bar(stat="identity",position = 'dodge') +
  theme_classic()
plot_grid(p.set5,p.set5.bar, nrow = 2)

plot_grid(p.set1,p.set2,p.set3,p.set4,
          nrow = 2)
ggsave(paste0(plotdir,'_inclusion_rate.png'),width = 15,height = 10)

plot_grid(p.set1.bar + theme(legend.position = "none"),p.set2.bar + theme(legend.position = "none"),
          p.set3.bar  + theme(legend.position = "none") ,p.set4.bar+ theme(legend.position = "none"),
          nrow = 2)
ggsave(paste0(plotdir,'_inclusion_rate_bar.png'),width = 7,height = 5)



# ---- Compare backward citation to others -----
bwd.cit <- colandr_get(2120,"david.gill@duke.edu") # 
table(bwd.cit$citation_status)
bwd.cit <- filter(bwd.cit,citation_status!="conflict")

comb.incl <- bwd.cit %>% select(citation_status) %>% mutate(samp="backward citation") %>% 
    bind_rows(set1a %>% filter(!data_source_id%in%source.1) %>% select(citation_status) %>% mutate(samp="ten pct")) %>% 
    bind_rows(set1c %>% select(citation_status) %>% mutate(samp="set 1")) %>% 
    bind_rows(set2c %>% select(citation_status) %>% mutate(samp="set 2")) %>% 
    bind_rows(set3c %>% select(citation_status) %>% mutate(samp="set 3")) %>% 
    bind_rows(set4c %>% select(citation_status) %>% mutate(samp="set 4")) %>% 
    bind_rows(set5c %>% select(citation_status) %>% mutate(samp="set 5")) %>% 
  group_by(samp,citation_status) %>%
  summarise(num=n()) %>% 
  group_by(samp) %>% 
  mutate(samp.num=sum(num),pct=num/samp.num, pct.val=paste0(round(pct*100,1),"%"))
  

ggplot(comb.incl,aes(x=samp, y=pct, fill=citation_status, label=pct.val)) +
  geom_bar(position="stack",stat = "identity") +
  geom_label() +
  theme_classic()
ggsave(paste0(plotdir,'_inclusion_rate.png'),width = 8,height = 4)


# sample plots

# control only
ggplot() +
  geom_line(data=filter(dat2, samp=="control"), aes(x=tot.screened,y=tot.incl,col=samp)) +
  labs(x="total screened", y="total included", title="Set 2") +
  theme_classic()
ggsave(paste0(plotdir,'_control_test.png'),width = 8,height = 4)

# control + random
ggplot(dat2.rand, aes(x=avg.incl,y=tot.incl,col=samp)) +
  geom_line() +
  geom_line(aes(x=ci.lower,y=tot.incl,col=samp), linetype = 2) +
  geom_line(aes(x=ci.upper,y=tot.incl,col=samp), linetype = 2)+
  theme_classic() +
  geom_line(data=filter(dat2, samp=="control"), aes(x=tot.screened,y=tot.incl,col=samp)) +
  labs(x="total screened", y="total included", title="Set 2")
ggsave(paste0(plotdir,'_ctrl_random_test.png'),width = 8,height = 4)


# control + random + active learning
ggplot(data=dat2.rand, aes(x=avg.incl,y=tot.incl,col=samp)) +
  geom_line() +
  geom_line(aes(x=ci.lower,y=tot.incl,col=samp), linetype = 2) +
  geom_line(aes(x=ci.upper,y=tot.incl,col=samp), linetype = 2)+
  theme_classic() +
  geom_line(data=filter(dat2, samp!="active learning_10pct"), aes(x=tot.screened,y=tot.incl,col=samp)) +
  labs(x="total screened", y="total included", title="Set 2")
ggsave(paste0(plotdir,'_ctrl_random_active_only_test.png'),width = 8,height = 4)

# all
ggplot(dat2.rand, aes(x=avg.incl,y=tot.incl,col=samp)) +
  geom_line() +
  geom_line(aes(x=ci.lower,y=tot.incl,col=samp), linetype = 2) +
  geom_line(aes(x=ci.upper,y=tot.incl,col=samp), linetype = 2)+
  theme_classic() +
  geom_line(data=dat2, aes(x=tot.screened,y=tot.incl,col=samp)) +
  labs(x="total screened", y="total included", title="Set 2")
ggsave(paste0(plotdir,'_all_test.png'),width = 8,height = 4)


