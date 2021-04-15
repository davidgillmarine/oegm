pacman::p_load(rio,hrbrthemes,viridis,revtools,janitor,tidyverse)
workdir <- "R:/Gill/research/oegm/"
#workdir <- gsub("git","data",getwd())
inputdir <- paste0(workdir,"tables/raw/efficency_sets/")
outputdir <- paste0(workdir,"output/tables/")
today.date <- gsub("-","",Sys.Date())

list.files(inputdir)
screened.set <- import(paste0(inputdir,"OEGM Set 3c_screened.csv"))
names(screened.set) <- gsub("citation_","",names(screened.set))
table(screened.set$screening_status)
names(screened.set)



dat <-  screened.set %>% 
  select(screening_status) %>% 
  mutate(samp=1,
         tot.screened=seq(1,nrow(.),1),
         incl=as.integer(screening_status=="included")) %>% 
  select(-screening_status)
head(dat)

dat.out <- dat
for (i in 2:100){
dat.rand <- dat %>%
  mutate(samp=i,
         tot.screened=dat$tot.screened,
         incl=sample(dat$incl))
dat.out <- rbind(dat.out,dat.rand)
  }

dat.out <-  dat.out %>% 
  group_by(samp) %>% 
  mutate(tot.incl=cumsum(incl))
head(dat.out)

dat.out %>% 
  ungroup() %>% 
  group_by(samp,tot.screened) %>% 
  summarise(avg=mean(tot.incl))

ggplot(dat.out, aes(x=tot.screened,y=tot.incl)) +
  geom_point(stat="identity", alpha=0.05) +
  geom_smooth(method = loess, se=T) +
  theme_classic()

