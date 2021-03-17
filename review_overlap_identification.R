##Randomly select citations
pacman::p_load(rio,rcrossref,bib2df,tidytext,revtools,fuzzyjoin,janitor,tidyverse)

workdir <- "R:/Gill/research/oegm/"
#workdir <- gsub("git","data",getwd())
inputdir <- paste0(workdir,"tables/raw/")
inputdir2 <- paste0(workdir,"tables/raw/overlap/")
outputdir <- paste0(workdir,"output/tables/")
plotdir <- paste0(workdir,"output/plots/")
today.date <- gsub("-","",Sys.Date())
list.files(inputdir)
last.file <- function(dir.nam,nam){
  import(paste0(dir.nam,last(sort(grep(nam,list.files(dir.nam), value=T)))))
}


#--- read in master data ---#
data <- last.file(outputdir,"_oegm_all.rds")
sapply(data, function(x) sum(is.na(x))/length(x))

data.edit <- data %>% 
  mutate(across(c(title,abstract),list(z=~replace_na(.,"")))) %>%  # replace NAs with "" (for combining strings)
  mutate(across(c(title_z,abstract_z),~str_to_lower(.))) %>%   # lower case
  mutate(across(c(title_z,abstract_z),~str_replace_all(.,"\\W", ""))) # remove anything that isn't a word character
  
head(data.edit$title_z)
u90.dat <- filter(data.edit, batch=="unique_90") %>% 
  select(dataid,title_z,title,abstract,doi,journal,year)

# read in citation review table
review.dat<-import(paste0(inputdir,"overlap/OEGM Relevant Screening reviews.xlsx"),.name_repair = "universal") 
names(review.dat) <- tolower(names(review.dat))
head(review.dat)  
unique(review.dat$reference)

review.dat <- review.dat %>% 
  mutate(across(c(title),list(z=~replace_na(.,"")))) %>%  # replace NAs with "" (for combining strings)
  mutate(across(c(title_z),~str_to_lower(.))) %>%   # lower case
  mutate(across(c(title_z),~str_replace_all(.,"\\W", ""))) %>% # remove anything that isn't a word character
  rename(journal=journal.name)
head(review.dat$title_z)
  
# check against those review citations already screened
clndr.dat<-import(paste0(inputdir,"overlap/202010216_OESM Screening_Fall 2020.csv")) %>% 
  rename(title=citation_title) %>% 
  mutate(across(c(title),list(z=~replace_na(.,"")))) %>%  # replace NAs with "" (for combining strings)
  mutate(across(c(title_z),~str_to_lower(.))) %>%   # lower case
  mutate(across(c(title_z),~str_replace_all(.,"\\W", ""))) 
head(clndr.dat$title_z)  

review.dat.subset <- review.dat %>% 
  filter(!title_z%in%clndr.dat$title_z) %>% 
  distinct(title_z,.keep_all = T) %>% 
  select(-c(abstract,reference)) %>% 
  filter(!is.na(title))

# check overlap with 90%
start.time <- Sys.time()
  first.match <- review.dat %>% 
  stringdist_left_join(u90.dat,
                       by="title_z", ignore_case = T, max_dist = 25, distance_col="dist") %>% 
  #  filter(year.y==year.x) %>% 
  group_by(title.x) %>% 
  slice(which.min(dist)) %>% # record with lowest distance
  arrange(desc(dist)) %>% 
  mutate(year.y=as.integer(year.y),
         diff.yr=year.x-year.y) %>% 
  select(dataid,title.x,title.y,dist,year.x,year.y,diff.yr,journal.x,journal.y,doi.x,doi.y,everything())
Sys.time()-start.time
  
#visually check to ensure that title.x is the same as title.y, year.x and year.y, etc. 
View(first.match)

# get ones that match
good.match <- first.match %>% 
  filter(dist<=4 | dataid%in%c(16295,36402,87065,73777)) %>% 
  filter(!is.na(title.x))
View(good.match)

#spot check
View(filter(first.match,!dataid%in%good.match$dataid))

# identify matches in orginal dataset and export
ovrlp.dat <- review.dat %>% 
   filter(!is.na(title)) %>% 
   distinct(title_z,.keep_all = T) %>% 
  select(authors,title,year,abstract,journal,doi) %>% 
  filter(title%in%good.match$title.x)  

#  export data to ris to import into Mendeley  to populate missing fields
write_bibliography(ovrlp.dat, filename=paste0(outputdir,today.date,"review_overlap.ris"),format="ris")

