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
  mutate(across(c(title_z,abstract_z),~str_replace_all(.,"\\W", ""))) %>% # remove anything that isn't a word character
  # create a unique record id. (can be longer to avoid accidental deletions in de-duplication process)
  mutate(rec.id=str_c(str_sub(title_z,1,16),
                      year,
                      str_sub(abstract_z,1,25),
                      sep="_")) 

head(data.edit$rec.id)

#------  ID  accepted studies in main dataset ---------
# Goals: 1. would like an indicator variable (0,1) to identify the accepted studies from the u10_include records 
# 2. copy over the Article.ID into the main database
full_include<-import(paste0(inputdir,"20191022_All Data.xlsx"), skip =2, .name_repair = "universal") %>% 
  filter(Full.text.screening.=="Accept") %>%  # accepted articles
  distinct(Article.ID, .keep_all = T) %>%  # unique Article IDs
  select(Article.ID,
         title=Title,
         year=Year.of.publication,
         author=Author.s.) %>% 
  mutate(across(c(title),list(z=~replace_na(.,"")))) %>%  # replace NAs with "" (for combining strings)
  mutate(across(c(title_z),~str_to_lower(.))) %>%   # lower case
  mutate(across(c(title_z),~str_replace_all(.,"\\W", ""))) %>% # remove anything that isn't a word character
  mutate(temp.id=paste(str_sub(title_z,1,16),
                       year,
                       sep="_")) 

full_include %>% 
  summarise(across(everything(),n_distinct))

sapply(full_include, function(x) sum(is.na(x))/length(x)) # % NAs

# All accepted articles were in the 10% included batch (can do with full dataset but subsetting to reduce computing time)
u10.include <- data.edit %>% 
  filter(batch=="unique_10_include") %>% 
  select(rec.id,title,title_z,year) %>% 
  distinct(title_z,.keep_all = T)

# ---- Optional approach: Fuzzy string matching 
# Identify the best match in the main dataset using the "cleaned" titles to identify the rec.ids
test <- full_include %>% 
  #  distinct(Article.ID,.keep_all = T) %>% 
  select(Article.ID,title,title_z,year) %>% 
  stringdist_left_join(select(u10.include, rec.id,title,title_z,year),
                       by="title_z", ignore_case = T, max_dist = 10, distance_col="dist") %>% 
  #  filter(year.y==year.x) %>% 
  group_by(title.x) %>% 
  slice(which.min(dist)) %>% # record with lowest distance
  arrange(desc(dist)) %>% 
  select(title.x,title.y,year.x,year.y,everything())
#visually check to ensure that title.x is the same as title.y, if not, reduce max_dist or keep till next round
View(test)
matched.dat <- test  # ONLY include the ones that were an accurate match
found.ids <- unique(matched.dat$rec.id)
no.match <-filter(full_include,!(title%in%matched.dat$title.x))  # leftovers to try to match again below

# try again with larger max_dist
test2 <- no.match %>% 
  select(title,title_z,year) %>% 
  stringdist_left_join(select(u10.include, rec.id,title,title_z,year),
                       by="title_z", ignore_case = T, max_dist = 25, distance_col="dist") %>% 
  #  filter(year.y==year.x) %>% 
  group_by(title.x) %>% 
  slice(which.min(dist)) %>% 
  arrange(desc(dist)) %>% 
  select(title.x,title.y,year.x,year.y,everything())

#visually check to ensure that title.x is the same as title.y, if not, keep till next round or reduce max_dist 
View(test2)
matched.dat <- rbind(matched.dat,test2)  # ONLY the ones that were an accurate match
found.ids <- c(found.ids,unique(matched.dat$rec.id))
no.match2 <-filter(no.match,!(title%in%matched.dat$title.x))  # leftovers to try to match again below
# and so on.... when no.match gets small, might be faster to do it manually than to repeat this...

# when we find and confirm all matches, 
# 1. create variable in main dataframe to identify all studies that were full text screened (full.screen==1)
# 2. bring over the Article.ID into the main dataset (data.edit) to facilitate joining the main data & full_include at a later date
