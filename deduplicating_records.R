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

#--- read in data ---#
data <- last.file(outputdir,"_oegm_all.rds")
sapply(data, function(x) sum(is.na(x))/length(x))

# data comprise of 3 batches
unique(data$batch)

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

# Quick visual check for duplicates within each batch 
u10.excl.test <- filter(data.edit,batch=="unique_10_exclude") %>%  select(rec.id,title, abstract, year,batch) 
View(u10.excl.test %>% get_dupes(rec.id))

u90.test <- filter(data.edit,batch=="unique_90")%>%  select(rec.id,title, abstract, year,batch) 
View(u90.test %>% get_dupes(rec.id))

# technically this shouldn't return records if rec.ids were unique, so we definitely have a problem with duplicates
u10.excl.test %>% 
  inner_join(u90.test,by="rec.id") %>% 
  view()

# Create a file of duplicates to be inspected by Asha  (already done)
excl.dupl <- data.edit %>%
  select(rec.id,title, abstract, year,batch) %>%
  filter(batch%in%c("unique_90","unique_10_exclude")) %>%
  get_dupes(rec.id)
# file exported and checked for "true duplicates" by Asha
# export(excl.dupl,paste0(outputdir,today.date,"_10exclude_unique90_duplicates.csv"))

# Asha manually inspected the exported file above, identifying those that were not duplicates (not_duplicate field) in this file:
excl.dupl.checked <- import(paste0(inputdir,"20201105_10exclude_unique90_duplicates_checked.csv")) %>% 
  select(rec.id:not_duplicate)
#filter(excl.dupl.checked,rec.id=="11thinternationa_NA_theproceedingscontain522p") %>% view
leave.alone.id <- excl.dupl.checked %>% 
  filter(not_duplicate==1) %>% 
  pull(rec.id)
likely.dupl.id<- excl.dupl.checked %>% 
  filter(not_duplicate!=1) %>% 
  pull(rec.id)

data.edit <- data.edit %>% 
  mutate(likely.dupl=case_when(
    rec.id%in%likely.dupl.id ~ 1,
    rec.id%in%leave.alone.id ~ 0,
    TRUE ~ NA_real_
  ))
# excl.dupl.checked %>% 
#   arrange(rec.id,batch) %>% 
#   group_by(rec.id) %>% 
#   summarise_all(first) %>% 
#   view()

# De-duplication steps:
# 1. de-duplicate by title
# identify which duplicates will be deleted. del.order ensures that duplicates in 90%, then 10% excluded are deleted. Never 10% include
data.del.order <- data.edit %>% 
  select(rec.id,title, title_z,abstract, year,batch) %>%
  mutate(del.order=case_when(
    batch=="unique_90" ~ 1,
    batch=="unique_10_exclude" ~ 2,
    TRUE ~ 3
  )) %>% 
  arrange(rec.id,del.order) 

data.del.order %>% 
  get_dupes(rec.id) %>% 
  view()

# may take some time (~1min). De-duplicate by title
test <-  data.del.order %>% 
  group_by(title_z) %>% 
  summarise_all(last) # the last function will choose to keep the 10_include first (if available), then 10_exclude, etc.
#view(test %>% filter(rec.id=="2004estuarinefis_2016_chapter1fishcommunitydata"))  
paste(nrow(data.edit) - nrow(test),"duplicates removed") # these may/may not be safe to remove as titles are pretty unique, but not always


# 2. Additional de-duplication step
#  De-duplicate by rec.id based on what Asha checked
nrow(data.edit %>% filter(rec.id %in% leave.alone.id)) # how many were not duplicates

test2 <-  test %>% 
  filter(!rec.id %in% leave.alone.id) %>% 
  arrange(rec.id,del.order) %>% 
  group_by(rec.id) %>% 
  summarise_all(last) %>% 
  bind_rows(filter(test,rec.id %in% leave.alone.id))
paste(nrow(test) - nrow(test2),"duplicates removed") # should be safe to remove, but depends on how "unique" rec.id is. May need to check

# 3. Alternatively, can deduplicate within each dataset, then pull in titles or rec.ids that are not in the dataset (longer, & inefficient)

# u10.incl.de.dupl <- data.edit %>% 
#   filter(batch=="unique_10_include") %>% 
#   group_by(title_z) %>% 
#   summarise_all(last) %>% 
#   group_by(rec.id) %>% 
#   mutate(rec.id.count=n()) %>% # ID potential duplicates by rec.id
#   ungroup()
# paste(nrow(filter(data.edit,batch=="unique_10_include")) - nrow(u10.incl.de.dupl),"duplicates removed") # should be safe to remove, but depends on how "unique" rec.id is.
# filter(u10.incl.de.dupl,rec.id.count>1) %>% view() # are these duplicates?
# 
# u10.excl.de.dupl <- data.edit %>% 
#   filter(batch=="unique_10_exclude") %>% 
#   group_by(title_z) %>% 
#   summarise_all(last) %>% 
#   group_by(rec.id) %>% 
#   mutate(rec.id.count=n()) %>% # ID potential duplicates by rec.id
#   ungroup()
# paste(nrow(filter(data.edit,batch=="unique_10_exclude")) - nrow(u10.excl.de.dupl),"duplicates removed") # should be safe to remove, but depends on how "unique" rec.id is.
# filter(u10.excl.de.dupl,rec.id.count>1) %>%  view() # are these duplicates? (use likely.dupl variable to help)
# 
# u90.de.dupl <- data.edit %>% 
#   filter(batch=="unique_90") %>% 
#   group_by(title_z) %>% 
#   summarise_all(last) %>% 
#   group_by(rec.id) %>% 
#   mutate(rec.id.count=n()) %>% # ID potential duplicates by rec.id
#   ungroup()
# paste(nrow(filter(data.edit,batch=="unique_90")) - nrow(u90.de.dupl),"duplicates removed") # should be safe to remove, but depends on how "unique" rec.id is.
# filter(u90.de.dupl,rec.id.count>1) %>%  view() # are these duplicates? (use likely.dupl variable to help)
# 
# # then join the 3 with titles (and rec.id?) that are not in the the data
# comb.dat <- u10.incl.de.dupl %>% 
#   rbind(filter(u10.excl.de.dupl,!title_z%in%u10.incl.de.dupl$title_z))
# comb.dat <- comb.dat %>% 
#   rbind(filter(u90.de.dupl,!title_z%in%comb.dat$title_z))
# 
# comb.dat.de.dupl <- comb.dat %>% 
#   group_by(title_z) %>% 
#   summarise_all(last) %>% 
#   group_by(rec.id) %>% 
#   mutate(rec.id.count=n()) %>% # ID potential duplicates by rec.id
#   ungroup()
# paste(nrow(comb.dat) - nrow(comb.dat.de.dupl),"duplicates removed") # should be safe to remove, but depends on how "unique" rec.id is.
# filter(comb.dat.de.dupl,rec.id.count>1) %>%  view() # are these duplicates? (use likely.dupl variable to help)

# or use fuzzy string matching to find similar titles (likely overkill, will take really long). Example below
# pot.dupl.u10.excl <- u10.incl.de.dupl %>%
#   #  distinct(Article.ID,.keep_all = T) %>%
#   select(title,title_z,year) %>%
#   # left_join(select(full_screen,title,title_z,year),by="title_z")
#   stringdist_left_join(select(u10.excl.de.dupl,title,title_z,year),
#                        by="title_z", ignore_case = T, max_dist = 10, distance_col="dist") %>%
#   #  filter(year.y==year.x) %>%
#   group_by(title.x) %>%
#   slice(which.min(dist)) %>%
#   arrange(desc(dist))




#---- END
