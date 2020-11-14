##Randomly select citations
<<<<<<< HEAD
pacman::p_load(rio,rcrossref,bib2df,tidytext,revtools,fuzzyjoin,janitor,tidyverse)
=======
pacman::p_load(rio,rcrossref,bib2df,revtools,fuzzyjoin,janitor,tidyverse)
>>>>>>> a0e4a1f482acd2c1d9a96a60edaa702cc0868203

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

last.file <- function(dir.nam,nam){
  import(paste0(dir.nam,last(sort(grep(nam,list.files(dir.nam), value=T)))))
}


##Read in file
# data <- ReadBib("10_OEGM.bib", check = FALSE)
# df.csv <- import(paste0(inputdir,"OEGM_90_clean.csv"))
system.time(unique_90 <- bib2df::bib2df(paste0(inputdir,"oegm90_clean.bib")))
system.time(excl_unique <- read_bibliography(paste0(inputdir2,"OEGM-Set3_10_excluded_unique.ris")))
system.time(incl_unique <- read_bibliography(paste0(inputdir2,"OEGM-Set4_10_included_unique.ris")))
names(unique_90)[1:36]

# recode variables
unique_90.edit <- unique_90 %>%
  mutate(label=NA,issue=NA,CODE=NA, accession=NA,batch="unique_90", author=str_c(flatten_chr(AUTHOR), sep=", ", collapse="")) %>% 
  select(label=label,
         type=TYPE,
         title=TITLE,
         abstract=ABSTRACT,
         address=ADDRESS,
         author=author,
         doi=DOI,
         issue=issue,
         keywords=KEYWORDS,
         language=LANGUAGE,
         year=YEAR,
         issn=ISSN,
         pages=PAGES,
         journal=JOURNAL,
         url=URL,
         volume=VOLUME,
         CODE=CODE,
         publisher=PUBLISHER,
         accession=accession,
         institution=INSTITUTION,
         batch=batch)

incl_unique.edit <- incl_unique %>% 
  mutate(batch="unique_10_include") %>% 
  select(names(unique_90.edit))

excl_unique.edit <- excl_unique %>% 
  mutate(batch="unique_10_exclude") %>% 
  select(names(unique_90.edit))

comb.data <- unique_90.edit %>% 
  bind_rows(incl_unique.edit,excl_unique.edit)

# Export as csv
export(comb.data,paste0(outputdir,today.date,"_oegm_all.rds"))
export(comb.data,paste0(outputdir,today.date,"_oegm_all.csv"))

#--- Update ---#
data <- last.file(outputdir,"_oegm_all.rds")
sapply(data, function(x) sum(is.na(x))/length(x))


data.edit <- data %>% 
  mutate(across(c(title,abstract),list(z=~replace_na(.,"")))) %>%  # replace NAs with "" (for combining strings)
  mutate(across(c(title_z,abstract_z),~str_to_lower(.))) %>%   # lower case
  mutate(across(c(title_z,abstract_z),~str_replace_all(.,"\\W", ""))) %>% # remove anything that isn't a word character
  mutate(rec.id=str_c(str_sub(title_z,1,10),
                      year,
                      str_sub(abstract_z,1,15),
                      sep="_")) %>% 
  mutate(temp.id=paste(str_sub(title_z,1,16),
                       year,
                       sep="_")) 

head(data.edit$rec.id)
# Check for duplicates
excl.test <- filter(data.edit,batch=="unique_10_exclude") %>%  select(rec.id,title, abstract, year,batch) 
View(excl.test %>% get_dupes(rec.id))

u90.test <- filter(data.edit,batch=="unique_90")%>%  select(rec.id,title, abstract, year,batch) 
View(u90.test %>% get_dupes(rec.id))

test <- excl.test %>% 
  inner_join(u90.test,by="rec.id")

excl.dupl <- data.edit %>% 
  select(rec.id,title, abstract, year,batch) %>% 
  filter(batch%in%c("unique_90","unique_10_exclude")) %>% 
  get_dupes(rec.id)
  
export(excl.dupl,paste0(outputdir,today.date,"_10exclude_unique90_duplicates.csv"))


#  ID full screen data
data.include <- data.edit %>% 
  filter(batch=="unique_10_include") %>% 
  select(title,year,journal,author) %>% 
  mutate(across(c(title),~str_replace_all(.,"\\W", "")))  # remove anything that isn't a word character

full_screen<-import(paste0(inputdir,"OESM_included.csv")) %>% 
  select(title=citation_title,
         abstract=citation_abstract,
         author=citation_authors,
         journal=citation_journal_name,
         year=citation_pub_year)  %>% 
  mutate(across(c(title),list(z=~replace_na(.,"")))) %>%  # replace NAs with "" (for combining strings)
  mutate(across(c(title_z),~str_to_lower(.))) %>%   # lower case
  mutate(across(c(title_z),~str_replace_all(.,"\\W", ""))) %>% # remove anything that isn't a word character
  mutate(temp.id=paste(str_sub(title_z,1,16),
                       year,
                       sep="_")) 

full_include<-import(paste0(inputdir,"20191022_All Data.xlsx"), skip =2, .name_repair = "universal") %>% 
  filter(Full.text.screening.=="Accept") %>% 
  distinct(Article.ID, .keep_all = T) %>% 
  select(Article.ID,
         title=Title,
         year=Year.of.publication,
         author=Author.s.) %>% 
  mutate(across(c(title),~str_replace_all(.,"\\W", "")))  # remove anything that isn't a word character
  
  
full_include %>% 
  summarise(across(everything(),n_distinct))


test <- full_include %>% 
  stringdist_left_join(data.include, by = c("title" = "title"), max_dist = 9, distance_col = "distance") %>% 
  mutate(one_correct = title.x == title.y)
test %>% 
  summarise(across(everything(),n_distinct))
found.id <-  test$Article.ID[!is.na(test$title.y)]
test1 <- full_include %>% 
  filter(!Article.ID%in%found.id) %>% 
  stringdist_left_join(data.include, by = c("title" = "title"), max_dist = 20, distance_col = "distance") %>% 
  mutate(one_correct = title.x == title.y)
test1 %>% 
  summarise(across(everything(),n_distinct))
found.id <-  c(found.id,test1$Article.ID[!is.na(test1$title.y)])
test2 <- full_include %>% 
  filter(!Article.ID%in%found.id & title!="NA") %>% 
  stringdist_left_join(data.include, by = c("title" = "title"), max_dist = 30, distance_col = "distance") %>% 
  mutate(one_correct = title.x == title.y)
test2 %>% 
  summarise(across(everything(),n_distinct))
found.id <-  c(found.id,test2$Article.ID[!is.na(test2$title.y)])

 
  mutate(across(c(title),list(z=~replace_na(.,"")))) %>%  # replace NAs with "" (for combining strings)
  mutate(across(c(title_z),~str_to_lower(.))) %>%   # lower case
  mutate(across(c(title_z),~str_replace_all(.,"\\W", ""))) %>% # remove anything that isn't a word character
  mutate(temp.id=paste(str_sub(title_z,1,20),
                      year,
                      sep="_")) 

  test <- full_screen %>% 
  #  distinct(Article.ID,.keep_all = T) %>% 
    select(title,title_z,year) %>% 
   # left_join(select(full_screen,title,title_z,year),by="title_z")
    stringdist_left_join(select(incl.test,title,title_z,year),
                         by="title_z", ignore_case = T, max_dist = 100, distance_col="dist") %>% 
    #  filter(year.y==year.x) %>% 
    group_by(title.x) %>% 
    slice(which.min(dist)) %>% 
    arrange(desc(dist))
no.match <-filter(full_screen,!(title%in%test$title.x))  # should be 0

incl.test <- data.edit %>% 
  filter(batch=="unique_10_include") %>% 
  select(title,title_z,year) %>% 
  distinct(title_z,.keep_all = T)

test <- full_include %>% 
  distinct(Article.ID,.keep_all = T) %>% 
  select(title,title_z,year) %>% 
  # left_join(select(full_screen,title,title_z,year),by="title_z")
  stringdist_left_join(incl.test,
                       by="title_z", ignore_case = T, max_dist = 27, distance_col="dist") %>% 
  #  filter(year.y==year.x) %>% 
  group_by(title.x) %>% 
  slice(which.min(dist)) %>% 
  arrange(desc(dist))
no.match <-filter(full_include,!(title%in%test$title.x))  # should be 0

test2 <- no.match %>% 
  distinct(Article.ID,.keep_all = T) %>% 
  select(title,title_z,year) %>% 
  # left_join(select(full_screen,title,title_z,year),by="title_z")
  stringdist_left_join(incl.test,
                       by="title_z", ignore_case = T, max_dist = 100, distance_col="dist") %>% 
  #  filter(year.y==year.x) %>% 
  group_by(title.x) %>% 
  slice(which.min(dist)) %>% 
  arrange(desc(dist))
no.match <-filter(full_include,!(title%in%test$title.x))


# gorgonian paper a bad match, actually in larger sample
no.match$temp.id2 <- NA
no.match$title[1]
agrep("guilds", full_screen$title,ignore.case = T,value = T,max.distance = 0.1)
no.match$temp.id2[1] <- full_screen$temp.id[agrep("guilds", full_screen$title,ignore.case = T,value = F,max.distance = 0.1)]

no.match$title[2]
agrep("gorgonian", full_screen$title,ignore.case = T,value = T,max.distance = 0.1)
no.match$temp.id2[2] <- full_screen$temp.id[agrep("gorgonian", full_screen$title,ignore.case = T,value = F,max.distance = 0.1)]

no.match$title[3]
agrep("biomass gains", full_screen$title,ignore.case = T,value = T,max.distance = 0.1)
no.match$temp.id2[3] <- full_screen$temp.id[agrep("biomass gains", full_screen$title,ignore.case = T,value = F,max.distance = 0.1)]
  
  # no.match <-filter(test,is.na(title.y)) 
  # 
  # test2 <- full_include %>% 
  #   stringdist_left_join(select(full_screen,title,title_z,year),
  #                        by="title_z", ignore_case = T, max_dist = 30, distance_col="dist") %>% 
  # #  filter(year.y==year.x) %>% 
  #   group_by(title.x) %>% 
  #   slice(which.min(dist))
  # no.match <-filter(test2,is.na(title.y)) 
  # no.match <-filter(test,!(title%in%test2$title.x))  
  
#   
#   
# test2 <- full_include %>% 
#     select(title,year) %>% 
#     stringdist_left_join(select(full_screen,title,year),by="title", ignore_case = T, max_dist = 0.9, distance_col="dist")
# 
# no.match <-filter(test2,is.na(title.y))  
# 
# test3 <-full_include %>% 
# #  rename(title=title.x) %>% 
#   mutate(title=gsub("ORIGINAL PAPER","",title),
#          temp.id=paste(str_sub(title,15,30))) %>% 
#   stringdist_left_join(select(full_screen,title,title_z,year)%>% 
#                          mutate(temp.id=paste(str_sub(title_z,1,30))) %>% 
#                          filter(title!="No Title"),
#                                 by="title_z", ignore_case = T, max_dist = 30, distance_col="dist") %>% 
#   filter(year.y==year.x) %>% 
#   group_by(title.x) %>% 
#   slice(which.min(dist))
# 
# View(test3 %>% arrange(desc(dist)))
# no.match <-filter(test3,is.na(title.y))  
# no.match <-filter(full_include,!(title%in%test3$title.x))  
# test4 <- no.match %>% 
#   mutate(title=gsub("ORIGINAL PAPER","",title),
#          temp.id=paste(str_sub(title_z,15,25))) %>% 
#   stringdist_left_join(select(full_screen,title,title_z,year)%>% 
#                          mutate(temp.id=paste(str_sub(title_z,15,40))) %>% 
#                          filter(title!="No Title"),
#                        by=c("title"), ignore_case = T, max_dist = 0.7, distance_col="dist") %>% 
#  # filter(year.y==year.x) %>% 
#   group_by(title.x) %>% 
#   slice(which.min(dist))
# View(test4 %>% arrange(desc(dist)))
# View(test3 %>% get_dupes(title.x))
# 
# no.match <-filter(full_include,!(title%in%c(test3$title.x,test4$title.x)))  
# test5 <- no.match %>% 
#   mutate(title=gsub("ORIGINAL PAPER","",title),
#          temp.id=paste(str_sub(title_z,15,25))) %>% 
#   stringdist_left_join(select(full_screen,title,title_z,year)%>% 
#                          mutate(temp.id=paste(str_sub(title_z,15,40))) %>% 
#                          filter(title!="No Title"),
#                        by=c("title_z"), ignore_case = T, max_dist = 0.1, distance_col="dist") %>% 
#   # filter(year.y==year.x) %>% 
#   group_by(title.x) %>% 
#   slice(which.min(dist))
# View(test5 %>% arrange(desc(dist)))
# 
# no.match <-filter(full_include,!(title%in%c(test3$title.x,test4$title.x,test5$title.x)))  

no.match$temp.id2 <- NA
no.match$title[1]
agrep("guilds", full_screen$title,ignore.case = T,value = T,max.distance = 0.1)
no.match$temp.id2[1] <- full_screen$temp.id[agrep("guilds", full_screen$title,ignore.case = T,value = F,max.distance = 0.1)]

no.match$title[2]
agrep("gorgonian", full_screen$title,ignore.case = T,value = T,max.distance = 0.1)
no.match$temp.id2[2] <- full_screen$temp.id[agrep("gorgonian", full_screen$title,ignore.case = T,value = F,max.distance = 0.1)]

no.match$title[3]
agrep("biomass gains", full_screen$title,ignore.case = T,value = T,max.distance = 0.1)
no.match$temp.id2[3] <- full_screen$temp.id[agrep("biomass gains", full_screen$title,ignore.case = T,value = F,max.distance = 0.1)]

no.match$title[4]
agrep("southwest Madagascar", full_screen$title,ignore.case = T,value = T,max.distance = 0.1)
no.match$temp.id2[4] <- full_screen$temp.id[agrep("southwest Madagascar", full_screen$title,ignore.case = T,value = F,max.distance = 0.1)]

no.match$title[5]
agrep("MARINE NATIONAL RESERVE", full_screen$title,ignore.case = T,value = T,max.distance = 0.1)
no.match$temp.id2[5] <- full_screen$temp.id[agrep("MARINE NATIONAL RESERVE", full_screen$title,ignore.case = T,value = F,max.distance = 0.1)]

no.match$title[6]
agrep("Petroleum industry's", full_screen$title,ignore.case = T,value = T,max.distance = 0.1)
no.match$temp.id2[6] <- full_screen$temp.id[agrep("Petroleum industry's", full_screen$title,ignore.case = T,value = F,max.distance = 0.1)]

no.match$title[7]
agrep("Anegada", full_screen$title,ignore.case = T,value = T,max.distance = 0.1)
no.match$temp.id2[7] <- full_screen$temp.id[agrep("Anegada", full_screen$title,ignore.case = T,value = F,max.distance = 0.1)]

no.match$title[8]
agrep("north of Yucatan", full_screen$title,ignore.case = T,value = T,max.distance = 0.1)
no.match$temp.id2[8] <- full_screen$temp.id[agrep("north of Yucatan", full_screen$title,ignore.case = T,value = F,max.distance = 0.1)]

no.match$temp.id2[1:8]
