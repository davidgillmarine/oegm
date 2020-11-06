##Randomly select citations
pacman::p_load(rio,rcrossref,bib2df,revtools,fuzzyjoin,janitor,tidyverse)

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


##Read in file
# data <- ReadBib("10_OEGM.bib", check = FALSE)
# df.csv <- import(paste0(inputdir,"OEGM_90_clean.csv"))
system.time(unique_90 <- bib2df::bib2df(paste0(inputdir,"oegm90_clean.bib")))
system.time(excl_unique <- read_bibliography(paste0(inputdir2,"OEGM-Set3_10_excluded_unique.ris")))
system.time(incl_unique <- read_bibliography(paste0(inputdir2,"OEGM-Set4_10_included_unique.ris")))
names(unique_90)[1:36]

test <- flatten_chr(unique_90$AUTHOR[2])
str_c(test, sep=", ", collapse="")
paste0(flatten_chr(unique_90$AUTHOR[2]))
unique_90.edit$test <- flatten_chr(unique_90$AUTHOR[1])
head(test)
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

#######################################################################
# --- Updating 
#######################################################################
data <- last.file(outputdir,"_oegm_all.rds")
table(data$batch)

data.edit <- data %>% 
  mutate(across(c(title,abstract),list(z=~replace_na(.,"")))) %>%  # replace NAs with "" (for combining strings)
  mutate(across(c(title_z,abstract_z),~str_to_lower(.))) %>%   # lower case
  mutate(across(c(title_z,abstract_z),~str_replace_all(.,"\\W", ""))) %>% # remove anything that isn't a word character
  mutate(rec.id=paste(str_sub(title_z,1,16),
                      year,
                      str_sub(abstract_z,1,25),
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
  mutate(temp.id=paste(str_sub(title_z,1,16),
                      year,
                      sep="_")) 

  test <- full_include %>% 
    anti_join(data.edit,by="title_z")


