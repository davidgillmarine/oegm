##Randomly select citations
pacman::p_load(rio,rcrossref,bib2df,revtools,janitor,tidyverse)

workdir <- "R:/Gill/research/oegm/"
#workdir <- gsub("git","data",getwd())
inputdir <- paste0(workdir,"tables/raw/")
inputdir2 <- paste0(workdir,"tables/raw/overlap/")
outputdir <- paste0(workdir,"output/tables/")
plotdir <- paste0(workdir,"output/plots/")
today.date <- gsub("-","",Sys.Date())
list.files(inputdir)

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


# --- Update 
data <- last.file(outputdir,"_oegm_all.rds")

data.edit <- data %>% 
  mutate(across(c(title,abstract),list(z=~replace_na(.,"")))) %>%  # replace NAs with "" (for combining strings)
  mutate(across(c(title_z,abstract_z),~str_to_lower(.))) %>%   # lower case
  mutate(across(c(title_z,abstract_z),~str_replace_all(.,"\\W", ""))) %>% # remove anything that isn't a word character
  mutate(rec.id=str_c(str_sub(title_z,1,5),
                      year,
                      str_sub(abstract_z,1,5),
                      sep="_")) %>% 
  mutate(temp.id=paste(str_sub(title_z,1,16),
                       year,
                       sep="_")) 

head(data.edit$rec.id)  
nrow(data.edit %>% get_dupes(rec.id))
View(data.edit %>% get_dupes(rec.id))

full_include<-import(paste0(inputdir,"20191022_All Data.xlsx"), skip =2, .name_repair = "universal") %>% 
  filter(Full.text.screening.=="Accept") %>% 
  distinct(Article.ID, .keep_all = T) %>% 
  select(Article.ID,
         title=Title,
         year=Year.of.publication)  %>% 
  mutate(across(c(title),list(z=~replace_na(.,"")))) %>%  # replace NAs with "" (for combining strings)
  mutate(across(c(title_z),~str_to_lower(.))) %>%   # lower case
  mutate(across(c(title_z),~str_replace_all(.,"\\W", ""))) %>% # remove anything that isn't a word character
  mutate(temp.id=paste(str_sub(title_z,1,16),
                      year,
                      sep="_")) 

  test <- full_include %>% 
    anti_join(data.edit,by="title_z")


