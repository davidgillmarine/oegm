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

last.file <- function(dir.nam,nam){
  import(paste0(dir.nam,last(sort(grep(nam,list.files(dir.nam), value=T)))))
}


## Combine 10% and 90%
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


