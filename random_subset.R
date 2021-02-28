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
dat<- last.file(outputdir,"_oegm_all.rds")
sapply(dat, function(x) sum(is.na(x))/length(x))
table(dat$batch)

#--- read in already screened data ---#
screened.dat <- last.file(inputdir2,"_OESM Screening_Fall 2020.csv") %>% 
  rename(title=citation_title) %>% 
  mutate(across(c(title),list(z=~replace_na(.,"")))) %>%  # replace NAs with "" (for combining strings)
  mutate(across(c(title_z),~str_to_lower(.))) %>%   # lower case
  mutate(across(c(title_z),~str_replace_all(.,"\\W", "")))

#select data to draw subset
sub.dat <- dat %>% 
  filter(batch=="unique_90" & !titlenospace%in%screened.dat$title_z)


##Randomly select some - change number for different random amount
rand.id <- sample(nrow(sub.dat),2500,replace = F) # get indices for 500 x 5 rows
test <- as.data.frame(slice(sub.dat, rand.id[501:1000]))


rand1 <- as.bibliography(as.data.frame(slice(sub.dat, rand.id[1:500])))
rand2 <- as.bibliography(as.data.frame(slice(sub.dat, rand.id[501:1000])))
rand3 <- as.bibliography(as.data.frame( slice(sub.dat, rand.id[1001:1500])))
rand4 <- as.bibliography(as.data.frame( slice(sub.dat, rand.id[1501:2000])))
rand5 <- as.bibliography(as.data.frame( slice(sub.dat, rand.id[2001:2500])))


# Export as ris --> import into Mendeley --> export as ris --> import into Colandr
write_bibliography(rand1, filename=paste0(outputdir,"efficency_test_sets/eff_test_random1.ris"),format="ris")
write_bibliography(rand2, filename=paste0(outputdir,"efficency_test_sets/eff_test_random2.ris"),format="ris")
write_bibliography(rand3, filename=paste0(outputdir,"efficency_test_sets/eff_test_random3.ris"),format="ris")
write_bibliography(rand4, filename=paste0(outputdir,"efficency_test_sets/eff_test_random4.ris"),format="ris")
write_bibliography(rand5, filename=paste0(outputdir,"efficency_test_sets/eff_test_random5.ris"),format="ris")



##Read in file
# data <- ReadBib("10_OEGM.bib", check = FALSE)
# df.csv <- import(paste0(inputdir,"OEGM_90_clean.csv"))
# system.time(unique_90 <- bib2df::bib2df(paste0(inputdir,"oegm90_clean.bib")))
# system.time(excl_unique <- read_bibliography(paste0(inputdir2,"OEGM-Set3_10_excluded_unique.ris")))
# system.time(incl_unique <- read_bibliography(paste0(inputdir2,"OEGM-Set4_10_included_unique.ris")))
# names(unique_90)[1:36]
# 
# test <- flatten_chr(unique_90$AUTHOR[2])
# str_c(test, sep=", ", collapse="")
# paste0(flatten_chr(unique_90$AUTHOR[2]))
# unique_90.edit$test <- flatten_chr(unique_90$AUTHOR[1])
# head(test)
# unique_90.edit <- unique_90 %>%
#   mutate(label=NA,issue=NA,CODE=NA, accession=NA,batch="unique_90", author=str_c(flatten_chr(AUTHOR), sep=", ", collapse="")) %>% 
#   select(label=label,
#          type=TYPE,
#          title=TITLE,
#          abstract=ABSTRACT,
#          address=ADDRESS,
#          author=author,
#          doi=DOI,
#          issue=issue,
#          keywords=KEYWORDS,
#          language=LANGUAGE,
#          year=YEAR,
#          issn=ISSN,
#          pages=PAGES,
#          journal=JOURNAL,
#          url=URL,
#          volume=VOLUME,
#          CODE=CODE,
#          publisher=PUBLISHER,
#          accession=accession,
#          institution=INSTITUTION,
#          batch=batch)
# 
# incl_unique.edit <- incl_unique %>% 
#   mutate(batch="unique_10_include") %>% 
#   select(names(unique_90.edit))
# 
# excl_unique.edit <- excl_unique %>% 
#   mutate(batch="unique_10_exclude") %>% 
#   select(names(unique_90.edit))
# 
# comb.data <- unique_90.edit %>% 
#   bind_rows(incl_unique.edit,excl_unique.edit)
# 
# # Export as csv
# export(comb.data,paste0(outputdir,today.date,"_oegm_all.rds"))
# 
# ##Convert to dataframe
# df$id <- rownames(df)
##Remove random rows from rest of citations
# remaining <- anti_join(df,rand, by="id")
# 
# ##Convert format and write to an .RIS file
# rand <- select(rand,-id)
# remaining <- select(remaining,-id)
# 


##Import into colandr directly OR into citation manager etc...
