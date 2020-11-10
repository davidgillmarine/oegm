##Randomly select citations
pacman::p_load(rio,rcrossref,bib2df,revtools,tidyverse)

workdir <- "R:/Gill/research/oegm/"
#workdir <- gsub("git","data",getwd())
inputdir <- paste0(workdir,"tables/raw/")
inputdir2 <- paste0(workdir,"tables/raw/overlap/")
outputdir <- paste0(workdir,"output/tables")
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

##Convert to dataframe
df$id <- rownames(df)

##Randomly select some - change number for different random amount
rand <- df[sample(nrow(df), 5000),]

##Remove random rows from rest of citations
remaining <- anti_join(df,rand, by="id")

##Convert format and write to an .RIS file
rand <- select(rand,-id)
remaining <- select(remaining,-id)

randombib <- as.bibliography(as.data.frame(rand))
remainingbib <- as.bibliography(as.data.frame(remaining))

write_bibliography(randombib, filename=paste0(outputdir,"random.ris"),format="ris")
write_bibliography(remainingbib, filename=paste0(outputdir,"remaining.bib",format="bib")) ##make sure you write the remaining to a bibtex file as it is faster for R to read this in than an RIS don't ask me why, for other purposes you can also write to .ris like importing to endnote, just change the file extension and the format type in this command

##Import into colandr directly OR into citation manager etc...