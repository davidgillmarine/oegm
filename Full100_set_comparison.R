# Script that pulls in titles from the original 133K citations (100%) in EndNote to identify those accidentally deleted
# during the de-duplication process
pacman::p_load(rio,hrbrthemes,viridis,revtools,janitor,tidyverse)

workdir <- "R:/Gill/research/oegm/"
#workdir <- gsub("git","data",getwd())
inputdir <- paste0(workdir,"tables/")
inputdir2 <- paste0(workdir,"tables/raw/overlap/")
outputdir <- paste0(workdir,"output/tables/")
plotdir <- paste0(workdir,"output/plots/")
today.date <- gsub("-","",Sys.Date())
list.files(inputdir)
last.file <- function(dir.nam,nam){
  import(paste0(dir.nam,last(sort(grep(nam,list.files(dir.nam), value=T)))))
}

# Read in titles and year from the original 100% file (133K title & year)
list.files(outputdir)
orig.dat <- import(paste0(outputdir,"OESM_Articles_Library_100_title_year.txt"),quote="",header=F, encoding='UTF-8')
names(orig.dat) <- c("year","title")
str(orig.dat)

# auth.test <- import(paste0(outputdir,"OESM_Articles_Library_100_author.txt"),sep="#",header=F) ## fail
# names(test) <- c("year","title")
# str(test)
# 
# abstract.test <- import(paste0(outputdir,"OESM_Articles_Library_100_abstract.txt"),fill=T,header=F)
# 
# doi.test <- import(paste0(outputdir,"OESM_Articles_Library_100_doi.txt"),fill=T,header=F)
# 
# export(orig.dat,paste0(outputdir,"_100_with_dupl_title_only.csv"))

#--- read in master data ---#
finaldata<- last.file(outputdir,"_oegm_all.rds")
#finaldata$title <- stringi::stri_enc_toutf8(finaldata$title) # re-mark encodings

# clean records
orig.dat$year <- gsub('"', '',orig.dat$year)
orig.dat$year <- as.integer(orig.dat$year)
str(orig.dat)
orig.dat$title <- gsub('"', '',orig.dat$title)
# removeq <- colwise(function(x) str_replace_all(x, '\"', ""))
# orig.dat <- removeq(orig.dat)
#Replace non-alphanumeric values with spaces for updated titles and abstracts, and save to new variables
orig.dat$titlealnum<-str_replace_all(orig.dat$title, "[^[:alnum:]]", " ")
#Change all titles into lower case and save to new variables
orig.dat$titlelower<-tolower(orig.dat$titlealnum)
#Remove all spaces from updated titles, and save to new variables
orig.dat$titlelower<-trimws(orig.dat$titlelower)
searchString <- ' '
replacementString <- ''
orig.dat$titlenospace <- gsub(searchString,replacementString,orig.dat$titlelower)


# what citations are in the 133K that are not already in the final data (exact match)?
unmatched<-anti_join(orig.dat,finaldata,by="titlenospace")
unmatched<-distinct(unmatched, titlenospace,.keep_all = T)

# Loop through final data for potential matches for unmatched
unmatched.long.title <- unmatched[nchar(unmatched$title)>30,] # fuzzy match poor with short titles

out.match <- data.frame()
loop.start <- seq(1,nrow(finaldata),1000)

for (i in loop.start){
  
 row.start <-  i
 row.end <- min(row.start+999,nrow(finaldata))
 
matched1 <- unmatched.long.title %>%
  #  distinct(Article.ID,.keep_all = T) %>%
  select(title,titlenospace,year) %>%
  fuzzyjoin::stringdist_left_join(select(finaldata[row.start:row.end,],title,titlenospace,year),
                       by="titlenospace", ignore_case = T, max_dist = 15, distance_col="dist") %>%
  #  filter(year.y==year.x) %>%
  group_by(title.x) %>%
  slice(which.min(dist)) %>% # choose record with lowest distance (differences)
  arrange(desc(dist)) %>%
  select(title.x,title.y,year.x,year.y,everything())

out.match <- rbind(out.match,matched1)
print(paste0("finished rows ", row.start, "-", row.end))
}

# prep for export & compare
out.match <- out.match %>% 
  group_by(titlenospace.x) %>%
  slice(which.min(dist)) %>% 
  mutate(dist.prop=dist/nchar(titlenospace.x),
         year.diff=year.x-year.y) %>% 
  arrange(-dist.prop) 

# export file to manually check matches
export(out.match,str_c(outputdir,"100_match_check.csv"))
  
# import file checked for matches
out.match.checked <- import(str_c(outputdir,"100_match_check_complete.xlsx"))

#filter out those that are not matches (i.e. no match=add to database)
out.match.checked.dupl <- filter(out.match.checked,is.na(not.duplicate))

# Remove citations from unmatched that were fuzzy matched, leaving behind those that were not matches
unmatched.no.dupl<-anti_join(unmatched,out.match.checked.dupl,by = c("titlenospace" = "titlenospace.x")) %>% 
  select(title,year)

# Export to Mendeley to populate missing fields using "Update Fields" function 
system.time(write_bibliography(unmatched.no.dupl,str_c(outputdir,"100_unmatched.no.dupl.ris")) )

# Read in from Mendeley with updated/populated fields
unmatched.no.dupl.mend.update <- read_bibliography(str_c(outputdir,"100_unmatched.no.dupl.mend.update.ris")) 

# export file to add to final data
export(unmatched.no.dupl.mend.update,str_c(outputdir,"100_match_check_to_add.csv"))



