pacman::p_load(rio,hrbrthemes,viridis,revtools,janitor,tidyverse)
workdir <- "R:/Gill/research/oegm/"
#workdir <- gsub("git","data",getwd())
inputdir <- paste0(workdir,"tables/raw/efficency_sets/")
outputdir <- paste0(workdir,"output/tables/")
today.date <- gsub("-","",Sys.Date())

list.files(inputdir)
screened.set <- import(paste0(inputdir,"OEGM Set 5c_screened.csv"))
names(screened.set) <- gsub("citation_","",names(screened.set))
table(screened.set$screening_status)


exclude.set <- filter(screened.set,screening_status=="excluded")
exclude.set <- filter(screened.set,screening_status%in%c("excluded","conflict"))
include.set <- filter(screened.set,screening_status=="included")

# conflicts
conflict.set <- filter(screened.set,screening_status%in%c("conflict"))
conflict.set$title
exclude.set <- rbind(exclude.set,conflict.set[1,])
include.set <- rbind(include.set,conflict.set[2,])


# Export
sum(nrow(rbind(include.set,exclude.set)))==500
write_bibliography(exclude.set,str_c(outputdir,"OEGM Set 5c_screened_excl.ris")) # export to try to find missing abstracts
write_bibliography(include.set,str_c(outputdir,"OEGM Set 5c_screened_incl.ris")) # export to try to find missing abstracts

