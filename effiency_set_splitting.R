pacman::p_load(rio,hrbrthemes,viridis,revtools,janitor,tidyverse)
workdir <- "R:/Gill/research/oegm/"
#workdir <- gsub("git","data",getwd())
inputdir <- paste0(workdir,"tables/raw/efficency_sets/")
outputdir <- inputdir
plotdir <- paste0(workdir,"output/plots/")
today.date <- gsub("-","",Sys.Date())

list.files(inputdir)
screened.set <- import(paste0(inputdir,"OEGM Set 2c_screened.csv"))

table(screened.set$citation_screening_status)
exclude.set <- filter(screened.set,citation_screening_status=="excluded")
write_bibliography(exclude.set,str_c(outputdir,"OEGM Set 2c_screened_excl.ris")) # export to try to find missing abstracts

include.set <- filter(screened.set,citation_screening_status=="included")
write_bibliography(exclude.set,str_c(outputdir,"OEGM Set 2c_screened_incl.ris")) # export to try to find missing abstracts

