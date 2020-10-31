pacman::p_load(rio,revtools,tidyverse)
workdir <- "R:/Gill/research/oegm/"
workdir <- gsub("git","data",getwd())
inputdir <- paste0(workdir,"/overlap/")
today.date <- gsub("-","",Sys.Date())

excl_ovrlp <- read_bibliography(paste0(inputdir,"OEGM-Set1_10_excluded_overlap.ris"))
incl_ovrlp <- read_bibliography(paste0(inputdir,"OEGM-Set2_10_included_overlap.ris"))
excl_unique <- read_bibliography(paste0(inputdir,"OEGM-Set3_10_excluded_unique.ris"))
incl_unique <- read_bibliography(paste0(inputdir,"OEGM-Set4_10_included_unique.ris"))

View(M)
