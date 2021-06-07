library(stringr)
library(rio)
#Read the new dataset as data
data<-read.csv("100_match_check_to_add.csv")
#Read the de-duplicated dataset as finaldata
finaldata<-readRDS("De-duplication_oegm_all.rds")
#Create titlenospace column for the new dataset
data$titlelower<-tolower(data$title)
data$titlealnum<-str_replace_all(data$titlelower, "[^[:alnum:]]", " ")
data$titlealnum<-trimws(data$titlealnum)
searchString <- ' '
replacementString <- ''
data$titlenospace <- gsub(searchString,replacementString,data$titlealnum)
data$titlelower <- NULL
data$titlealnum <- NULL
#Create new columns to match the columns of the de-duplicated dataset
data$dataid<-replicate(nrow(data),0)
data$dataid<-as.integer(data$dataid)
data$address<-replicate(nrow(data),"NA")
data$language<-replicate(nrow(data),"NA")
data$CODE<-replicate(nrow(data),"NA")
data$publisher<-replicate(nrow(data),"NA")
data$accession<-replicate(nrow(data),"NA")
data$institution<-replicate(nrow(data),"NA")
data$batch<-replicate(nrow(data),"unique_90")
#Combine the new dataset to the de-duplicated dataset
finaldata1<-rbind(data,finaldata)
#Check the structure of the finaldata1 dataset
str(finaldata1)
#Export the finaldata1 dataset
export(finaldata1,"Finaldata.rds")