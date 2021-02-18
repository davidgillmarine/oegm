pacman::p_load(rio,data.table,janitor,tidyverse)

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
datanew <- last.file(outputdir,"_oegm_all.rds")

#View the data
View(datanew)
#Display structure
#str(datanew)
#Check for null values
datanew %>% summarise_all(funs(sum(is.na(.))))
#Before converting year column into integer values, remove any non-alphanumeric values to avoid 
#losing data
datanew$year<-str_replace_all(datanew$year, "[^[:alnum:]]", "")
#Convert year column into integer values
datanew$year<-as.integer(datanew$year)
#Change all titles and abstracts into lower case and save to new variables
titlelower<-tolower(datanew$title)
abstractlower<-tolower(datanew$abstract)
#Combine the new variables into the previous dataset
datanew1<-cbind(datanew,titlelower)
datanew2<-cbind(datanew1,abstractlower)
#Replace non-alphanumeric values with spaces for updated titles and abstracts, and save to new variables
titlealnum<-str_replace_all(datanew2$titlelower, "[^[:alnum:]]", " ")
abstractalnum<-str_replace_all(datanew2$abstractlower, "[^[:alnum:]]", " ")
#Combine the new variables into the previous dataset
datanew3<-cbind(datanew2,titlealnum)
datanew4<-cbind(datanew3,abstractalnum)
#Remove all spaces from updated titles and abstracts, and save to new variables
titlealnum<-trimws(titlealnum)
abstractalnum<-trimws(abstractalnum)
searchString <- ' '
replacementString <- ''
titlenospace <- gsub(searchString,replacementString,titlealnum)
abstractnospace <- gsub(searchString,replacementString,abstractalnum)
#Combine the new variables into the previous dataset
datanew5<-cbind(datanew4,titlenospace)
datanew6<-cbind(datanew5,abstractnospace)
#Select the first 100 characters from updated titles and abstracts
titleshortf<-substr(titlenospace, 1, 100) 
abstractshortf<-substr(abstractnospace, 1, 100)
#Combine the new variables into the previous dataset
datanew7<-cbind(datanew6,titleshortf)
datanew8<-cbind(datanew7,abstractshortf)
#Check the updated dataset
#View(datanew8)
#Set delete order by assigning 'a' to 'unique_10_include',
#assigning 'b' to 'unique_10_exclude'
#assigning 'c' to 'unique_90'
del_order <- data.frame(batch=c('unique_10_include','unique_10_exclude','unique_90'),
                        delorder=c('a','b','c'),stringsAsFactors = FALSE)
datanew9<-merge(datanew8,del_order,by.x='batch',by.y='batch',all.x=TRUE)
#Check the updated dataset
#View(datanew9)
#During spot checking, it was shown that papers with same titles but null values in abstract 
#are actually not duplicates
#Therefore, separate the values in 'datanotnull' and 'datanull' before deduplication
#'datanotnull' includes records that the abstract is not null and titles with more than 50 characters
datanotnull<-datanew9[is.na(datanew9$abstractshortf)==FALSE|nchar(datanew8$titleshortf)>50,]
#Check the filtered dataset
#View(datanotnull)
#'datanull' includes records that the abstract is null and titles with less than or equal to 50 #characters
datanull<-datanew9[is.na(datanew9$abstractshortf)&nchar(datanew8$titleshortf)<=50,]
#Check the filtered dataset
#View(datanull)
#Data checking processes
#Get duplicates using the updated titles and abstracts
datacheck1<-datanotnull%>%get_dupes('titleshortf','abstractshortf')
#Arrange data using updated titles, abstracts, and delete orders
datacheck2<-arrange(datacheck1,titleshortf,abstractshortf,delorder)
#View arranged data
#View(datacheck2)
#Remove duplicates in datacheck2 to further check if the processes are working as intended
datacheck3<-unique(setDT(datacheck2), by = c('titleshortf','abstractshortf'))
#View modified data
#View(datacheck3)
#Apply the previous processes to the created dataset
datanew11<-arrange(datanotnull,titleshortf,abstractshortf,delorder)
datanew12<-unique(setDT(datanew11), by = c('titleshortf','abstractshortf'))
#View modified data
#View(datanew12)
#Combined the 'datanull' back to the dataset
data<-rbind(datanew12,datanull)
#Drop the variables to match the original dataset
data$titlelower<-NULL
data$abstractlower<-NULL
data$titlealnum<-NULL
data$abstractalnum<-NULL
#data$titlenospace<-NULL (Kept for matching with accepted articles)
data$abstractnospace<-NULL
data$titleshortf<-NULL
data$abstractshortf<-NULL
data$delorder<-NULL
#Rearrange the order to match the original dataset
data<-data %>% relocate(batch, .after = institution)
#Create the dataid
dataid<-1:nrow(data)
#Final dataset
finaldata<-cbind(dataid,data)
#Export to csv and rds (directory needs to be created/modified)
today.date <- gsub("-","",Sys.Date())
export(finaldata,paste0(outputdir,today.date,"_oegm_all.csv"))
export(finaldata,paste0(outputdir,today.date,"_oegm_all.rds"))
# export(finaldata,"De-duplication.csv")
# export(finaldata,"De-duplication.rds")
#View the final dataset
View(finaldata)
#Display structure
str(finaldata)
