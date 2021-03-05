pacman::p_load(rio,rcrossref,bib2df,tidytext,revtools,fuzzyjoin,janitor,tidyverse)
library(data.table)
library(stringr)
library(dplyr)
library(janitor)
library(tidyverse)
library(readxl)
#Read the data file
finaldata<-readRDS("De-duplication_oegm_all.rds")
names(finaldata)[names(finaldata) == "titlenospace"] <- "title_z"
#Copied and modified from update_oegm_data.R
#------  ID  accepted studies in main dataset ---------
# Goals: 1. would like an indicator variable (0,1) to identify the accepted studies from the u10_include records 
# 2. copy over the Article.ID into the main database
full_include<-read_excel("20191022_All Data.xlsx",skip=2) %>% 
  filter(`Full text screening?`=="Accept") %>%  # accepted articles
  distinct(`Article ID`, .keep_all = T) %>%  # unique Article IDs
  select(`Article ID`,
         title=Title,
         year=`Year of publication`,
         author='Author(s)') %>% 
  mutate(across(c(title),list(z=~replace_na(.,"")))) %>%  # replace NAs with "" (for combining strings)
  mutate(across(c(title_z),~str_to_lower(.))) %>%   # lower case
  mutate(across(c(title_z),~str_replace_all(.,"\\W", ""))) %>% # remove anything that isn't a word character
  mutate(temp.id=paste(str_sub(title_z,1,16),
                       year,
                       sep="_")) 
full_include %>% 
  summarise(across(everything(),n_distinct))
sapply(full_include, function(x) sum(is.na(x))/length(x)) # % NAs
# All accepted articles were in the 10% included batch (can do with full dataset but subsetting to reduce computing time)
# Data selected from the final data after de-duplication
u10.include <- finaldata %>% 
  filter(batch=="unique_10_include") %>% 
  select(title,title_z,year,dataid) %>% 
  distinct(title_z,.keep_all = T)
# ---- Optional approach: Fuzzy string matching 
# Identify the best match in the main dataset using the "cleaned" titles to identify the rec.ids
#The 27 distance is selected by trying different numbers. After 27, there appears to be articles that are not the same but are selected to be matched
matched <- full_include %>% 
  #  distinct(Article.ID,.keep_all = T) %>% 
  select(`Article ID`,title,title_z,year) %>% 
  stringdist_left_join(select(u10.include,title,title_z,year,dataid),
                       by="title_z", ignore_case = T, max_dist = 27, distance_col="dist") %>% 
  #  filter(year.y==year.x) %>% 
  group_by(title.x) %>% 
  slice(which.min(dist)) %>% # record with lowest distance
  arrange(desc(dist)) %>% 
  select(title.x,title.y,year.x,year.y,everything())
#visually check to ensure that title.x is the same as title.y, if not, reduce max_dist or keep till next round
View(matched)
#Create a matched1 dataset and create a column called full.screen to indicate if the 
#article belongs to the full screen group
matched1<-matched
#Drop unnecessary columns
matched1$title_z.x<-NULL
matched1$title_z.y<-NULL
matched1$title.x<-NULL
matched1$title.y<-NULL
matched1$year.x<-NULL
matched1$year.y<-NULL
matched1$dist<-NULL
names(matched1)[names(matched1) == "Article ID"] <- "colndr.id"
full.screen<-replicate(nrow(matched1),1)
matched1<-cbind(matched1,full.screen)
#Separate finaldata dataset into in 'unique_10_include' and not in 'unique_10_include'
finaldata10in<-finaldata[finaldata$batch=="unique_10_include",]
finaldataother<-finaldata[finaldata$batch!="unique_10_include",]
#Set full.screen to 0 for data that is not in 'unique_10_include'
full.screen1<-replicate(nrow(finaldataother),0)
finaldataother<-cbind(finaldataother,full.screen1)
#Set colner.id1 to 0 for data that is not in 'unique_10_include'
colndr.id1<-replicate(nrow(finaldataother),0)
finaldataother<-cbind(finaldataother,colndr.id1)
#Change the name to remove the "1" in names
names(finaldataother)[names(finaldataother) == "full.screen1"] <- "full.screen"
names(finaldataother)[names(finaldataother) == "colndr.id1"] <- "colndr.id"
#Use dataid to match "matched1" dataset to "finaldata10in" dataset
MatchById<-merge(x = finaldata10in, y = matched1, by = "dataid", all.x = TRUE)
#The updated finaldata
finaldata<-rbind(MatchById,finaldataother)
#Change the 0 in colndr.id into null
finaldata$colndr.id<-na_if(finaldata$colndr.id, 0)
finaldata$title_z<-NULL
#View(finaldata)
#Check structure
str(finaldata)
#Create a matched2 dataset to be used to identify which accepted articles are not identified
matched2<-matched
matched2$title_z.x<-NULL
matched2$title_z.y<-NULL
matched2$title.x<-NULL
matched2$title.y<-NULL
matched2$year.x<-NULL
matched2$year.y<-NULL
matched2$dist<-NULL
matched2$dataid<-NULL
identified<-replicate(nrow(matched2),1)
matched2<-cbind(matched2,identified)
#Use Article ID to match "matched2" dataset to "full_include" dataset
MatchByArticleid<-merge(x = full_include, y = matched2, by = "Article ID", all.x = TRUE)
#Find the unidentified articles
unidentified<-MatchByArticleid[is.na(MatchByArticleid$identified),]
#Export unidentified to csv
export(unidentified,"Unidentified.csv")
#View(unidentified)
#Manually check the unidentified 25 articles
sort(agrep("Reef accessibility impairs the protection of sharks",finaldata$title,value=T,ignore.case = T,max.distance = 0.45))
sort(agrep("The Ocean is our Farm: Marine Conservation",finaldata$title,value=T,ignore.case = T,max.distance = 0.3))
sort(agrep("Long-term monitoring of habitats and reef fish found",finaldata$title,value=T,ignore.case = T,max.distance = 0.4))
sort(agrep("Eco-development in Orissa's protected areas",finaldata$title,value=T,ignore.case = T,max.distance = 0.3))
sort(agrep("Large Recovery of Fish Biomass in a No-Take Marine",finaldata$title,value=T,ignore.case = T,max.distance = 0.4))
sort(agrep("Restoration of threatened red gorgonian populations",finaldata$title,value=T,ignore.case = T,max.distance = 0.3))
findbatch1<-finaldata[finaldata$title=="Restoration of threatened red gorgonian populations: an experimental and modelling approach",]
findbatch1$batch
sort(agrep("Contrasting effects of marine protected areas on the",finaldata$title,value=T,ignore.case = T,max.distance = 0.2))
sort(agrep("The codevelopment of coastal fisheries monitoring",finaldata$title,value=T,ignore.case = T,max.distance = 0.3))
sort(agrep("Sea cucumbers in the Seychelles: effects of marine",finaldata$title,value=T,ignore.case = T,max.distance = 0.4))
sort(agrep("Assessing policies promoting poverty alleviation and",finaldata$title,value=T,ignore.case = T,max.distance = 0.4))
sort(agrep("Multi-level governance for large marine commons",finaldata$title,value=T,ignore.case = T,max.distance = 0.4))
sort(agrep("South-South Exchanges Enhance Resource",finaldata$title,value=T,ignore.case = T,max.distance = 0.45))
sort(agrep("REPORT ON THE DISTRIBUTION OF FISH SPECIES BETWEEN KISITE MARINE NA TIONAL P ARK AND lVIPUNGUTI MARINE NATIONAL RESERVE",finaldata$title,value=T,ignore.case = T,max.distance = 0.2))
findbatch2<-finaldata[finaldata$title=="Report on the Distribution of Fish Species Between the Kisite Marine National Park and the Mpunguti Marine National Reserve Part 2: Kenya Shimoni Marine Park Expedition 1992",]
findbatch2$batch
sort(agrep("Fish post-larvae assemblages at two contrasted coral reef habitats inFish",finaldata$title,value=T,ignore.case = T,max.distance = 0.4))
sort(agrep("Local fishing influences coral reef fish behavior inside protected areas of the",finaldata$title,value=T,ignore.case = T,max.distance = 0.45))
sort(agrep("Developing capacity for coastal management in the  absence of the government",finaldata$title,value=T,ignore.case = T,max.distance = 0.45))
sort(agrep("Adaptive Comanagement of a Marine Protected Area Network in Fiji",finaldata$title,value=T,ignore.case = T,max.distance = 0.4))
sort(agrep("The codevelopment of coastal fisheries monitoring methods to support local management",finaldata$title,value=T,ignore.case = T,max.distance = 0.5))
sort(agrep("Adaptive Capacity of Fishing Communities at Marine Protected Areas: A Case Study from the",finaldata$title,value=T,ignore.case = T,max.distance = 0.3))
sort(agrep("How Are Our MPAs Doing? Challenges in Assessing Global Patterns in Marine Protected Area",finaldata$title,value=T,ignore.case = T,max.distance = 0.5))
sort(agrep("Can Habitat Protection Lead to Improvements  in Human Well-Being? Evidence from",finaldata$title,value=T,ignore.case = T,max.distance = 0.5))
sort(agrep("Small Scale Fisheries Management: Lessons from Cockle Harvesters in Nicaragua and Tanzania",finaldata$title,value=T,ignore.case = T,max.distance = 0.5))
sort(agrep("The changing social relations of a community-based mangrove forest project in Zanzibar",finaldata$title,value=T,ignore.case = T,max.distance = 0.5))
sort(agrep("The role of fish and fisheries in recovering from natural hazards: Lessons learned from Vanuatu",finaldata$title,value=T,ignore.case = T,max.distance = 0.5))
sort(agrep("Of reef fishes, overfishing, and in situ observations of fish traps in St. John, U.S. Virgin Islands",finaldata$title,value=T,ignore.case = T,max.distance = 0.5))
