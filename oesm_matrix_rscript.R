#set wd
#setwd("C:/Users/david/Dropbox/data/analysis/oegm/data")
workdir <- gsub("git","",getwd())
inputdir <- paste0(workdir,"data/")
plotdir <- paste0(workdir,"plots/")
library(rio)
library(cowplot)
library(tidyverse)

#--- Organize data ----
#read in data, skip first 2 rows
# non.mangrove<-import(paste0(inputdir,"Non-mangrove_FINAL.xlsx"), which="Data Extraction Sheet", skip =2, .name_repair = "universal") %>% 
#   mutate(Date=as.character(Date))
# mangrove<-import(paste0(inputdir,"Mangrove_FINAL.xlsx"), which="Data Extraction Sheet", skip =2,  .name_repair = "universal") %>% 
#   mutate(Date=as.character(Date))

all.data<-import(paste0(inputdir,"All Data_FINAL.xlsx"), skip =2, .name_repair = "universal") %>% 
  mutate(Date=as.character(Date))

# Select accepted papers
data_all <- all.data %>%  
  filter(Full.text.screening.=="Accept" & !is.na(Intervention.category) & !is.na(Outcome.category)) %>% # is this ok to do? would be there cases where NA is ok?
  rename(Int_cat=Intervention.category,Outcome_cat=Outcome.category, aid=Article.ID)


# read in full intervention lists
int_list<-import(paste0(inputdir,"intervention abbreviations.csv"))
out_list<-import(paste0(inputdir,"outcome abbreviations.csv"))
int_sub_list<-import(paste0(inputdir,"Non-Mangrove Data 092519.xlsx"), which="Dropdowns", skip =1, .name_repair = "universal") %>% 
  select(Intervention.subcategory) %>% 
  na.omit() %>% 
  rename(int_sub=Intervention.subcategory)
out_sub_list<-import(paste0(inputdir,"Non-Mangrove Data 092519.xlsx"), which="Dropdowns", skip =1, .name_repair = "universal") %>% 
  select(Outcome.subcategory) %>% 
  na.omit() %>% 
  rename(out_sub=Outcome.subcategory) 


#--- Clean up data ----
# Something went wrong? shift over rows that were missing a column so the data shifted (aid=772), inspect in original data
data_all[data_all$aid%in%772,32:42] # wrong
data_all[data_all$aid%in%772,32:42]  <- data_all[data_all$aid%in%772,31:41] 
data_all[data_all$aid%in%772,32:42]  # right

# Recodes any value begining with specific number to the correct version 
# interventions
for (i in 1:nrow(int_list)){
  num.val=paste0("^",i,"\\. *")
  data_all <- data_all %>% 
    mutate(Int_cat=ifelse(grepl(num.val,Int_cat),grep(num.val,int_list$Int_cat_orig,value = T),Int_cat))
}
# outcomes
for (i in 1:nrow(out_list)){
  num.val=paste0("^",i)
  data_all <- data_all %>% 
    mutate(Outcome_cat=ifelse(grepl(num.val,Outcome_cat),grep(num.val,out_list$Outcome_cat_orig,value = T),Outcome_cat))
}

# quick checks
unique(data_all$Int_cat)  # 10 interventions
unique(data_all$Outcome_cat) # 7 outcomes
# these below should both be 0, or else we have a naming issue
unique(data_all$Int_cat[!data_all$Int_cat%in%int_list$Int_cat_orig])
unique(data_all$Outcome_cat[!data_all$Outcome_cat%in%out_list$Outcome_cat_orig])
# test <- filter(data_all,Int_cat=="1. Land/water management")
# length(unique(test$aid))

# Add habitat fields
data_all <- data_all %>% 
mutate(coral=ifelse(grepl("Coral",Habitat.type,ignore.case = T),1,0),
       seagrass=ifelse(grepl("Seagrass",Habitat.type,ignore.case = T),1,0),
       mangrove=ifelse(grepl("mangrove",Habitat.type,ignore.case = T),1,0),)
# check
unique(data_all$Habitat.type[data_all$coral==1])
unique(data_all$Habitat.type[data_all$seagrass==1])
unique(data_all$Habitat.type[data_all$mangrove==1])


#--- All studies ----
# Get unique article ID, int. and out. list (and sum of articles)
typology_dat <- data_all %>% 
  select("aid", "Int_cat", "Outcome_cat") %>% 
  filter(Int_cat!="" & !is.na(Int_cat)) %>%  # just in case
  distinct() %>% 
  group_by(Int_cat) %>% 
  mutate(int_val=paste0(Int_cat," (",n_distinct(aid),")")) %>% 
  # group_by(Intervention.subcategory) %>% 
  # mutate(sub_int_val=paste0(Intervention.subcategory," (",n_distinct(aid),")")) %>% 
  group_by(Outcome_cat) %>% 
  mutate(out_val=paste0(Outcome_cat," (",n_distinct(aid),")")) %>% 
  # group_by(Outcome.subcategory) %>% 
  # mutate(sub_out_val=paste0(Outcome.subcategory," (",n_distinct(aid),")")) %>% 
  mutate(out_val=ifelse(grepl("Knowledge",out_val),gsub("1.","8.", out_val),out_val)) %>% 
  ungroup() %>% 
  select(-c(Int_cat,Outcome_cat))
  
head(typology_dat)

# Get full intervention list
int_list <- typology_dat %>%
  ungroup() %>% 
  distinct(int_val) %>% 
  mutate(ord=as.numeric(str_extract(int_val,"^\\d*"))) %>% # extract digit to sort list
  arrange(ord) 
out_list <- typology_dat %>% 
  ungroup() %>% 
  distinct(out_val) %>% 
  arrange(out_val)
io_list <- expand.grid(int_list$int_val,out_list$out_val) %>% 
  rename(int_val=Var1,out_val=Var2)
head(io_list)

#gather data and get counts 
io_counts <- typology_dat %>%
  group_by(int_val, out_val) %>% 
  count() %>%
  full_join(io_list) %>% 
  mutate(n=replace_na(n,0)) %>%
  ungroup() %>% 
  mutate(int_val=factor(int_val,levels=int_list$int_val)) 
  
head(io_counts)
nrow(int_list)*nrow(out_list)==nrow(io_counts) # quick check (should be true)

#create heatmap
(heat.map.domain <- ggplot(data=io_counts, aes(x=int_val,y=reorder(out_val, desc(out_val)),fill=n)) +
    geom_tile(color="gray90",size=0.1) +
    geom_text(aes(label=n),show.legend = F) +
    scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="# Cases",na.value="gray90", limits=c(0,max(io_counts$n))) +
    coord_equal() +
    # theme_tufte(base_family="Helvetica") +	# having issues with font, font colour in windows...
    theme(axis.ticks=element_line(size=0.4)) +
    theme(axis.text=element_text(size=9)) +
    theme(legend.title=element_text(size=10)) +
    theme(legend.text=element_text(size=10)) +
    theme(legend.title.align=1) +
    theme(legend.position="bottom") +
    theme(legend.key.size=unit(1, "cm")) +
    theme(legend.key.width=unit(1, "cm")) +
    labs(x="Conservation Intervention", y="Outcome", title ="All ecosystems") +
    theme(axis.text.x = element_text(angle=45,hjust=1,size=9)))

ggsave(paste0(plotdir,'all_map_test_update.png'),width = 8,height = 8)

#--- Ecosystem specific map ----
unique(data_all$Habitat.type)

typology_dat_hab <- data_all %>% 
  select("aid","Habitat.type", "Int_cat", "Outcome_cat") %>% 
  filter(Int_cat!="" & !is.na(Int_cat)) %>%  # just in case
  distinct() %>% 
  mutate(coral=ifelse(grepl("Coral",Habitat.type,ignore.case = T),1,0),
         seagrass=ifelse(grepl("Seagrass",Habitat.type,ignore.case = T),1,0),
         mangrove=ifelse(grepl("mangrove",Habitat.type,ignore.case = T),1,0),)
# check
unique(typology_dat_hab$Habitat.type[typology_dat_hab$coral==1])
unique(typology_dat_hab$Habitat.type[typology_dat_hab$seagrass==1])
unique(typology_dat_hab$Habitat.type[typology_dat_hab$mangrove==1])

# head(typology_dat_hab)
# View(io_counts_hab)
# head(io_counts_hab)

# Coral

int.cat.tot.coral <- typology_dat_hab %>% 
  filter(coral==1) %>% 
  group_by(Int_cat) %>% 
  summarise(num=n_distinct(aid)) %>% 
  mutate(int_val=paste0(Int_cat," (",num,")")) %>% 
  select(-num ) %>% 
  mutate(Int_cat=factor(Int_cat,levels=int_list$Int_cat_orig)) %>% 
  arrange(Int_cat)

out.cat.tot.coral <- typology_dat_hab %>% 
  filter(coral==1) %>% 
  group_by(Outcome_cat) %>% 
  summarise(num=n_distinct(aid)) %>% 
  mutate(out_val=paste0(Outcome_cat," (",num,")")) %>% 
  select(-num )

head(typology_dat)
#View(io_counts)
#head(io_counts)
#gather data and get counts 
io_counts_coral <- typology_dat_hab %>%
  filter(coral==1) %>% 
  group_by(Int_cat, Outcome_cat) %>% 
  count() %>%
  full_join(int.cat.tot.coral, by=c("Int_cat")) %>%  # get full intervention list (if all not there)
  full_join(out.cat.tot.coral,by=c("Outcome_cat")) %>% # get full outcome list (if all not there)
  ungroup() %>% 
  select(-Int_cat,-Outcome_cat) %>% 
  spread(key=int_val, value=n) %>%  
  gather("int_val","n", -out_val) %>% 
  filter(!is.na(out_val)) %>% 
  mutate(n=replace_na(n,0)) %>%
  mutate(int_val=factor(int_val,levels=int.cat.tot.coral$int_val)) %>% 
  ungroup() %>% 
  select(int_val,out_val,n) %>% 
  mutate(out_val=ifelse(grepl("Knowledge",out_val),gsub("1.","8.", out_val),out_val))

head(io_counts_coral)
nrow(int_list)*nrow(out_list)==nrow(io_counts_coral) # quick check (should be true)

# Seagrass
# gather data and get counts 
int.cat.tot.seagrass <- typology_dat_hab %>% 
  filter(seagrass==1) %>% 
  group_by(Int_cat) %>% 
  summarise(num=n_distinct(aid)) %>% 
  mutate(int_val=paste0(Int_cat," (",num,")")) %>% 
  select(-num ) %>% 
  mutate(Int_cat=factor(Int_cat,levels=int_list$Int_cat_orig)) %>% 
  arrange(Int_cat)

out.cat.tot.seagrass <- typology_dat_hab %>% 
  filter(seagrass==1) %>% 
  group_by(Outcome_cat) %>% 
  summarise(num=n_distinct(aid)) %>% 
  mutate(out_val=paste0(Outcome_cat," (",num,")")) %>% 
  select(-num )

head(typology_dat)
#View(io_counts)
#head(io_counts)
#gather data and get counts 
io_counts_seagrass <- typology_dat_hab %>%
  filter(seagrass==1) %>% 
  group_by(Int_cat, Outcome_cat) %>% 
  count() %>%
  full_join(int.cat.tot.seagrass, by=c("Int_cat")) %>%  # get full intervention list (if all not there)
  full_join(out.cat.tot.seagrass,by=c("Outcome_cat")) %>% # get full outcome list (if all not there)
  ungroup() %>% 
  select(-Int_cat,-Outcome_cat) %>% 
  spread(key=int_val, value=n) %>%  
  gather("int_val","n", -out_val) %>% 
  filter(!is.na(out_val)) %>% 
  mutate(n=replace_na(n,0)) %>%
  mutate(int_val=factor(int_val,levels=int.cat.tot.seagrass$int_val)) %>% 
  ungroup() %>% 
  select(int_val,out_val,n) %>% 
  mutate(out_val=ifelse(grepl("Knowledge",out_val),gsub("1.","8.", out_val),out_val))

head(io_counts_seagrass)
nrow(int_list)*nrow(out_list)==nrow(io_counts_seagrass) # quick check (should be true)


# Mangrove
# gather data and get counts 
int.cat.tot.mangrove <- typology_dat_hab %>% 
  filter(mangrove==1) %>% 
  group_by(Int_cat) %>% 
  summarise(num=n_distinct(aid)) %>% 
  mutate(int_val=paste0(Int_cat," (",num,")")) %>% 
  select(-num ) %>% 
  mutate(Int_cat=factor(Int_cat,levels=int_list$Int_cat_orig)) %>% 
  arrange(Int_cat)

out.cat.tot.mangrove <- typology_dat_hab %>% 
  filter(mangrove==1) %>% 
  group_by(Outcome_cat) %>% 
  summarise(num=n_distinct(aid)) %>% 
  mutate(out_val=paste0(Outcome_cat," (",num,")")) %>% 
  select(-num )

head(typology_dat)
#View(io_counts)
#head(io_counts)
#gather data and get counts 
io_counts_mangrove <- typology_dat_hab %>%
  filter(mangrove==1) %>% 
  group_by(Int_cat, Outcome_cat) %>% 
  count() %>%
  full_join(int.cat.tot.mangrove, by=c("Int_cat")) %>%  # get full intervention list (if all not there)
  full_join(out.cat.tot.mangrove,by=c("Outcome_cat")) %>% # get full outcome list (if all not there)
  ungroup() %>% 
  select(-Int_cat,-Outcome_cat) %>% 
  spread(key=int_val, value=n) %>%  
  gather("int_val","n", -out_val) %>% 
  filter(!is.na(out_val)) %>% 
  mutate(n=replace_na(n,0)) %>%
  mutate(int_val=factor(int_val,levels=int.cat.tot.mangrove$int_val)) %>% 
  ungroup() %>% 
  select(int_val,out_val,n) %>% 
  mutate(out_val=ifelse(grepl("Knowledge",out_val),gsub("1.","8.", out_val),out_val))

head(io_counts_mangrove)
nrow(int_list)*nrow(out_list)==nrow(io_counts_mangrove) # quick check (should be true)

max.val <- max(io_counts_mangrove$n,io_counts_coral$n,io_counts_seagrass$n)
#create heatmaps
(heat.map.domain.coral <- ggplot(data=io_counts_coral, aes(x=int_val,y=reorder(out_val, desc(out_val)),fill=n)) +
    geom_tile(color="gray90",size=0.1) +
    geom_text(aes(label=n),show.legend = F) +
    scale_fill_gradient2(low="#f7fbff",high="red3",name="# Cases",na.value="gray90", limits=c(0,max.val)) +
    coord_equal() +
    # theme_tufte(base_family="Helvetica") +	# having issues with font, font colour in windows...
    theme(axis.ticks=element_line(size=0.4)) +
    theme(axis.text=element_text(size=9)) +
    theme(legend.title=element_text(size=10)) +
    theme(legend.text=element_text(size=10)) +
    theme(legend.title.align=1) +
    theme(legend.position="bottom") +
    theme(legend.key.size=unit(1, "cm")) +
    theme(legend.key.width=unit(1, "cm")) +
    labs(x="Conservation Intervention", y="Outcome", title ="Coral") +
    theme(axis.text.x = element_text(angle=45,hjust=1,size=9)))

(heat.map.domain.seagrass <- ggplot(data=io_counts_seagrass, aes(x=int_val,y=reorder(out_val, desc(out_val)),fill=n)) +
    geom_tile(color="gray90",size=0.1) +
    geom_text(aes(label=n),show.legend = F) +
    scale_fill_gradient2(low="#f7fbff",high="green4",name="# Cases",na.value="gray90", limits=c(0,max.val)) +
    coord_equal() +
    # theme_tufte(base_family="Helvetica") +	# having issues with font, font colour in windows...
    theme(axis.ticks=element_line(size=0.4)) +
    theme(axis.text=element_text(size=9)) +
    theme(legend.title=element_text(size=10)) +
    theme(legend.text=element_text(size=10)) +
    theme(legend.title.align=1) +
    theme(legend.position="bottom") +
    theme(legend.key.size=unit(1, "cm")) +
    theme(legend.key.width=unit(1, "cm")) +
    labs(x="Conservation Intervention", y="Outcome", title ="Seagrass") +
    theme(axis.text.x = element_text(angle=45,hjust=1,size=9)))
(heat.map.domain.mangrove <- ggplot(data=io_counts_mangrove, aes(x=int_val,y=reorder(out_val, desc(out_val)),fill=n)) +
    geom_tile(color="gray90",size=0.1) +
    geom_text(aes(label=n),show.legend = F) +
    scale_fill_gradient2(low="#f7fbff",high="orangered4",name="# Cases",na.value="gray90", limits=c(0,max.val)) +
    coord_equal() +
    # theme_tufte(base_family="Helvetica") +	# having issues with font, font colour in windows...
    theme(axis.ticks=element_line(size=0.4)) +
    theme(axis.text=element_text(size=9)) +
    theme(legend.title=element_text(size=10)) +
    theme(legend.text=element_text(size=10)) +
    theme(legend.title.align=1) +
    theme(legend.position="bottom") +
    theme(legend.key.size=unit(1, "cm")) +
    theme(legend.key.width=unit(1, "cm")) +
    labs(x="Conservation Intervention", y="Outcome", title ="Mangrove") +
    theme(axis.text.x = element_text(angle=45,hjust=1,size=9)))

plot_grid(heat.map.domain.coral,heat.map.domain.seagrass, heat.map.domain.mangrove,labels=letters[1:3], ncol = 3, nrow = 1, hjust=-1)
ggsave(paste0(plotdir,'habitat_map_test_update.png'),width = 18,height = 8)

#--- Sub-group intervention ----
typology_dat_sub_int <- data_all %>% 
  select(aid, Intervention.subcategory, Outcome_cat) %>% 
  filter(Intervention.subcategory!="" & !is.na(Intervention.subcategory)) %>%  # just in case
  rename(int_sub=Intervention.subcategory) %>% 
  distinct()

anti_join(typology_dat_sub_int,int_sub_list, by="int_sub") # incorrect value entered
anti_join(int_sub_list,typology_dat_sub_int, by="int_sub") # those that were not observed

# temporarily fix incorrect text
typology_dat_sub_int <- typology_dat_sub_int %>% 
  mutate(int_sub=ifelse(grepl("^6a",int_sub),"6a. Protected area designation and/or acquisition",int_sub)) %>% 
  filter(int_sub!="Local")
anti_join(typology_dat_sub_int,int_sub_list, by="int_sub") # should be 0 now

head(typology_dat_sub_int)
# View(io_counts_sub)
# head(io_counts_sub_int)
# names(io_counts_sub)
# head(str_sort(io_counts_sub$out_sub, numeric = TRUE))

#gather data and get counts 
io_counts_sub_int <- typology_dat_sub_int %>%
  group_by(int_sub, Outcome_cat) %>% 
  count() %>%
  right_join(int_sub_list, by="int_sub") %>%  # get full intervention list
  full_join(select(out_list,Outcome_cat_abbr), by=c("Outcome_cat"="Outcome_cat_abbr")) %>%  # get full intervention list
  spread(key=int_sub, value=n) %>%  
  gather("int_sub","n", -Outcome_cat) %>% 
  filter(!is.na(Outcome_cat) & int_sub!="<NA>") %>% 
  mutate(n=ifelse(is.na(n),0,as.integer(n))) %>%
  mutate(int_sub=factor(int_sub,levels=str_sort(int_sub_list$int_sub, numeric = TRUE))) %>% 
  arrange(int_sub) %>% 
  select(int_sub,Outcome_cat,n) 

head(io_counts_sub_int)
nrow(int_sub_list)*nrow(out_list)==nrow(io_counts_sub_int) # quick check (should be true)

#create heatmap
(heat.map.domain_sub_int <- ggplot(data=io_counts_sub_int, aes(x=int_sub,y=reorder(Outcome_cat, desc(Outcome_cat)),fill=n)) +
    geom_tile(color="gray90",size=0.1) +
    geom_text(aes(label=n),show.legend = F) +
    scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="# Cases",na.value="gray90", limits=c(0,max(io_counts$n))) +
    coord_equal() +
    # theme_tufte(base_family="Helvetica") +	# having issues with font, font colour in windows...
    theme(axis.ticks=element_line(size=0.4)) +
    theme(axis.text=element_text(size=9)) +
    theme(legend.title=element_text(size=10)) +
    theme(legend.text=element_text(size=10)) +
    theme(legend.title.align=1) +
    theme(legend.position="bottom") +
    theme(legend.key.size=unit(1, "cm")) +
    theme(legend.key.width=unit(1, "cm")) +
    labs(x="Conservation Intervention", y="Outcome", title ="Intervention subcategories") +
    theme(axis.text.x = element_text(angle=45,hjust=1,size=9)))

ggsave(paste0(plotdir,'all_sub_int_map_test.png'),width = 15,height = 8)

#--- Sub-group both ----
typology_dat_sub <- data_all %>% 
  select(aid, Intervention.subcategory, Outcome.subcategory) %>% 
  filter(Intervention.subcategory!="" & !is.na(Intervention.subcategory)) %>%  # just in case
  rename(int_sub=Intervention.subcategory,out_sub=Outcome.subcategory) %>% 
  distinct()

anti_join(typology_dat_sub,int_sub_list, by="int_sub") # incorrect value entered
anti_join(int_sub_list,typology_dat_sub, by="int_sub") # those that were not observed

anti_join(typology_dat_sub,out_sub_list, by="out_sub") # incorrect value entered
anti_join(out_sub_list,typology_dat_sub, by="out_sub") # those that were not observed

# temporarily fix incorrect text
typology_dat_sub <- typology_dat_sub %>% 
  mutate(int_sub=ifelse(grepl("^6a",int_sub),"6a. Protected area designation and/or acquisition",int_sub)) %>% 
  filter(int_sub!="Local")
anti_join(typology_dat_sub,int_sub_list, by="int_sub") # should be 0 now

typology_dat_sub <- typology_dat_sub %>% 
  group_by(int_sub) %>% 
  mutate(int_sub_val=paste0(int_sub," (",n_distinct(aid),")")) %>% 
  group_by(out_sub) %>% 
  mutate(out_sub_val=paste0(out_sub," (",n_distinct(aid),")")) %>% 
  mutate(out_sub_val=ifelse(grepl("Knowledge",out_sub_val),gsub("1.","8.", out_sub_val),out_sub_val)) %>% 
  ungroup() %>% 
  select(-c(int_sub,out_sub))

head(typology_dat_sub)

# Get full intervention list
int_list <- typology_dat_sub %>%
  ungroup() %>% 
  distinct(int_sub_val) %>% 
  mutate(ord=as.numeric(str_extract(int_sub_val,"^\\d*"))) %>% 
  arrange(ord) 
out_list <- typology_dat_sub %>% 
  ungroup() %>% 
  distinct(out_sub_val) %>% 
  arrange(out_sub_val)
io_list <- expand.grid(int_list$int_sub_val,out_list$out_sub_val) %>% 
  rename(int_sub_val=Var1,out_sub_val=Var2)
head(io_list)

#gather data and get counts 
io_counts_sub <- typology_dat_sub %>%
  group_by(int_sub_val, out_sub_val) %>% 
  count() %>%
  full_join(io_list) %>% 
  mutate(n=replace_na(n,0)) %>%
  ungroup() %>% 
  mutate(int_sub_val=factor(int_sub_val,levels=int_list$int_sub_val)) 


head(io_counts_sub)

nrow(int_sub_list)*nrow(out_sub_list)==nrow(io_counts_sub) # quick check (should be true)

#create heatmap
(heat.map.domain_sub <- ggplot(data=io_counts_sub, aes(x=int_sub_val,y=reorder(out_sub_val, desc(out_sub_val)),fill=n)) +
    geom_tile(color="gray90",size=0.1) +
    geom_text(aes(label=n),show.legend = F) +
    scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="# Cases",na.value="gray90", limits=c(0,max(io_counts$n))) +
    coord_equal() +
    # theme_tufte(base_family="Helvetica") +	# having issues with font, font colour in windows...
    theme(axis.ticks=element_line(size=0.4)) +
    theme(axis.text=element_text(size=9)) +
    theme(legend.title=element_text(size=10)) +
    theme(legend.text=element_text(size=10)) +
    theme(legend.title.align=1) +
    theme(legend.position="bottom") +
    theme(legend.key.size=unit(1, "cm")) +
    theme(legend.key.width=unit(1, "cm")) +
    labs(x="Conservation Intervention", y="Outcome", title ="All subcategories") +
    theme(axis.text.x = element_text(angle=45,hjust=1,size=9)))

ggsave(paste0(plotdir,'all_sub_map_test+update.png'),width = 15,height = 15)

