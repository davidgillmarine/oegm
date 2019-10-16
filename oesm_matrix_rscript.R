#set wd
setwd("C:/Users/david/Dropbox/data/analysis/oegm/data")
plotdir <- "C:/Users/david/Dropbox/data/analysis/oegm/plots/"
#setwd("C:/Users/dag71/Dropbox/data/analysis/oegm/data")
library(rio)
library(cowplot)
library(tidyverse)

#--- Organize data ----
#read in data, skip first 2 rows
non.mangrove<-import("Non-Mangrove Data 092519.xlsx", which="Data Extraction Sheet", skip =2, .name_repair = "universal") %>% 
  mutate(Date=as.character(Date))
mangrove<-import("Mangrove Data.xlsx", which="Data Extraction Sheet", skip =2,  .name_repair = "universal") %>% 
  mutate(Date=as.character(Date))

# Something went wrong? shift over rows that were missing a column so the data shifted (aid=772), inspect in original data
non.mangrove[non.mangrove$Article.ID%in%772,32:42] # wrong
non.mangrove[non.mangrove$Article.ID%in%772,32:42]  <- non.mangrove[non.mangrove$Article.ID%in%772,31:41] 
non.mangrove[non.mangrove$Article.ID%in%772,32:42]  # right

# read in intervention list
int_list<-import("intervention abbreviations.csv")
out_list<-import("outcome abbreviations.csv")
int_sub_list<-import("Non-Mangrove Data 092519.xlsx", which="Dropdowns", skip =1, .name_repair = "universal") %>% 
  select(Intervention.subcategory) %>% 
  na.omit() %>% 
  rename(int_sub=Intervention.subcategory)
out_sub_list<-import("Non-Mangrove Data 092519.xlsx", which="Dropdowns", skip =1, .name_repair = "universal") %>% 
  select(Outcome.subcategory) %>% 
  na.omit() %>% 
  rename(out_sub=Outcome.subcategory) 

data_all <- non.mangrove %>% 
  bind_rows(mangrove) %>% 
  filter(Full.text.screening.=="Accept" & !is.na(Intervention.category) & !is.na(Outcome.category)) %>% # is this ok to do? would be there cases where NA is ok?
  rename(Int_cat=Intervention.category,Outcome_cat=Outcome.category, aid=Article.ID) %>%
  # recodes any value begining with specific number
  mutate(Int_cat=ifelse(grepl("^10",Int_cat),"Int_10.inst_dev",Int_cat),
         Int_cat=ifelse(grepl("^1",Int_cat),"Int_1.area_mgmt",Int_cat),
         Int_cat=ifelse(grepl("^2",Int_cat),"Int_2.sp_mgmt",Int_cat),
         Int_cat=ifelse(grepl("^3",Int_cat),"Int_3.aware_raising",Int_cat),
         Int_cat=ifelse(grepl("^4",Int_cat),"Int_4.enforcement",Int_cat),
         Int_cat=ifelse(grepl("^5",Int_cat),"Int_5.liv_incent",Int_cat),
         Int_cat=ifelse(grepl("^6",Int_cat),"Int_6.con_plan",Int_cat),
         Int_cat=ifelse(grepl("^7",Int_cat),"Int_7.legal",Int_cat),
         Int_cat=ifelse(grepl("^8",Int_cat),"Int_8.research_monit",Int_cat),
         Int_cat=ifelse(grepl("^9",Int_cat),"Int_9.edu_train",Int_cat),
         Outcome_cat=ifelse(grepl("^1",Outcome_cat),"Out_1.knowledge",Outcome_cat),
         Outcome_cat=ifelse(grepl("^2",Outcome_cat),"Out_2.pop_sp",Outcome_cat),
         Outcome_cat=ifelse(grepl("^3",Outcome_cat),"Out_3.eco_comm",Outcome_cat),
         Outcome_cat=ifelse(grepl("^4",Outcome_cat),"Out_4.eco_funct",Outcome_cat),
         Outcome_cat=ifelse(grepl("^5",Outcome_cat),"Out_5.eco_serv",Outcome_cat),
         Outcome_cat=ifelse(grepl("^6",Outcome_cat),"Out_6.hwb",Outcome_cat),
         Outcome_cat=ifelse(grepl("^7",Outcome_cat),"Out_7.gov",Outcome_cat))

unique(data_all$Int_cat)  # 10 interventions
unique(data_all$Outcome_cat) # 7 outcomes
# these below should both be 0, or else we have a naming issue
unique(data_all$Int_cat[!data_all$Int_cat%in%int_list$Int_cat_abbr])
unique(data_all$Outcome_cat[!data_all$Outcome_cat%in%out_list$Outcome_cat_abbr])

#--- All studies ----
typology_dat <- data_all %>% 
  select("aid", "Int_cat", "Outcome_cat") %>% 
  filter(Int_cat!="" & !is.na(Int_cat)) %>%  # just in case
  distinct()

head(typology_dat)
#View(io_counts)
#head(io_counts)
#gather data and get counts 
io_counts <- typology_dat %>%
  group_by(Int_cat, Outcome_cat) %>% 
  count() %>%
  right_join(int_list, by=c("Int_cat"="Int_cat_abbr")) %>%  # get full intervention list
  select(-Int_cat_orig) %>% 
  spread(key=Int_cat, value=n) %>%  
  gather("Int_cat_abbr","n", Int_1.area_mgmt:Int_9.edu_train) %>% 
  filter(!is.na(Outcome_cat)) %>% 
  mutate(n=ifelse(is.na(n),0,as.integer(n))) %>%
  left_join(int_list,by="Int_cat_abbr") %>% 
  mutate(Int_cat_orig=factor(Int_cat_orig, 
                             levels=int_list$Int_cat_orig)) %>% 
  left_join(out_list,by=c("Outcome_cat"="Outcome_cat_abbr")) %>% 
  ungroup() %>% 
  select(Int_cat_orig,Outcome_cat_orig,n) 
  
head(io_counts)
nrow(int_list)*nrow(out_list)==nrow(io_counts) # quick check (should be true)

int_list1 <- typology_dat %>% 
  group_by(Int_cat) %>% 
  mutate(int_num=n_distinct(aid)) %>% 
  distinct(Int_cat,int_num) %>% 
  left_join(int_list,by=c("Int_cat"="Int_cat_abbr"))
out_list1 <- typology_dat %>% 
  group_by(Outcome_cat) %>% 
  mutate(out_num=n_distinct(aid)) %>% 
  distinct(Outcome_cat,out_num) %>% 
  left_join(out_list,by=c("Outcome_cat"="Outcome_cat_abbr"))

io_counts1 <- io_counts %>% 
  left_join(int_list1,by=("Int_cat_orig"))%>% 
  left_join(out_list1,by=("Outcome_cat_orig")) %>% 
  mutate(Int_cat_orig=paste0(Int_cat_orig," (",int_num,")"),
         Outcome_cat_orig=paste0(Outcome_cat_orig," (",out_num,")"),
         Outcome_cat_orig=gsub("1. Knowledge and behavior","8. Knowledge and behavior", Outcome_cat_orig)) %>% 
  ungroup() %>% 
  select(Int_cat_orig,Outcome_cat_orig,n)

#create heatmap
(heat.map.domain <- ggplot(data=io_counts1, aes(x=Int_cat_orig,y=reorder(Outcome_cat_orig, desc(Outcome_cat_orig)),fill=n)) +
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
# gather data and get counts 
io_counts_coral <- typology_dat_hab %>%
  filter(coral==1) %>% 
  group_by(Int_cat, Outcome_cat) %>% 
  count() %>%
  right_join(int_list, by=c("Int_cat"="Int_cat_abbr")) %>%  # get full intervention list
  select(-Int_cat_orig) %>% 
  spread(key=Int_cat, value=n) %>%  
  gather("Int_cat_abbr","n", Int_1.area_mgmt:Int_9.edu_train) %>% 
  filter(!is.na(Outcome_cat)) %>% 
  mutate(n=ifelse(is.na(n),0,as.integer(n))) %>%
  left_join(int_list,by="Int_cat_abbr") %>% 
  mutate(Int_cat_orig=factor(Int_cat_orig, 
                             levels=int_list$Int_cat_orig)) %>% 
  left_join(out_list,by=c("Outcome_cat"="Outcome_cat_abbr")) %>% 
  ungroup() %>% 
  select(Int_cat_orig,Outcome_cat_orig,n) 

head(io_counts_coral)
nrow(int_list)*nrow(out_list)==nrow(io_counts_coral) # quick check (should be true)

# Seagrass
# gather data and get counts 
io_counts_seagrass <- typology_dat_hab %>%
  filter(seagrass==1) %>% 
  group_by(Int_cat, Outcome_cat) %>% 
  count() %>%
  right_join(int_list, by=c("Int_cat"="Int_cat_abbr")) %>%  # get full intervention list
  select(-Int_cat_orig) %>% 
  spread(key=Int_cat, value=n) %>%  
  gather("Int_cat_abbr","n", Int_1.area_mgmt:Int_9.edu_train) %>% 
  filter(!is.na(Outcome_cat)) %>% 
  mutate(n=ifelse(is.na(n),0,as.integer(n))) %>%
  left_join(int_list,by="Int_cat_abbr") %>% 
  mutate(Int_cat_orig=factor(Int_cat_orig, 
                             levels=int_list$Int_cat_orig)) %>% 
  left_join(out_list,by=c("Outcome_cat"="Outcome_cat_abbr")) %>% 
  ungroup() %>% 
  select(Int_cat_orig,Outcome_cat_orig,n) 

head(io_counts_seagrass)
nrow(int_list)*nrow(out_list)==nrow(io_counts_seagrass) # quick check (should be true)

# Mangrove
# gather data and get counts 
io_counts_mangrove <- typology_dat_hab %>%
  filter(mangrove==1) %>% 
  group_by(Int_cat, Outcome_cat) %>% 
  count() %>%
  right_join(int_list, by=c("Int_cat"="Int_cat_abbr")) %>%  # get full intervention list
  select(-Int_cat_orig) %>% 
  spread(key=Int_cat, value=n) %>%  
  gather("Int_cat_abbr","n", Int_1.area_mgmt:Int_9.edu_train) %>% 
  filter(!is.na(Outcome_cat)) %>% 
  mutate(n=ifelse(is.na(n),0,as.integer(n))) %>%
  left_join(int_list,by="Int_cat_abbr") %>% 
  mutate(Int_cat_orig=factor(Int_cat_orig, 
                             levels=int_list$Int_cat_orig)) %>% 
  left_join(out_list,by=c("Outcome_cat"="Outcome_cat_abbr")) %>% 
  ungroup() %>% 
  select(Int_cat_orig,Outcome_cat_orig,n) 

head(io_counts_mangrove)
nrow(int_list)*nrow(out_list)==nrow(io_counts_mangrove) # quick check (should be true)

# get max # articles to put all 3 maps on the same colour scale
max.val=max(c(io_counts_coral$n,io_counts_seagrass$n,io_counts_mangrove$n))

#create heatmaps
(heat.map.domain.coral <- ggplot(data=io_counts_coral, aes(x=Int_cat_orig,y=reorder(Outcome_cat_orig, desc(Outcome_cat_orig)),fill=n)) +
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

(heat.map.domain.seagrass <- ggplot(data=io_counts_seagrass, aes(x=Int_cat_orig,y=reorder(Outcome_cat_orig, desc(Outcome_cat_orig)),fill=n)) +
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
(heat.map.domain.mangrove <- ggplot(data=io_counts_mangrove, aes(x=Int_cat_orig,y=reorder(Outcome_cat_orig, desc(Outcome_cat_orig)),fill=n)) +
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
ggsave(paste0(plotdir,'habitat_map_test.png'),width = 18,height = 8)

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

head(typology_dat_sub)
# View(io_counts_sub)
# head(io_counts_sub)
# names(io_counts_sub)
# head(str_sort(io_counts_sub$out_sub, numeric = TRUE))

#gather data and get counts 
io_counts_sub <- typology_dat_sub %>%
  group_by(int_sub, out_sub) %>% 
  count() %>%
  right_join(int_sub_list, by="int_sub") %>%  # get full intervention list
  full_join(out_sub_list, by="out_sub") %>%  # get full intervention list
  spread(key=int_sub, value=n) %>%  
  gather("int_sub","n", -out_sub) %>% 
  filter(!is.na(out_sub) & int_sub!="<NA>") %>% 
  mutate(n=ifelse(is.na(n),0,as.integer(n))) %>%
  mutate(int_sub=factor(int_sub,levels=str_sort(int_sub_list$int_sub, numeric = TRUE))) %>% 
  arrange(int_sub) %>% 
  select(int_sub,out_sub,n) 

head(io_counts_sub)
nrow(int_sub_list)*nrow(out_sub_list)==nrow(io_counts_sub) # quick check (should be true)

#create heatmap
(heat.map.domain_sub <- ggplot(data=io_counts_sub, aes(x=int_sub,y=reorder(out_sub, desc(out_sub)),fill=n)) +
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

ggsave(paste0(plotdir,'all_sub_map_test.png'),width = 15,height = 15)

