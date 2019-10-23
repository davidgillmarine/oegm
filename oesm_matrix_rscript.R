#set working directory to the correct folders on your machine
#setwd("C:/Users/david/Dropbox/data/analysis/oegm/data")
workdir <- gsub("git","",getwd())
inputdir <- paste0(workdir,"data/")
plotdir <- paste0(workdir,"plots/")
library(rio)
library(gtools)
library(cowplot)
library(tidyverse)

#--- Organize data ----
#read in data, skip first 2 rows
# non.mangrove<-import(paste0(inputdir,"Non-mangrove_FINAL.xlsx"), which="Data Extraction Sheet", skip =2, .name_repair = "universal") %>% 
#   mutate(Date=as.character(Date))
# mangrove<-import(paste0(inputdir,"Mangrove_FINAL.xlsx"), which="Data Extraction Sheet", skip =2,  .name_repair = "universal") %>% 
#   mutate(Date=as.character(Date))

all.data<-import(paste0(inputdir,"20191022_All Data.xlsx"), skip =2, .name_repair = "universal") %>% 
  mutate(Date=as.character(Date))

# Select accepted papers, rename variables
data_all <- all.data %>%  
  filter(Full.text.screening.=="Accept" & !is.na(Intervention.category) & !is.na(Outcome.category)) %>% # is this ok to do? would be there cases where NA is ok?
  rename(Int_cat=Intervention.category,Outcome_cat=Outcome.category, aid=Article.ID)


# read in full intervention lists
int_list<-import(paste0(inputdir,"intervention abbreviations.csv"))
out_list<-import(paste0(inputdir,"outcome abbreviations.csv"))
drop.down.lists<-import(paste0(inputdir,"20191022_All Data.xlsx"), which="Dropdowns", skip =1, .name_repair = "universal")
int_sub_list <-  drop.down.lists %>% 
  select(Intervention.subcategory) %>% 
  na.omit() %>% 
  rename(int_sub=Intervention.subcategory)
out_sub_list <-  drop.down.lists %>% 
  select(Outcome.subcategory) %>% 
  na.omit() %>% 
  rename(out_sub=Outcome.subcategory) 


#--- Clean up data ----
# Something went wrong? shift over rows that were missing a column so the data shifted (aid=772), inspect in original data
data_all[data_all$aid%in%772,32:42] # wrong
data_all[data_all$aid%in%772,32:42]  <- data_all[data_all$aid%in%772,31:41] 
data_all[data_all$aid%in%772,32:42]  # right

# Recodes intervention and outcomes fields to the correct version 
# interventions
for (i in 1:nrow(int_list)){
  num.val=paste0("^",i,"\\. *") # starts with this number
  data_all <- data_all %>% 
    mutate(Int_cat=ifelse(grepl(num.val,Int_cat),grep(num.val,int_list$Int_cat_orig,value = T),Int_cat))
}
# sub-interventions
for (i in 1:nrow(int_sub_list)){
  num.val=paste0("^",gsub("\\.(.*)","",int_sub_list$int_sub[i]))  # starts with this number
  data_all <- data_all %>% 
    mutate(Intervention.subcategory=ifelse(grepl(num.val,Intervention.subcategory),
                                           grep(num.val,int_sub_list$int_sub,value = T),Intervention.subcategory))
}
# Outcomes
for (i in 1:nrow(out_list)){
  num.val=paste0("^",i)
  data_all <- data_all %>% 
    mutate(Outcome_cat=ifelse(grepl(num.val,Outcome_cat),grep(num.val,out_list$Outcome_cat_orig,value = T),Outcome_cat))
}
# Sub-outcomes
for (i in 1:nrow(out_sub_list)){
  num.val=paste0("^",gsub("\\.(.*)","",out_sub_list$out_sub[i]))  # starts with this number
  data_all <- data_all %>% 
    mutate(Outcome.subcategory=ifelse(grepl(num.val,Outcome.subcategory),
                                           grep(num.val,out_sub_list$out_sub,value = T),Outcome.subcategory))
}

#quick checks
unique(data_all$Int_cat)  # 10 interventions
unique(data_all$Outcome_cat) # 7 outcomes
sort(unique(data_all$Intervention.subcategory)) # 36 sub-outcomes?
sort(unique(data_all$Outcome.subcategory)) # 41 sub-outcomes?

# those that were not observed
int_list$Int_cat_orig[!int_list$Int_cat_orig%in%data_all$Int_cat] 
int_sub_list$int_sub[!int_sub_list$int_sub%in%data_all$Intervention.subcategory] 
out_list$Outcome_cat_orig[!out_list$Outcome_cat_orig%in%data_all$Outcome_cat] 
out_sub_list$out_sub[!out_sub_list$out_sub%in%data_all$Outcome.subcategory] 

# incorrect values entered (all should be zero)  !Morgan: Intervention.subcategory with an NA
data_all$Int_cat[!data_all$Int_cat%in%int_list$Int_cat_orig] 
data_all$Intervention.subcategory[!data_all$Intervention.subcategory%in%int_sub_list$int_sub] 
data_all$Outcome_cat[!data_all$Outcome_cat%in%out_list$Outcome_cat_orig] 
data_all$Outcome.subcategory[!data_all$Outcome.subcategory%in%out_sub_list$out_sub] 

# Add habitat fields
data_all <- data_all %>% 
mutate(coral=ifelse(grepl("Coral",Habitat.type,ignore.case = T),1,0),
       seagrass=ifelse(grepl("Seagrass",Habitat.type,ignore.case = T),1,0),
       mangrove=ifelse(grepl("mangrove",Habitat.type,ignore.case = T),1,0),)
# check habitat coding
unique(data_all$Habitat.type[data_all$coral==1])
unique(data_all$Habitat.type[data_all$seagrass==1])
unique(data_all$Habitat.type[data_all$mangrove==1])

#--- Heat map function ----
my_heat_map <- function (.data,intc, outc, high.col="#2171b5") {
  
# don't know why... but this works
  .data$intc<- as.vector(.data %>%  pull(intc))
  .data$outc<- as.vector(.data %>%  pull(outc))

# Get unique article ID, int. and out. list (and sum of articles)
  typology_dat <- .data %>% 
    select(aid, intc, outc) %>% 
    filter(intc!="" & !is.na(intc)) %>%  # just in case
    distinct() %>% 
    group_by(intc) %>% 
    mutate(int_val=paste0(intc," (",n_distinct(aid),")")) %>% 
    group_by(outc) %>% 
    mutate(out_val=paste0(outc," (",n_distinct(aid),")")) %>% 
    ungroup() %>% 
    select(-c(intc,outc))
  
  # Get full intervention list
  int_list <- typology_dat %>%
    ungroup() %>% 
    distinct(int_val) %>% 
    arrange(int_val)
  out_list <- typology_dat %>% 
    ungroup() %>% 
    distinct(out_val) %>% 
    arrange(out_val)
  io_list <- expand.grid(int_list$int_val,out_list$out_val) %>% 
    rename(int_val=Var1,out_val=Var2)
  head(io_list)
  
  #gather data and get counts 
  io_counts <<- typology_dat %>%
    group_by(int_val, out_val) %>% 
    count() %>%
    full_join(io_list) %>%   # inserts missing combinations
    mutate(n=replace_na(n,0)) %>%
    ungroup() %>% 
    mutate(int_val=factor(int_val,levels=mixedsort(int_list$int_val)))  # set int list in right order
  
  # head(io_counts)
  # nrow(int_list)*nrow(out_list)==nrow(io_counts) # quick check (should be true)
  
  #create heatmap
  heat.map <<- ggplot(data=io_counts, aes(x=int_val,y=reorder(out_val, desc(out_val)),fill=n)) +
      geom_tile(color="gray90",size=0.1) +
      geom_text(aes(label=n),show.legend = F) +
      scale_fill_gradient2(low="#f7fbff",high=high.col,name="# Cases",na.value="gray90", limits=c(0,max(io_counts$n))) +
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
      #  labs(x="Conservation Intervention", y="Outcome", title ="All ecosystems") +
      theme(axis.text.x = element_text(angle=45,hjust=1,size=9))
}

#--- Create maps ----
# All ecosystems
my_heat_map(data_all,"Int_cat","Outcome_cat")
io_counts.all <- io_counts
(heat.map.all <- heat.map + labs(x="Conservation Intervention", y="Outcome", title ="All ecosystems"))
ggsave(paste0(plotdir,'all_map_test_update.png'),width = 8,height = 8)

# All ecosystems - Intervention subcategory X Outcome
my_heat_map(data_all,"Intervention.subcategory", "Outcome_cat")
io_counts.subint <- io_counts
(heat.map.subint <- heat.map + labs(x="Conservation Intervention", y="Outcome", title ="All ecosystems"))
ggsave(paste0(plotdir,'all_sub_int_map_test.png'),width = 15,height = 8)

# All ecosystems - Intervention  X Outcome subcategory
my_heat_map(data_all,"Int_cat", "Outcome.subcategory")
io_counts.subint <- io_counts
(heat.map.subint <- heat.map + labs(x="Conservation Intervention", y="Outcome", title ="All ecosystems"))
ggsave(paste0(plotdir,'all_sub_out_map_test.png'),width = 8,height = 15)

# All ecosystems - Intervention subcategory X Outcome subcategory
my_heat_map(data_all,"Intervention.subcategory", "Outcome.subcategory")
io_counts.subintout <- io_counts
(heat.map.subint <- heat.map + labs(x="Conservation Intervention", y="Outcome", title ="All ecosystems"))
ggsave(paste0(plotdir,'all_sub_map_test+update.png'),width = 15,height = 15)

# Coral
my_heat_map(filter(data_all,coral==1),"Int_cat","Outcome_cat", high.col = "red3")
io_counts.coral <- io_counts
(heat.map.coral <- heat.map + labs(x="Conservation Intervention", y="Outcome", title ="Coral"))

# Mangrove
my_heat_map(filter(data_all,mangrove==1),"Int_cat","Outcome_cat", high.col = "orangered4")
io_counts.mangrove <- io_counts
(heat.map.mangrove <- heat.map + labs(x="Conservation Intervention", y="Outcome", title ="Mangrove")) 

# Seagrass
my_heat_map(filter(data_all,seagrass==1),"Int_cat","Outcome_cat", high.col = "green4")
io_counts.seagrass <- io_counts
(heat.map.seagrass <- heat.map + labs(x="Conservation Intervention", y="Outcome", title ="Seagrass")) 

# Ecosystem maps on same scale
max.val <- max(io_counts.mangrove$n,io_counts.coral$n,io_counts.seagrass$n)
plot_grid(heat.map.coral + scale_fill_gradient2(low="#f7fbff",high= "red3",name="# Cases",na.value="gray90", limits=c(0,max.val)),
          heat.map.seagrass + scale_fill_gradient2(low="#f7fbff",high= "green4",name="# Cases",na.value="gray90", limits=c(0,max.val)),
          heat.map.mangrove + scale_fill_gradient2(low="#f7fbff",high= "orangered4",name="# Cases",na.value="gray90", limits=c(0,max.val)),
          labels=letters[1:3], ncol = 3, nrow = 1, hjust=-1, align = "hv")
ggsave(paste0(plotdir,'habitat_map_test_update.png'),width = 18,height = 8)

