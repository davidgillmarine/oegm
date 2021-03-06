#set working directory to the correct folders on your machine
workdir <- "R:/Gill/research/oegm/"
inputdir <- paste0(workdir,"tables/raw/")
mapdir <-  paste0(workdir,"spatial/raw/")
plotdir <- paste0(workdir,"output/plots/")
library(rio)
library(treemap)
library(gtools)
library(cowplot)
library(tidyverse)
today.date <- gsub("-","",Sys.Date())


#--- Organize data ----
#read in data, skip first 2 rows
# non.mangrove<-import(paste0(inputdir,"Non-mangrove_FINAL.xlsx"), which="Data Extraction Sheet", skip =2, .name_repair = "universal") %>% 
#   mutate(Date=as.character(Date))
# mangrove<-import(paste0(inputdir,"Mangrove_FINAL.xlsx"), which="Data Extraction Sheet", skip =2,  .name_repair = "universal") %>% 
#   mutate(Date=as.character(Date))

all.data<-import(paste0(inputdir,"20191022_All Data.xlsx"), skip =2, .name_repair = "universal") %>% 
  mutate(Date=as.character(Date))
country <- import(paste0(inputdir,"allcountries.csv"))

# Count total papers in Full Text Screening
length(unique(all.data$Article.ID))

# Select accepted papers, rename variables==
data_all <- all.data %>%  
  filter(Full.text.screening.=="Accept" & !is.na(Intervention.category) & !is.na(Outcome.category)) %>% # is this ok to do? would be there cases where NA is ok?
  rename(Int_cat=Intervention.category,Outcome_cat=Outcome.category, aid=Article.ID, study.ctry=Country.ies..of.study)
length(unique(data_all$aid))

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
unique(data_all$Habitat.type) 
data_all <- data_all %>% 
mutate(coral=ifelse(grepl("Coral",Habitat.type,ignore.case = T),1,0),
       seagrass=ifelse(grepl("Seagrass",Habitat.type,ignore.case = T),1,0),
       mangrove=ifelse(grepl("mangrove",Habitat.type,ignore.case = T),1,0),)

  # check habitat coding
unique(data_all$Habitat.type[data_all$coral==1])
unique(data_all$Habitat.type[data_all$seagrass==1])
unique(data_all$Habitat.type[data_all$mangrove==1])

habitat.test <- data_all %>% 
  filter(coral==0 & mangrove==0 & seagrass==0) %>% 
  select(Habitat.type)
unique(habitat.test$Habitat.type) # should just be "other"


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
    mutate(int_val=factor(int_val,levels=mixedsort(int_list$int_val)),  # set int list in right order
           out_val=gsub("^1","6temp",out_val),
           out_val=gsub("^2","1",out_val),
           out_val=gsub("^3","2",out_val),
           out_val=gsub("^4","3",out_val),
           out_val=gsub("^5","4",out_val),
           out_val=gsub("^6","5",out_val),
           out_val=gsub("^7","7",out_val),
           out_val=gsub("5temp","6",out_val)
           )
  
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
      theme(axis.text.x = element_text(angle=45,hjust=0,vjust=1,size=9)) +
      scale_x_discrete(position = "top") +
      scale_y_discrete(position = "right") 
  
  
}

#--- Create maps ----
# All ecosystems
my_heat_map(data_all,"Int_cat","Outcome_cat")
io_counts.all <- io_counts
(heat.map.all <- heat.map +
    labs(x="Conservation Intervention", y="Outcome", title ="All ecosystems"))
ggsave(paste0(plotdir,today.date,'_int_out_map.png'),width = 8,height = 8)

# All ecosystems - Intervention subcategory X Outcome
my_heat_map(data_all,"Intervention.subcategory", "Outcome_cat")
io_counts.subint <- io_counts
(heat.map.subint <- heat.map + labs(x="Conservation Intervention", y="Outcome", title ="All ecosystems"))
ggsave(paste0(plotdir,today.date,'_sub.int_out_map.png'),width = 16,height = 8)

# All ecosystems - Intervention  X Outcome subcategory
my_heat_map(data_all,"Int_cat", "Outcome.subcategory")
io_counts.subint <- io_counts
(heat.map.subint <- heat.map + labs(x="Conservation Intervention", y="Outcome", title ="All ecosystems"))
ggsave(paste0(plotdir,today.date,'_int_sub.out_map.png'),width = 8,height = 16)

# All ecosystems - Intervention subcategory X Outcome subcategory
my_heat_map(data_all,"Intervention.subcategory", "Outcome.subcategory")
io_counts.subintout <- io_counts
(heat.map.subint <- heat.map + labs(x="Conservation Intervention", y="Outcome", title ="All ecosystems"))
ggsave(paste0(plotdir,today.date,'_sub.int_sub.out_map.png'),width = 8,height = 8)

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
ggsave(paste0(plotdir,today.date,'_habitat_int_out_map.png'),width = 17,height = 8)



#---- World map ----
unique(data_all$study.ctry)
# fix data in country column
ctry <- data_all %>%
  mutate(study.ctry=gsub(",",";",study.ctry),  # replace "," with ";" to help with splitting
         study.ctry=gsub("Korea; South","Korea, South",study.ctry),
         study.ctry=gsub("Turks and Caicos","Turks and Caicos Islands",study.ctry),
         study.ctry=gsub(".*Virgin Islands","U.S. Virgin Islands",study.ctry),
         study.ctry=gsub("Tanzania; United Republic Of","Tanzania, United Republic Of",study.ctry),
         study.ctry=gsub("Taiwan; Province Of China","Taiwan, Province Of China",study.ctry)) %>%
  group_by(aid,study.ctry) %>%
  summarise() %>% 
  filter(!is.na(study.ctry))
head(ctry)


# get no. articles per country (and percentage) to create vector of column names for each country by counting the number of ";" 
max.ctry <- paste0("ctry",rep(1:max(str_count(ctry$study.ctry, ";")+1))) 
num.studies <- length(unique(data_all$aid))

ctry_sum <-ctry %>% 
  separate(study.ctry,max.ctry, sep="; ",fill="right") %>% # fill=right fills blanks with NAs
  gather("X","Country", max.ctry) %>% 
  filter(!is.na(Country)) %>% 
  select(-X) %>% 
  group_by(Country) %>% 
  count() %>% 
  mutate(pct=n/num.studies)

arrange(ctry_sum,desc(n))


# Countries that need to be fixed
ctry_sum$Country[!ctry_sum$Country%in%country$Country]
# agrep("Virgin Islands",country$Country, value = T) - search original list

# join to full country list
ctry_sum1 <- country %>% 
  left_join(ctry_sum,by="Country") %>%
  mutate(n=na.replace(n,0)) 

ctry_sum$Country[!ctry_sum$Country%in%ctry_sum1$Country] # any missing?

# read in map
map <- rgdal::readOGR(paste0(mapdir,"TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp"))
map <-broom::tidy(map,region="ISO3")
#map@data[map@data$NAME=="Turks and Caicos Islands",]


#plot study map
(study_ctry_map <- ggplot() +
    geom_map(data=ctry_sum1, aes(map_id=code, fill=n),map=map) +
    expand_limits(x=map$long,y=map$lat) +
    theme(panel.background = element_rect(fill = "#CCCCCC", colour = "#CCCCCC"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_fill_gradient2(low="white",mid="#2171b5",high="#08519c",
                         midpoint=max(ctry_sum1$n)/2,limits=c(0,max(ctry_sum1$n)))+
    labs(title="Study countries"))
ggsave(paste0(plotdir,today.date,'_study_country_map.png'),width = 10,height = 5)


#---- Treemapping ----
# - Outcomes
unique(data_all$Outcome.subcategory)

outcomes2 <-data_all %>% 
  filter(!is.na(Outcome.subcategory)) %>% 
  select(aid,Outcome_cat,Outcome.subcategory) %>% 
  group_by(Outcome_cat) %>% 
  mutate(out.cat=paste0(Outcome_cat,"\n (",n_distinct(aid),")")) %>% 
  group_by(Outcome_cat,Outcome.subcategory) %>% 
  mutate(n=n_distinct(aid)) %>% 
  arrange(Outcome.subcategory)
unique(outcomes2$out.cat)

pdf(file=paste0(plotdir,today.date,"_outcomes_treemap.pdf"))
treemap(outcomes2,index=c("out.cat","Outcome.subcategory"),
        vSize="n",
        type="index",
        fontsize.labels = c(15,10),
        fontcolor.labels=c("white","black"),
        fontface.labels=c(2,1),
        bg.labels=c("transparent"),
        align.labels=list(
          c("center","top"),
          c("left","bottom")
        ),
        overlap.labels=0.5,
        inflate.labels=F,
        title="Study Outcomes")
dev.off()

# - Interventions
unique(data_all$Intervention.subcategory)

intervention2 <-data_all %>% 
  filter(!is.na(Intervention.subcategory)) %>% 
  select(aid,Int_cat,Intervention.subcategory) %>% 
  group_by(Int_cat) %>% 
  mutate(int.cat=paste0(Int_cat,"\n (",n_distinct(aid),")")) %>% 
  group_by(Int_cat,Intervention.subcategory) %>% 
  mutate(n=n_distinct(aid)) %>% 
  arrange(Intervention.subcategory)

unique(intervention2$int.cat)

pdf(file=paste0(plotdir,today.date,"_intervention_treemap.pdf"))
treemap(intervention2,index=c("int.cat","Intervention.subcategory"),
        vSize="n",
        type="index",
        fontsize.labels = c(15,10),
        fontcolor.labels=c("white","black"),
        fontface.labels=c(2,1),
        bg.labels=c("transparent"),
        align.labels=list(
          c("center","top"),
          c("left","bottom")
        ),
        overlap.labels=0.5,
        inflate.labels=F,
        title="Study interventions")
dev.off()



#---- Studies over time ----
All_time_gps <- data_all %>%
  group_by(Year.of.publication) %>% 
  summarise(pub.per.yr= n_distinct(aid)) %>% 
  ungroup() %>% 
  arrange(Year.of.publication) %>% 
  mutate(val=cumsum(pub.per.yr),
         gp="All") %>% 
  select(Year.of.publication,gp,val) %>% 
  filter(!is.na(Year.of.publication))
tail(All_time_gps)
# growth by decade
#overall
(max(All_time_gps$val)-min(All_time_gps$val))/(max(All_time_gps$Year.of.publication)-min(All_time_gps$Year.of.publication))
#1990s
(max(All_time_gps$val[All_time_gps$Year.of.publication<2000])-max(All_time_gps$val[All_time_gps$Year.of.publication<1990]))/10
#2000s
(max(All_time_gps$val[All_time_gps$Year.of.publication<2010])-max(All_time_gps$val[All_time_gps$Year.of.publication<2000]))/10
#2000-latest date
(max(All_time_gps$val)-max(All_time_gps$val[All_time_gps$Year.of.publication<2010]))/(max(All_time_gps$Year.of.publication)-2009)

Int_time_gps <-  data_all %>%
  group_by(Year.of.publication,Int_cat) %>% 
  summarise(int_per_yr= n_distinct(aid)) %>% 
  spread(Int_cat,value = int_per_yr) %>% 
  mutate_all(funs(replace(., is.na(.), 0))) %>% 
  ungroup() %>% 
  arrange(Year.of.publication) %>% 
  mutate_at(vars(-Year.of.publication),funs(cumsum(.))) %>% 
  gather("int_typ","val",-Year.of.publication) %>% 
  bind_rows(All_time_gps %>% rename(int_typ=gp))
tail(Int_time_gps)
# non-elegant way to convert units to factor in order to reorder plot legends
order.gp <- Int_time_gps %>% group_by(int_typ) %>% summarise(order_typ=max(val)) %>% arrange(desc(order_typ)) %>% pull(int_typ)
Int_time_gps <- Int_time_gps %>%  
  mutate(int_typ = factor(int_typ, levels = order.gp)) %>% 
  rename(Intervention=int_typ)

# int.cols <- c("black","blue","orangered1","yellow4","tan1")
names(int.cols) <- order.gp

(pInt_time_gps <- ggplot(Int_time_gps)  + 
    geom_line(aes(x = Year.of.publication, y = val, group = Intervention, colour = Intervention), size = 1.3)  +
    ylab("Cumulative frequency") +
  #  scale_x_continuous(name="Year", breaks=seq(1985,2025,5)) +
  #  scale_colour_manual(values=int.cols) +
    ggtitle('Intervention') )
   # scale_y_continuous(limits=c(0,80), breaks=seq(0,80,10), labels=c("0","","20","","40","","60","","80")))


# Outcomes

out_time_gps <-  data_all %>%
  group_by(Year.of.publication,Outcome_cat) %>% 
  summarise(out_per_yr= n_distinct(aid)) %>% 
  spread(Outcome_cat,value = out_per_yr) %>% 
  mutate_all(funs(replace(., is.na(.), 0))) %>% 
  ungroup() %>% 
  arrange(Year.of.publication) %>% 
  mutate_at(vars(-Year.of.publication),funs(cumsum(.))) %>% 
  gather("out_typ","val",-Year.of.publication) %>% 
  bind_rows(All_time_gps %>% rename(out_typ=gp))
tail(out_time_gps)
# non-elegant way to convert units to factor in order to reorder plot legends
order.gp <- out_time_gps %>% group_by(out_typ) %>% summarise(order_typ=max(val)) %>% arrange(desc(order_typ)) %>% pull(out_typ)
out_time_gps <- out_time_gps %>%  
  mutate(out_typ = factor(out_typ, levels = order.gp)) %>% 
  rename(Outcome=out_typ)

# int.cols <- c("black","blue","orangered1","yellow4","tan1")
# names(int.cols) <- order.gp

(pout_time_gps <- ggplot(out_time_gps)  + 
    geom_line(aes(x = Year.of.publication, y = val, group = Outcome, colour = Outcome), size = 1.3)  +
    ylab("Cumulative frequency") +
    #  scale_x_continuous(name="Year", breaks=seq(1985,2025,5)) +
    #  scale_colour_manual(values=int.cols) +
    ggtitle('Outcome') )
# scale_y_continuous(limits=c(0,80), breaks=seq(0,80,10), labels=c("0","","20","","40","","60","","80")))
plot_grid(pInt_time_gps,pout_time_gps)
ggsave(paste0(plotdir,today.date,'_studies_over_time.png'),width = 12,height = 4)


########################
# DIG IN PROGRESS:

# --- Percentages --- #
# test <-  data_all %>%
#   select(aid, Int_cat, Intervention.subcategory, Outcome_cat,Outcome.subcategory)

#Total number of studies
num.studies <- length(unique(data_all$aid))

#List of data headings
head(data_all,0)

#########Geographies: IN PROGRESS

# P: Geographies (aka intervention location)


################DANA IN PROGRESS

#######
# 1st step, clean up coutry columns and lists

###MAKING COUNTRY COLUMNS

#### Add country list
cty <- country$Country

# What David did for habitat:
# come back to

# Add habitat fields
unique(data_all$Habitat.type) 
data_all <- data_all %>% 
  mutate(coral=ifelse(grepl("Coral",Habitat.type,ignore.case = T),1,0),
         seagrass=ifelse(grepl("Seagrass",Habitat.type,ignore.case = T),1,0),
         mangrove=ifelse(grepl("mangrove",Habitat.type,ignore.case = T),1,0),)

# check habitat coding
unique(data_all$Habitat.type[data_all$coral==1])
unique(data_all$Habitat.type[data_all$seagrass==1])
unique(data_all$Habitat.type[data_all$mangrove==1])

habitat.test <- data_all %>% 
  filter(coral==0 & mangrove==0 & seagrass==0) %>% 
  select(Habitat.type)
unique(habitat.test$Habitat.type) # should just be "other"

###


#############

# Continent
data_all %>%
  group_by(Continent) %>% 
  summarise(num=n_distinct(aid), pct=num/num.studies*100)  %>%
  arrange (desc(num))  # this arranges them in descending order 
##### Need to do the "search for term within a row" method here                                  

# Country
data_all %>%
  group_by(study.ctry) %>% 
  summarise(num=n_distinct(aid), pct=num/num.studies*100)  %>%
  arrange (desc(num))  # this arranges them in descending order 
  # arrange(num)
##### Need to do the "search for term within a row" method here   

# Scale of Study
data_all %>%
  group_by(Scale.of.study) %>% 
  summarise(num=n_distinct(aid), pct=num/num.studies*100)  %>%
  arrange (desc(num))  # this arranges them in descending order 
##### Need to do the "search for term within a row" method here


##
# Add habitat fields
unique(data_all$Habitat.type) 
data_all <- data_all %>% 
  mutate(coral=ifelse(grepl("Coral",Habitat.type,ignore.case = T),1,0),
         seagrass=ifelse(grepl("Seagrass",Habitat.type,ignore.case = T),1,0),
         mangrove=ifelse(grepl("mangrove",Habitat.type,ignore.case = T),1,0),)

# check habitat coding
unique(data_all$Habitat.type[data_all$coral==1])
unique(data_all$Habitat.type[data_all$seagrass==1])
unique(data_all$Habitat.type[data_all$mangrove==1])

habitat.test <- data_all %>% 
  filter(coral==0 & mangrove==0 & seagrass==0) %>% 
  select(Habitat.type)
unique(habitat.test$Habitat.type) # should just be "other"
##


# Geographic Scale of Intervention
data_all %>%
  group_by(Geographic.scale.of.intervention) %>% 
  summarise(num=n_distinct(aid), pct=num/num.studies*100)  %>%
  arrange (desc(num))  # this arranges them in descending order 
##### Need to pinpoint NAs, L vs L for local



# P: Habitats

# Coral
data_all %>%
  filter(coral==1) %>% 
  summarise(num=n_distinct(aid), pct=num/num.studies*100)

# Seagrass
data_all %>%
  filter (seagrass==1) %>% 
  summarise(num=n_distinct(aid), pct=num/num.studies*100) 

# Mangrove
data_all %>%
  filter (mangrove==1) %>% 
  summarise(num=n_distinct(aid), pct=num/num.studies*100) 

# Note: these DO contain overlap, ie. some studies have multiple habitats:
data_all %>%
  group_by(Habitat.type) %>% 
  summarise(num=n_distinct(aid), pct=num/num.studies*100)  %>%
  arrange (desc(num)) 

# How many studies took place in only one of three habitats?
data_all %>% 
  filter(coral + seagrass + mangrove == 1) %>% 
  summarise(num=n_distinct(aid), pct=num/num.studies*100)

# How many studies took place in multiple habitats? (2 or more)
  #doesn't inlcude "other"
data_all %>% 
  filter(coral + seagrass + mangrove >= 2) %>% 
  summarise(num=n_distinct(aid), pct=num/num.studies*100)

# How many studies took place in all 3 habitats?
data_all %>% 
  filter(coral + seagrass + mangrove == 3) %>% 
  summarise(num=n_distinct(aid), pct=num/num.studies*100)

# What are the "other" studies that don't define a habitat?
  unique(data_all$aid[data_all$Habitat.type=="Other"])
  # ONLY for the habitat types that are ONLY other, aka doesn't include 
    # studies that have a "real" habitat and an "Other"

  
  
# I: Interventions
data_all %>%
  group_by(Int_cat) %>% 
  summarise(num=n_distinct(aid), pct=num/num.studies*100)  %>%
  arrange (desc(num))  # arranges them in descending order 

  # Highest: % in cons desig and planning, species management, and 
    # research and monitoring 
data_all %>%  
  filter(Int_cat== "2. Species management"| 
           Int_cat== "6. Conservation designation & planning"| 
           Int_cat== "6. Conservation designation & Planning"|
           Int_cat== "8. Research & monitoring") %>%  
  summarise(num=n_distinct(aid), pct=num/num.studies*100)

  # Lowest: % in Enforcement and Protection, awareness raising, and 
    # Education and Training
data_all %>%  
  filter(Int_cat== "4. Enforcement & protection"| 
           Int_cat== "3. Awareness raising"| 
           Int_cat== "9. Education & training") %>%  
  summarise(num=n_distinct(aid), pct=num/num.studies*100)

  # Subcategories (want % of each):
data_all %>%
  group_by(Intervention.subcategory) %>% 
  summarise(num=n_distinct(aid), pct=num/num.studies*100)  %>%
  arrange (desc(num))  # arranges them in descending order 
  # arrange (num) # arranges in ascending order, swap



#########comparators: IN PROGRESS

# Discuss w David: what do we want from this section/how in depth?
# Still playing around

# C: Comparators, Study Designs Used

  # Primary vs Secondary Data
data_all %>%
  group_by(Primary.or.secondary.data.) %>% 
  summarise(num=n_distinct(aid), pct=num/num.studies*100)  %>%
  arrange (desc(num))  # this arranges them in descending order 

  # Study Design
data_all %>%
  group_by(Study.design) %>% 
  summarise(num=n_distinct(aid), pct=num/num.studies*100)  %>%
  arrange (desc(num))  # this arranges them in descending order 
##### Need to do the "search for term within a row" method here

  # Type of Data
data_all %>%
  group_by(What.type.of.data.does.this.study.consider.) %>% 
  summarise(num=n_distinct(aid), pct=num/num.studies*100)  %>%
  arrange (desc(num))  # this arranges them in descending order 
##### Need to do the "search for term within a row" method here

  # Comparator Type
data_all %>%
  group_by(Comparator.type) %>% 
  summarise(num=n_distinct(aid), pct=num/num.studies*100)  %>%
  arrange (desc(num))  # this arranges them in descending order 
  # arrange (num) # for ascending order, swap
##### Need to do the "search for term within a row" method here

  # Does the Study population considers human groups?
data_all %>%
  group_by(Does.the.study.explicitly.examine.impacts.on.different.human.groups.) %>% 
  summarise(num=n_distinct(aid), pct=num/num.studies*100)  %>%
  arrange (desc(num))  # this arranges them in descending order 

#Find and look at the ONE NA Study here... what are the notes on this?
quick.stats <- data_all %>% 
         filter(is.na(Does.the.study.explicitly.examine.impacts.on.different.human.groups.))
unique(quick.stats$aid)
    # way to filter it without looking at data/one line, no quick.stats:
    # unique(data_all %>% 
      # filter(is.na(Does.the.study.explicitly.examine.impacts.on.different.human.groups.)) %>% 
      # select(aid))
    # non-piping way to do it:
      # unique(data_all$aid[is.na(data_all$Does.the.study.explicitly.examine.impacts.on.different.human.groups)])

  # Note: This 1 NA is tech number 205, so there must be a duplicate that got in
  # One section has both an N and a Y...
###########How do I ID which one has an overlap?

  # If so, what types?
  # NOT worth analysis, most of these are NAs:
    # data_all %>%
    #  group_by(If.so..what.types.of.different.groups.) %>% 
    #  summarise(num=n_distinct(aid), pct=num/num.studies*100)  %>%
    #  arrange (desc(num))  # this arranges them in descending order 


# O: Outcomes:

data_all %>%
  group_by(Outcome_cat) %>% 
  summarise(num=n_distinct(aid), pct=num/num.studies*100)

  # % Pop/Species and Ecological Community combined (no overlaps):
data_all %>%  
  filter(Outcome_cat== "2. Population/species"| 
         Outcome_cat==  "3. Ecological community") %>%  
  summarise(num=n_distinct(aid), pct=num/num.studies*100)

  # % in human well-being, knowledge and behavior, and governance
data_all %>%  
  filter(Outcome_cat== "6. Human well-being"| 
          Outcome_cat== "1. Knowledge and behavior"| 
          Outcome_cat== "7. Governance") %>%  
  summarise(num=n_distinct(aid), pct=num/num.studies*100)

  # % in ecosystem function and ecosystem service
data_all %>%  
  filter(Outcome_cat== "4. Ecosystem function"| 
          Outcome_cat==  "5. Ecosystem services") %>%  
  summarise(num=n_distinct(aid), pct=num/num.studies*100)
  
  # Subcategories (want % of each):
data_all %>% 
  group_by(Outcome.subcategory) %>% 
  summarise(num=n_distinct(aid), pct=num/num.studies*100) %>%
  arrange (desc(num))  # this arranges them in descending order 
  # arrange (num) # for ascending order, swap
  

#########time periods: IN PROGRESS

# Extra: Time Period:

  # % of studies after 2000
  # % of studies in the year:....
  # % of studies in [specific intervention area] during [specific time]
  
  # % of studies in top 2 interventions (cons desig and spec management) 
    # during [specific time]
  
  # could do ^ for outcome, could also try to get a chart that gives # of each
    # intervention and outcome for each year. Should be able to do this!



# make barplot
test.dat <- data_all %>%
            group_by(Outcome_cat) %>% 
            summarise(num=n_distinct(aid), pct=num/num.studies)
ggplot(test.dates(Outcome_cat,num)) +
  geom_bar(stat = )
