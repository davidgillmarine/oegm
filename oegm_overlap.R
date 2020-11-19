pacman::p_load(rio,hrbrthemes,viridis,revtools,janitor,tidyverse)
workdir <- "R:/Gill/research/oegm/"
#workdir <- gsub("git","data",getwd())
inputdir <- paste0(workdir,"tables/raw/overlap/")
outputdir <- paste0(workdir,"output/tables/")
plotdir <- paste0(workdir,"output/plots/")
today.date <- gsub("-","",Sys.Date())
list.files(inputdir)
last.file <- function(dir.nam,nam){
  import(paste0(dir.nam,last(sort(grep(nam,list.files(dir.nam), value=T)))))
}

# Review overlap
excl_ovrlp <- read_bibliography(paste0(inputdir,"OEGM-Set1_10_excluded_overlap.ris"))
incl_ovrlp <- read_bibliography(paste0(inputdir,"OEGM-Set2_10_included_overlap.ris"))
review.ovrlp <- incl_ovrlp %>% 
  mutate(incl.excl="Included") %>% 
  select(label,title,abstract,year,author,doi,journal,keywords,incl.excl) %>% 
  bind_rows(excl_ovrlp %>% 
              mutate(incl.excl="Excluded") %>% 
              select(label,title,abstract,year,author,doi,journal,keywords,incl.excl)) 

# Deduplicated 10%
#excl_unique <- read_bibliography(paste0(inputdir,"OEGM-Set3_10_excluded_unique.ris"))
#incl_unique <- read_bibliography(paste0(inputdir,"OEGM-Set4_10_included_unique.ris"))

review.ovrlp %>% 
  summarise(across(everything(), n_distinct))

# generate unique id for each citation
review.ovrlp <- review.ovrlp %>% 
  mutate(across(c(title,abstract),list(z=~replace_na(.,"")))) %>%  # replace NAs with "" (for combining strings)
  mutate(across(c(title_z,abstract_z),~str_to_lower(.))) %>%   # lower case
  mutate(across(c(title_z,abstract_z),~str_replace_all(.,"\\W", ""))) %>% # remove anything that isn't a word character
  mutate(rec.id=str_c(str_sub(title_z,1,5),
                      year,
                      str_sub(abstract_z,1,5),
                      sep="_")) %>% 
  select(-c(title_z,abstract_z))

review.ovrlp$rec.id[1:20]

# ID duplicates
nrow(review.ovrlp)==length(unique(review.ovrlp$rec.id))
View(review.ovrlp %>% get_dupes(rec.id))

# Remove duplicates (check first!!!)
review.ovrlp <- review.ovrlp %>% 
  distinct(rec.id,.keep_all = T)


#----- Colandr export
clndr.export <- import(paste0(inputdir,"OESM Screening_Fall 2020_10percent_review_overlap_colandr_export.csv")) %>% 
  select(study_id,
         title=citation_title,
         abstract=citation_abstract,
         year=citation_pub_year,
         author=citation_authors,
         journal=citation_journal_name,
         keywords=citation_keywords,
         screen.status=citation_screening_status)

clndr.export %>% 
  summarise(across(everything(), n_distinct))

# generate unique id for each citation
clndr.export <- clndr.export %>% 
  mutate(across(c(title,abstract),list(z=~replace_na(.,"")))) %>%  # replace NAs with "" (for combining strings)
  mutate(across(c(title_z,abstract_z),~str_to_lower(.))) %>%   # lower case
  mutate(across(c(title_z,abstract_z),~str_replace_all(.,"\\W", ""))) %>% # remove anything that isn't a word character
  mutate(rec.id=str_c(str_sub(title_z,1,5),
                      year,
                      str_sub(abstract_z,1,5),
                      sep="_")) %>% 
  select(-c(title_z,abstract_z))

clndr.export$rec.id[1:20]

# finish screening
clndr.export$title[grepl("rebui_2010",clndr.export$rec.id)]
clndr.export$screen.status[grepl("rebui_2010",clndr.export$rec.id)] <- "excluded"
clndr.export$title[grepl("marin_2007",clndr.export$rec.id)]
clndr.export$screen.status[grepl("marin_2007",clndr.export$rec.id)] <- "included"
clndr.export$title[grepl("trade_2009",clndr.export$rec.id)]
clndr.export$screen.status[grepl("trade_2009",clndr.export$rec.id)] <- "included"
table(clndr.export$screen.status)

# ID duplicates
nrow(clndr.export)==length(unique(clndr.export$rec.id))
View(clndr.export %>% get_dupes(rec.id))

# Remove duplicates (check first!!!)
clndr.export <- clndr.export %>% 
  distinct(rec.id,.keep_all = T)

# -- combine the two
## Check
# test1 <- review.ovrlp %>% 
#   anti_join(clndr.export,by="rec.id") %>% 
#   select(rec.id,title,abstract,author)
# test2 <- clndr.export %>% 
#   anti_join(review.ovrlp,by="rec.id") %>% 
#   select(rec.id,title,abstract,author)
# nrow(test1)==0 # true?
# nrow(test2)==0 # true?

# all 0s?
review.ovrlp$rec.id[!review.ovrlp$rec.id%in%clndr.export$rec.id]
clndr.export$rec.id[!clndr.export$rec.id%in%review.ovrlp$rec.id]

# names line up?
comb.ovrlp.check <- review.ovrlp %>% 
  left_join(clndr.export,by="rec.id") %>% 
  select(rec.id,author.x,author.y,title.x,title.y,year.x,year.y,abstract.x,abstract.y)

View(comb.ovrlp.check)
comb.ovrlp.check %>% 
  summarise(across(everything(), n_distinct))

# combine datasets
comb.ovrlp <- review.ovrlp %>% 
  left_join(clndr.export,by="rec.id") %>% 
  select(rec.id,author=author.x,title=title.x,year=year.x,keywords=keywords.x,incl.excl,screen.status)
table(comb.ovrlp$incl.excl,comb.ovrlp$screen.status)

# Plot
comb.ovrlp %>%
  filter(screen.status != "conflict") %>%
  group_by(incl.excl, screen.status) %>%
  summarise(num = n()) %>%
  ggplot(aes(x = incl.excl, y = screen.status, fill = num)) +
  geom_tile(color = "white", size = 0.1) +
  geom_text(aes(label = num), show.legend = F) +
  scale_fill_gradient2(low = "lightblue",high = "blue",name = "# Cases",na.value = "gray90",guide = FALSE) +
  coord_equal() +
  labs(x = "Previous screened group", y = "Current screening") +
  theme(legend.position = "none") +
  scale_x_discrete(position = "top") +
  scale_y_discrete(position = "right") +
  theme_minimal()

ggsave(str_c(plotdir,today.date,"_review_overlap.png"),width=6,height=4)

# Mismatch betwene previous and current screening
mismatch <- comb.ovrlp %>% 
  filter(incl.excl=="Included" &  screen.status=="excluded")
export(mismatch,str_c(outputdir,today.date,"_mismatch.csv"))

#-----# 90% overlap fix  Colandr export 
# large set of papers were missing abstracts. Need to export and import them into Mendeley
e90_ovrlp.colandr.export <- import(paste0(inputdir,"OESM Screening_Fall 2020_90percent_review_overlap_tmp.csv"))
sapply(e90_ovrlp.colandr.export, function(x) sum(is.na(x))/length(x))
e90_ovrlp.no.abstract <- filter(e90_ovrlp.colandr.export,citation_abstract=="") %>%
  rename(title=citation_title,
         abstract=citation_abstract,
         year=citation_pub_year,
         author=citation_authors,
         journal=citation_journal_name,
         keywords=citation_keywords) 
  
write_bibliography(e90_ovrlp.no.abstract,str_c(outputdir,today.date,"_OEGM_90_Overlap_Reviews_no_abstract.ris")) # export to try to find missing abstracts


e90_ovrlp.no.abstract.update <- read_bibliography(paste0(outputdir,"20201113_OEGM_90_Overlap_Reviews_no_abstract_update.ris")) %>% 
  mutate(title_z=str_replace_all(title,"\\W", ""))
sapply(e90_ovrlp.no.abstract.update, function(x) sum(is.na(x))/length(x)) # should only be a handful

# e90.fix <- e90_ovrlp.colandr.export %>% 
#   filter(citation_abstract=="") %>% 
#   mutate(title_z=str_replace_all(citation_title,"\\W", "")) %>% 
#   left_join(select(e90_ovrlp.no.abstract.update,title,abstract),by=c("citation_title"="title")) %>% 
#   mutate(citation_abstract=ifelse(citation_abstract=="",abstract,citation_abstract)) %>% 
#   select(-abstract)
# sapply(e90.fix, function(x) sum(is.na(x))/length(x))
# export(e90_ovrlp.no.abstract.update,str_c(outputdir,today.date,"_OEGM_90_Overlap_Reviews_fix.csv")) # export to try to find missing abstracts
# e90.fix$citation_abstract[e90.fix$study_id==2458332]
# filter(e90.fix,is.na(citation_abstract)) %>% view
# oegm full map

# --- HWB remaining studies
# Additional citations screened by Cheng for their updated HWB map
hwb.screen <- import(paste0(inputdir,"2044_2020-11-12_Excluded_Marine_FROM_cons-hwb-backwards.csv")) %>% 
  bind_rows(import(paste0(inputdir,"2044_2020-11-12_Included_Marine_FROM_cons-hwb-backwards.csv"))) %>% 
  rename(year=publication_year) %>% 
  mutate(across(c(title,abstract),list(z=~replace_na(.,"")))) %>%  # replace NAs with "" (for combining strings)
  mutate(across(c(title_z,abstract_z),~str_to_lower(.))) %>%   # lower case
  mutate(across(c(title_z,abstract_z),~str_replace_all(.,"\\W", ""))) %>% # remove anything that isn't a word character
  mutate(rec.id=str_c(str_sub(title_z,1,10),
                      year,
                      str_sub(abstract_z,1,15),
                      sep="_"))
sapply(hwb.screen, function(x) sum(is.na(x))/length(x))

oegm <- last.file(outputdir,"_oegm_all.rds") %>% 
  mutate(across(c(title,abstract),list(z=~replace_na(.,"")))) %>%  # replace NAs with "" (for combining strings)
  mutate(across(c(title_z,abstract_z),~str_to_lower(.))) %>%   # lower case
  mutate(across(c(title_z,abstract_z),~str_replace_all(.,"\\W", ""))) %>% # remove anything that isn't a word character
  mutate(rec.id=str_c(str_sub(title_z,1,10),
                      year,
                      str_sub(abstract_z,1,15),
                      sep="_"))
oegm.90 <- filter(oegm,batch=="unique_90")

e90_ovrlp.colandr.export <- e90_ovrlp.colandr.export %>% 
  mutate(across(c(title,abstract),list(z=~replace_na(.,"")))) %>%  # replace NAs with "" (for combining strings)
  mutate(across(c(title_z,abstract_z),~str_to_lower(.))) %>%   # lower case
  mutate(across(c(title_z,abstract_z),~str_replace_all(.,"\\W", ""))) %>% # remove anything that isn't a word character
  mutate(rec.id=str_c(str_sub(title_z,1,10),
                      year,
                      str_sub(abstract_z,1,15),
                      sep="_"))

# Get papers that overlap with our 90% but aren't already in the previous sets  
hwb.screen.ovrlp <- hwb.screen %>% 
  filter(title_z%in%oegm.90$title_z & !(title_z%in%e90_ovrlp.colandr.export$title_z)) %>% 
  select(title:doi)
sapply(hwb.screen.ovrlp, function(x) sum(is.na(x))/length(x))
filter(hwb.screen.ovrlp,is.na(abstract)|abstract=="") %>% view
filter(hwb.screen.ovrlp,is.na(abstract)|abstract=="") %>% select(title)

write_bibliography(hwb.screen.ovrlp,str_c(outputdir,today.date,"_OEGM_90_Overlap_Reviews_remaining.ris")) # export to try to find missing abstracts

