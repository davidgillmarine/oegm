pacman::p_load(rio,hrbrthemes,viridis,revtools,janitor,tidyverse)
workdir <- "R:/Gill/research/oegm/"
#workdir <- gsub("git","data",getwd())
inputdir <- paste0(workdir,"tables/raw/overlap/")
plotdir <- paste0(workdir,"output/plots/")
today.date <- gsub("-","",Sys.Date())

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
