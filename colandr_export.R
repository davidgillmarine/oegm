##Development script

##Install needed packages
install.packages('RPostgreSQL')
install.packages('devtools')
install.packages('remotes')
install.packages('RPostgres')

#Load packages
library('RPostgreSQL')
library('devtools')
library('remotes')
library('RPostgres')
library('shiny')
library('DBI')
library('dplyr')
library('tidyr')
library('ggplot2')
library('base')
library('plotly')
library('rcrossref')

#Establish connection to database
db <- 'colandr'
host_db <- 'test-ohio-restore.czafaha6ruxc.us-east-2.rds.amazonaws.com'
db_port <- '5432' ## Can determine db port by going to: https://console.aws.amazon.com/rds/ and then going to RDS Dashboard and click on Instances. Select the RDS Instance and check port number under Connectivity & Security
db_user <- 'colandr'
db_password <- 'percolatorsiftersieve'

con <- dbConnect(RPostgres::Postgres(), dbname=db,host=host_db,port=db_port,user=db_user,password=db_password)

#Grab user credentials
r_id <- '2575'
email <- 'cheng@nceas.ucsb.edu'

#Check user credentials - is email associated with the owner of the review
user_2_review <- dbGetQuery(con,paste0('SELECT users_to_reviews.user_id, users_to_reviews.review_id, users.email FROM users_to_reviews, users WHERE users_to_reviews.user_id=users.id AND review_id=',r_id))
if (email %in% user_2_review$email == TRUE){
  print('YAY') 
} else print('BOO')

#Run queries user screening progress
user_progress <- dbGetQuery(con, paste0('SELECT users.name, citation_screenings.user_id, count(citation_screenings.id) FROM users, citation_screenings WHERE citation_screenings.user_id=users.id AND review_id=',r_id,' GROUP BY users.id,citation_screenings.user_id;'))

#Run queries for number of included/excluded
screenings <- dbGetQuery(con, paste0('SELECT * FROM citation_screenings WHERE review_id=',r_id))
screenings <- left_join(screenings,user_progress,by="user_id") %>% select(-count)
#Calculate screened/unscreened
df <- screenings %>% select(id,last_updated,user_id,status,exclude_reasons,name) %>% arrange(last_updated)
df$total <- c(1:nrow(df))
df$included <- c("")

for (i in 1:nrow(df)){
  if (df$status[i]=='included'){
    df$included[i] <- 1
  } else
    df$included[i] <- 0
}

df$total_included <- cumsum(df$included)

##By user
df2 <- df %>% 
  select(id,last_updated,user_id,status,exclude_reasons,included,name) %>% 
  arrange(name,last_updated) %>% 
  group_by(name) %>% 
  mutate(total_included = cumsum(included)) %>%
  mutate(total = 1:n())

##Plotting inclusion rate (overall)
ggplot() +
  geom_line(data=df, aes(x=total,y=total_included), linetype='dashed') +
  geom_line(data=df2, aes(x=total, y=total_included, group=name, color=name)) +
  ylab("Total citations screened") +
  xlab("Total citations included") +
  theme(axis.title.x=element_text(size=14, face="bold"),
        axis.title.y=element_text(size=14, face="bold"),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10))

#Summarizing exclusion reasons @ title & abstract
df3 <- screenings %>% select(id,exclude_reasons)
o <- filter(screenings2,grepl("second op",tags))

#Run queries for tags
tags <- dbGetQuery(con, paste0('SELECT * FROM studies WHERE review_id=',r_id)) %>% select(id,tags,citation_status)
tags$tags <- gsub("\\{","",as.character(tags$tags))
tags$tags <- gsub("\\}","",as.character(tags$tags))
s <- strsplit(tags$tags, split=",")
d <- data.frame(ID=rep(tags$id, sapply(s, length)), tags = unlist(s))
d$tags <- tolower(d$tags)
colnames(d) <- c("id","tags")
d <- distinct(d)

tags_summary <- count(d,tags)

tag.names <- d %>% select(tags) %>% distinct() %>% filter(tags != "") %>% distinct()  %>% arrange(tags)

##Full data download

export <- dbGetQuery(con, paste0('SELECT * FROM studies LEFT JOIN citation_screenings ON studies.id=citation_screenings.citation_id LEFT JOIN citations ON studies.id=citations.id LEFT JOIN fulltext_screenings ON studies.id=fulltext_screenings.fulltext_id WHERE studies.review_id=',r_id)) %>% select(-last_updated, -review_id, -id..12,-last_updated..14,-review_id..15,-citation_id,-id..20,-created_at..21,-last_updated..22,-review_id..23,-journal_name,-text_content_vector_rep,-id..42,-last_updated..44,-review_id..45,-status..48,-fulltext_id)

colnames(export) <- c("id","date_uploaded","uploaded_by","tags","data_source_id","duplicate","citation_status","fulltext_status","data_extraction_status","date_screened_t&a","t&a_screened_by","t&a_status","t&a_exclusion_reasons","type_of_work","title","journal_name","abstract","publication_year","publication_month","authors","keywords","type_of_reference","volume","issue_number","doi","issn","publisher","language","other_fields","date_screened_fulltext","fulltext_screened_by","fulltext_exclusion_reasons")

pp <- export %>% slice(1:10)

##Developing citation list generator
library('rcrossref')

#Select included at title and abstract

#Find DOIs for papers that do not exist
bib <- export %>% select(id,"duplicate",citation_status,fulltext_status,title,doi,authors,journal_name) %>% distinct()
subset <- bib %>% filter(duplicate == "not_duplicate" & citation_status == "included") %>% distinct()

#####
subset <- subset %>% left_join(d,by="id")
sub1 <- subset %>% filter(tags == "conservation")
sub2 <- subset %>% filter(tags == "hwb" | tags == "behavior" | tags == "es" | tags == "process")
sub1 <- sub1 %>% select(id,tags)

####
sub_final <- semi_join(sub2,sub1,by="id")

#citations_filt <- subset[grep("conservation",subset$tags),]
#citations_filt2 <- citations_filt[grep("hwb",citations_filt$tags),]
sub_final$title <- tolower(sub_final$title)
sub_final <- distinct(sub_final)

missing_dois <- sub_final %>% filter(is.na(doi)) %>% select(-tags) %>% distinct()
dois <- sub_final %>% filter(!is.na(doi)) %>% select(-tags) %>% distinct()

#with no tags
missing_dois <- sub_final %>% filter(is.na(doi)) %>% distinct()
dois <- sub_final %>% filter(!is.na(doi)) %>% distinct()

##Find by title
ref_list <- purrr::map(missing_dois$title, function(x) {
  print(x)
  my_works <- rcrossref::cr_works(query=x, limit=1,.progess="time") %>% purrr::pluck("data")
})

reflist_df <- ref_list %>% purrr::map_dfr(., function(x) {
  x[1,]
})
reflist_df$title <- tolower(reflist_df$title)

citations_found <- semi_join(reflist_df,missing_dois,by="title")

#Set out list for manual search
citations_manual <- anti_join(missing_dois,reflist_df,by="title")

##find by DOI (add found from above)
a <- dois %>% select(doi)
b <- citations_found %>% select(doi)

final_doi <- bind_rows(a,b)

###for fulll title searching
a <- citations_manual %>% filter(!is.na(doi))
citations_manual <- citations_manual %>% filter(is.na(doi))
b <- citations_found %>% select(doi)

final_doi <- bind_rows(a,b)

# my_citations2 <- rcrossref::cr_cn(final_doi[,1], format = "ris") %>% purrr::map_chr(.,purrr::pluck, 1)

my_citations <- purrr::map(final_doi$doi, function(x) {
  my_works <- rcrossref::cr_cn(doi=x, limit=1,format = "ris",.progress="time") %>% purrr::map_chr(.,purrr::pluck, 1)
})

writeLines(as.character(my_citations), "my_citations_test.ris")
write.csv(citations_manual, "citations_for_manual_search.csv", row.names=FALSE)

##Import RIS into citation manager, use CSV to manually locate any remaining citations
