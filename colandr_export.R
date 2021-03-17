##Script to connect to colandr for Duke OEGM team

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
library('DBI')
library('ArgumentCheck')

##FUNCTION TO RUN TO GET REVIEW DATA FROM COLANDR - you need to give it a review_id (number), user email (with quotation marks around it)

colandr_get <- function(review_id,user_email){
  #Grab user credentials and desired review
  r_id <- review_id
  email <- tolower(paste0(user_email))
  
  #Establish connection to colandr database
  db <- 'colandr'
  host_db <- 'test-ohio-restore.czafaha6ruxc.us-east-2.rds.amazonaws.com'
  db_port <- '5432' ## Can determine db port by going to: https://console.aws.amazon.com/rds/ and then going to RDS Dashboard and click on Instances. Select the RDS Instance and check port number under Connectivity & Security
  db_user <- 'colandr'
  db_password <- 'percolatorsiftersieve'
  
  con <- dbConnect(RPostgres::Postgres(), dbname=db,host=host_db,port=db_port,user=db_user,password=db_password)
  
  #Check user credentials - is email associated with the owner of the review
  user_2_review <- dbGetQuery(con,paste0('SELECT users_to_reviews.user_id, users_to_reviews.review_id, users.email FROM users_to_reviews, users WHERE users_to_reviews.user_id=users.id AND review_id=',r_id))
  user_2_review$email <- tolower(user_2_review$email)
  
  check <- ArgumentCheck::newArgCheck()
  
  if (email %in% user_2_review$email == FALSE)
    ArgumentCheck::addError(msg = "Email is not associated with this review.", argcheck = check)
  
  ArgumentCheck::finishArgCheck(check)
  
  ##Full data download
  export <- dbGetQuery(con, paste0('SELECT * FROM studies LEFT JOIN citation_screenings ON studies.id=citation_screenings.citation_id LEFT JOIN citations ON studies.id=citations.id WHERE studies.review_id=',r_id)) %>% select(-last_updated, -review_id, -id..12,-last_updated..14,-review_id..15,-citation_id,-id..20,-created_at..21,-last_updated..22,-review_id..23,-journal_name,-text_content_vector_rep)
  
  colnames(export) <- c("id","date_uploaded","uploaded_by","tags","data_source_id","duplicate","citation_status","fulltext_status","data_extraction_status","date_screened_t&a","t&a_screened_by","t&a_status","t&a_exclusion_reasons","type_of_work","title","journal_name","abstract","publication_year","publication_month","authors","keywords","type_of_reference","volume","issue_number","doi","issn","publisher","language","other_fields")
  
  ##Disconnect from colandr database
  dbDisconnect(con)
  
  colandr_export <<- export
}
