##Optimizing search strings

#Install packages
devtools::install_github("elizagrames/litsearchr")

#Load libraries
library(tidyr)
library(litsearchr)
library(dplyr)

#Experimental set up
#Compare search term selection between original included vs. excluded, test library, review included vs. excluded (3 total tests, 5 runs)

#Import datasets
search_directory <- "~/Documents/github/cons_hwb_2020/litsearchr/og_excluded/"
workdir <- "R:/Gill/research/oegm/"
search_directory <- paste0(workdir,"tables/raw/litsearchR")
outputdir <- paste0(workdir,"output/tables/")
today.date <- gsub("-","",Sys.Date())

naiveimport <- import_results(directory = search_directory, verbose=TRUE)
#naiveimport <- import_results(directory = paste0(search_directory, "/test"), file="OEGM-Set2_10_included_overlap.ris", verbose=TRUE)

#naiveresults <- remove_duplicates(naiveimport, field = "title", method = "string_osa")

naiveresults <- naiveimport

rakedkeywords <-
  litsearchr::extract_terms(
    text = paste(naiveresults$title, naiveresults$abstract),
    method = "fakerake",
    min_freq = 2,
    ngrams = TRUE,
    min_n = 2,
    language = "English"
  )

taggedkeywords <-
  litsearchr::extract_terms(
    keywords = naiveresults$keywords,
    method = "tagged",
    min_freq = 2,
    ngrams = TRUE,
    min_n = 2,
    language = "English"
  )

all_keywords <- unique(append(taggedkeywords, rakedkeywords))

naivedfm <-
  litsearchr::create_dfm(
    elements = paste(naiveresults$title, naiveresults$abstract),
    features = all_keywords
  )

naivegraph <-
  litsearchr::create_network(
    search_dfm = as.matrix(naivedfm),
    min_studies = 5,
    min_occ = 2
  )

cutoff <-
  litsearchr::find_cutoff(
    naivegraph,
    method = "cumulative",
    percent = .50,
    imp_method = "strength"
  )

reducedgraph <-
  litsearchr::reduce_graph(naivegraph, cutoff_strength = cutoff[1])

#searchterms <- litsearchr::get_keywords(reducedgraph)
searchterms <- sort(igraph::strength(reducedgraph), decreasing = TRUE) 

head(searchterms, 20)

write.csv(searchterms, str_c(outputdir,today.date,"_search_terms_reduced.csv"))
# manually group terms in the csv using a spreadsheet application

# read grouped CSV back in
#grouped_terms <- read.csv("./search_terms_grouped.csv")
# extract the grouped terms from the csv
#woodpecker_terms <- grouped_terms$term[grep("woodpecker", grouped_terms$group)]
# join together a list of manually generated woodpecker terms with the ones from the csv
#woodpeckers <- unique(append(c("woodpecker")), woodpecker_terms)
# repeat this for all concept groups


# then merge them into a list, using the code below as an example
mysearchterms <- list(woodpeckers, fire)
