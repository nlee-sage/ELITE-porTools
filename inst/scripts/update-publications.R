#!/usr/bin/Rscript

#######################################################
##      Update ELITE Portal Publications             ##
##                                                   ##
## Description:                                      ##
##   Query PubMed for publications and upload        ##
##   results to Synapse in the format required by    ##
##   the ELITE Portal                                ##
##                                                   ##
## Usage:                                            ##
##   Rscript update-publications-ad.R \              ##
##     --grant_table <synID> \                       ##
##     --parent <synID> \                            ##
##     [--auth_token <Synapse PAT>] \                ##
##     [--pub_table <synID>] \                       ##
##                                                   ##
#######################################################

## Libraries -------------------------------------------------------------------

librarian::shelf(
  optparse,
  rmarkdown,
  reticulate,
  janitor,
  dplyr,
  readr,
  stringr,
  reticulate,
  easyPubMed,
  synapser,
  httr,
  tidyr,
  dplyr
)

# nolint start
option_list <- list(
  make_option(
    "--auth_token",
    action = "store",
    default = NA,
    type = "character",
    help = "Synapse Personal Access Token. If not given, assumes local .synapseConfig."
  ),
  make_option(
    "--grant_table",
    action = "store",
    default = NA,
    type = "character",
    help = "Synapse synID for table with grants to query for. Requires columns `Grant Number`, `grantSerialNumber`, `Program`. Grants are queried by serial number."
  ),
  make_option(
    "--parent",
    action = "store",
    default = NA,
    type = "character",
    help = "Synapse synID of parent folder to store publication entities to."
  ),
  make_option(
    "--pub_table",
    action = "store",
    default = NA,
    type = "character",
    help = "Synapse synID of file view scoped to publication folder (`parent`)."
  )
)
opts <- parse_args(OptionParser(option_list = option_list))

# get the base working directory to make it work on others systems
base_dir <- gsub('vignettes', '', getwd())
source(glue::glue("{base_dir}/R/pubmed.R"))
source(glue::glue("{base_dir}/R/text-cleaning.R"))
source(glue::glue("{base_dir}/R/annotation.R"))
source(glue::glue("{base_dir}/R/global-hard-coded-variables.R"))

# Login to synapse
## Synapse client and logging in
synapseclient <- reticulate::import("synapseclient")
syntab <- reticulate::import("synapseclient.table")
syn <- synapseclient$Synapse()
if (!is.na(opts$auth_token)) {
  syn$login(authToken = opts$auth_token)
} else {
  syn$login()
}

## ----functions------------------------------------------------------------------------------------------------------------------------------------------------------------------------
hacky_cleaning <- function(text) {
  conv <- convert_to_ascii(text = text)
  conv <- remove_hmtl_formatting(text = conv)
  conv <- gsub("&amp;|amp;", "and", conv)
  conv <- gsub("&#39;|&quot;", "'", conv)
  conv <- gsub("&gt;", "Greater Than ", conv)
  conv <- gsub("[[:punct:]]+", "", conv)
  conv <- gsub("\\s+", " ", conv)
  conv <- str_trunc(text, width = 500)
  return(conv)
}


## ----vars, echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# table_id <- "syn51209786" # ELITE Portal Projects Table

# Gather list of grants from synapse
grants <-
  syn$tableQuery(glue::glue("SELECT grantNumber, program, name FROM {sid_projects_table}"))$asDataFrame()

# convert grant numbers into string
library(comprehenr)
grantNumbers <-
  to_list(for (g in grants$grantNumber)
    for (y in g)
      y)

# expand rows with multiple grantNumbers
grants$grantNumber <-
  purrr::map(grants$grantNumber, function(x) {
    paste(unlist(x), collapse = ",")
  })

grants <- grants %>%
  separate_rows(grantNumber)

## ----scrape pubmed ids from grant numbers---------------------------------------------------------------------------------------------------------------------------------------------
get_pub_details <- function(request_body) {
  # Make the POST request
  response <-
    POST(
      url = API_URL,
      headers = headers,
      body = request_body,
      encode = "json"
    )
  return (response)
}

process_response <- function(response) {
  if (response$status_code == 200) {
    # Success!
    data <- content(response, "parsed")

    df <- as.data.frame(do.call(rbind, data$results))

    return (df)
  }
}

# works for project Numbers instead of project serial numbers
# Set the API URL
API_URL <- "https://api.reporter.nih.gov/v2/publications/search"

# Set the headers
headers <- list(accept = "application/json",
                "Content-Type" = "application/json")

# Set the request body
request_body <- list(
  criteria = list(core_project_nums = grantNumbers),
  offset = 0,
  limit = 50,
  sort_field = "core_project_nums",
  sort_order = "desc"
)

# Make the POST request
response <-
  POST(
    url = API_URL,
    headers = headers,
    body = request_body,
    encode = "json"
  )

# Check the response status code
if (response$status_code == 200) {
  # Success!
  pmids <- list()

  # get results as dataframe
  pmids_temp <- process_response(response)

  data <- content(response, "parsed")

  total <- data$meta$total

  results <- process_response(response)

  pmids[[length(pmids) + 1]] <- results

  request_body$offset <- request_body$offset + request_body$limit

  while (nrow(results) > 0) {
    response <- get_pub_details(request_body)

    results <- process_response(response)

    # extend pmids list
    pmids[[length(pmids) + 1]] <- results

    # update offset in request
    request_body$offset <-
      request_body$offset + request_body$limit
  }
} else {
  # Something went wrong
  print("Error:", response$status_code)
}

# create dataframe with pmids
pmids_df <- do.call(rbind, pmids)

pmids_df <- pmids_df %>% rename('grantNumber' = 'coreproject')

# for joining
pmids_df$grantNumber <- as.character(pmids_df$grantNumber)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# one eternity later....
pmid_metadata <- pub_query(pmids_df$pmid)


## ----query----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# create complete dataset
dat <- dplyr::right_join(grants, pmids_df, by = "grantNumber")

dat$pmid <- as.character(dat$pmid)

dat <- dplyr::left_join(dat, pmid_metadata, by = "pmid")

# clean column names
dat <- janitor::clean_names(dat, "lower_camel")



## ----hacky----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Included in hacky_cleaning is conversion to ascii and removing html formatting
dat$year <- stringr::str_extract(dat$pubdate, "\\d{4}")
dat$year <- as.integer(dat$year)
dat$title <- hacky_cleaning(dat$title)
dat$authors <- hacky_cleaning(dat$authors)
dat$journal <- remove_unacceptable_characters(dat$fulljournalname)

# dat$abstract <- hacky_cleaning(dat$abstract)

# drop unnecessary columns
dat <- dat %>% select(-c('applid', 'result', 'pubdate'))

cat(
  'Total rows: ',
  nrow(dat),
  '\n',
  'Duplicates: ',
  sum(dat %>% duplicated()),
  '\n',
  'Rows after duplicate remove: ',
  nrow(dat) - sum(dat %>% duplicated())
)

# Need to remove duplicates, but keep all grants and consortium
# Includes some renaming and dropping of columns
dat <- dat %>%
  group_by(pmid) %>%
  mutate(grant = glue::glue_collapse(unique(.data$`grantNumber`), ", ")) %>%
  mutate(consortium = glue::glue_collapse(unique(.data$program), ", ")) %>%
  select(!c(grantNumber, program)) %>%
  rename(
    pubmed_id = pmid,
    DOI = doi,
    program = consortium,
    study = name
  ) %>%
  distinct()

dat <- dat %>% rename('pmid' = 'pubmed_id')
dat$entity_name <- make_entity_name(dat)
dat$Name <- make_entity_name(dat)


#Using rename()
dat <- dat %>% rename(
  "Authors" = "authors",
  "Journal" = "journal",
  "PubmedId" = "pmid",
  "Title" = "title",
  "Year" = "year",
  "Grant" = "grant",
  "Program" = "program",
)

# Remove common, unallowed characters from entity name; includes hacky_cleaning
dat$entity_name <- remove_unacceptable_characters(dat$entity_name)



## ----columns--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dat <- set_up_multiannotations(dat, "Grant")
dat <- set_up_multiannotations(dat, "Program")
dat <- set_up_multiannotations(dat, "Authors")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
store_as_annotations <- function(parent, list) {
  entity <- purrr::map(
    list,
    ~ synapseclient$File(
      path = glue::glue("http://doi.org/{.$DOI}"),
      name = .$entity_name,
      parent = parent,
      synapseStore = FALSE,
      annotations = .
    )
  )
  # entity
  purrr::map(entity, ~ syn$store(., forceVersion = FALSE))
}


## ----store, message=FALSE, echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
# parent = "syn51317180" # ELITE publications folder
dat_list <- purrr::transpose(dat)

# another eternity
store_as_annotations(parent = sid_pub_folder, dat_list)

## Force file view update
if (!is.na(opts$pub_table)) {
  syn$tableQuery(glue::glue("SELECT * FROM {opts$pub_table}"))
}
