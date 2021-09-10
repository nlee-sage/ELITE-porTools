#!/usr/bin/Rscript

#######################################################
##      Update AD Knowledge Portal Publications      ##
##                                                   ##
## Description:                                      ##
##   Query PubMed for publications and upload        ##
##   results to Synapse in the format required by    ##
##   the AD Knowledge Portal                         ##
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

library("dplyr")
library("optparse")
library("porTools")
## Required, but not fully loaded
## readr, reticulate, glue, easyPubMed, dccvalidator

## Setup -----------------------------------------------------------------------

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
# nolint end

## Synapse client and logging in
synapseclient <- reticulate::import("synapseclient")
syntab <- reticulate::import("synapseclient.table")
syn <- synapseclient$Synapse()
if(!is.na(opts$auth_token)) {
  syn$login(authToken = opts$auth_token)
} else {
  syn$login()
}

## Grab grants ---------------------------------------------------

grants <- syn$tableQuery(
  glue::glue(
    "SELECT \"Grant Number\", grantSerialNumber, Program ",
    "FROM {opts$grant_table}"
  )
)$asDataFrame()
## grantSerialNumber becomes the query
## Remove rows that have NaN or NA or empty string for the serial number
grants <- grants[!(grants$grantSerialNumber %in% c(NaN, NA, "")), ]

## Query and clean up --------------------------------------------

dat <- query_list_general(grants$grantSerialNumber)

dat <- dat %>%
  rename(grantSerialNumber = query)
## For some reason, grantSerialNumber isn't always a character
grants$grantSerialNumber <- as.character(grants$grantSerialNumber)
dat <- dplyr::right_join(grants, dat, by = "grantSerialNumber")
## Need to remove duplicates, but keep all grants and consortium
## Includes some renaming and dropping of columns
dat <- dat %>%
  group_by(pmid) %>%
  mutate(
    grant = glue::glue_collapse(unique(.data$`Grant Number`), ", ")
  ) %>%
  mutate(consortium = glue::glue_collapse(unique(.data$Program), ", ")) %>%
  select(!c(`Grant Number`, Program, grantSerialNumber)) %>%
  rename(pubmed_id = pmid, DOI = doi, Program = consortium) %>%
  distinct()

## Hacky cleaning
## Included in hacky_cleaning is conversion to ascii and removing html formatting
dat$title <- hacky_cleaning(dat$title)
dat$authors <- hacky_cleaning(dat$authors)
dat$journal <- hacky_cleaning(dat$journal)
dat$abstract <- hacky_cleaning(dat$abstract)
## Remove common, unallowed characters from entity name; includes hacky_cleaning
dat$entity_name <- remove_unacceptable_characters(dat$entity_name)

## Set up multi-annotation columns correctly
dat <- set_up_multiannotations(dat, "grant")
dat <- set_up_multiannotations(dat, "Program")

## Store publications --------------------------------------------

dat_list <- purrr::transpose(dat)
store_as_annotations(parent = opts$parent, dat_list)

## Force file view update
if (!is.na(opts$pub_table)) {
  syn$tableQuery(glue::glue("SELECT * FROM {opts$pub_table}"))
}
