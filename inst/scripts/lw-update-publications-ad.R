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
library("rentrez")
library("purrr")
library("stringr")
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

## Grab grants ------------------------------------------------------

# qury synapse
grants <- syn$tableQuery(
  glue::glue(
    "SELECT \"Grant Number\", grantSerialNumber, Program ",
    "FROM {opts$grant_table}"
  )
)$asDataFrame()

# remove rows that have NaN or NA or empty string for the serial number
grants <- grants[!(grants$grantSerialNumber %in% c(NaN, NA, "")), ]

## Query PubMed -----------------------------------------------------

# unlist list of grant serial numbers into a vector
grant_serial_nums <- unlist(grants$grantSerialNumber)

# run all grant serial numbers through query pubmed
# returns a tibble
  # each row is a publication
  # columns include grantserialnumber, pubmed id, publication date, title, full journal name, doi, authors
dat <- query_pubmed(grant_serial_nums)

## Clean up ---------------------------------------------------------

# munge pubmed query results
# this function pulls out the year from pubdate and adds entity_name column
dat <- munge_pubmed(dat)

# For some reason, grantSerialNumber isn't always a character
grants$grantSerialNumber <- as.character(grants$grantSerialNumber)

# Join dat and grants table by grantSerialNumber
dat <- dplyr::right_join(grants, dat, by = "grantSerialNumber")

# Some pubmedIDs show up multiple times under different grants
# Need to capture this information in a single row of information so it isn't duplicated

dat <- dat %>%
  # for each pubmedID
  group_by(pmid) %>%
  mutate(
    # Create a new column that captures all grants that duplicate pmid is associated with
    grant = glue::glue_collapse(unique(.data$`Grant Number`), ", ")
  ) %>%
  # Create a new column that captures all programs that duplicate pmid is associated with
  mutate(consortium = glue::glue_collapse(unique(.data$Program), ", ")) %>%
  # drop Grant Number, Program, and GrantSerialNumber cols
  select(!c(`Grant Number`, Program, grantSerialNumber)) %>%
  # rename some columns
  rename(pubmed_id = pmid, DOI = doi, Program = consortium, journal = fulljournalname) %>%
  # keep only distinct rows
  distinct()

# Hacky cleaning
## Included in hacky_cleaning is conversion to ascii and removing html formatting
dat$title <- hacky_cleaning(dat$title)
dat$authors <- hacky_cleaning(dat$authors)
dat$journal <- hacky_cleaning(dat$journal)

# Remove common, unallowed characters from entity name; includes hacky_cleaning
dat$entity_name <- remove_unacceptable_characters(dat$entity_name)

## Remove row of NA
# See this (https://github.com/Sage-Bionetworks/porTools/issues/10#issuecomment-1083441995) issue comment for more info
# TODO: Figure out why this is happening
# capture cases where pubmed ID is NA
idx <- is.na(dat$pubmed_id)
# only keep cases where pubmed ID is not NA
dat <- dat[!idx,]

# Set up multi-annotation columns correctly
dat <- set_up_multiannotations(dat, "grant")
dat <- set_up_multiannotations(dat, "Program")

## Store publications --------------------------------------------

dat_list <- purrr::transpose(dat)
store_as_annotations(parent = opts$parent, dat_list)

## Force file view update
if (!is.na(opts$pub_table)) {
  syn$tableQuery(glue::glue("SELECT * FROM {opts$pub_table}"))
}
