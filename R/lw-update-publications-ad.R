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

# FIXME: dat <- query_list_general(grants$grantSerialNumber)
# INPUT: grantsSerialNumber (grant unique ID)
# OUTPUT : a large dataframe
          # Each row is a publication
          # cols are pmid, doi, title, abstract, year, journal, entity_name (see entity name function),
          # authors, query (grantSerialNumber)
grantserialnumbers <- unlist(grants$grantSerialNumber)
dat <- query_pubmed(grantserialnumbers)



