#!/usr/bin/Rscript

#######################################################
##      Update AD Knowledge Portal Publications      ##
##                                                   ##
## Usage:                                            ##
##   update-publications-ad.R -d <directory>         ##
##                                                   ##
## Arguments:                                        ##
##   directory - the directory to store log files in ##
#######################################################

## Libraries -------------------------------------------------------------------

suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("log4r"))
suppressPackageStartupMessages(library("lubridate"))
suppressPackageStartupMessages(library("optparse"))
suppressPackageStartupMessages(library("porTools"))
## Required, but not fully loaded
## readr, reticulate, glue, easyPubMed, dccvalidator

## Helper function to store task annotation ------------------------------------

#' Update task annotation
#'
#' Update the 'completed_successfully' task annotation on a Synapse folder.
#' Expected that the annotation is a text type.
#'
#' @param syn Synapse client object
#' @param annots Synapse annotation object for the task folder
#' @param success TRUE if the task was completed successfully, else FALSE
update_task_annotation <- function(syn, annots, success) {
  annots['completed_successfully'] <- success
  syn$set_annotations(annots)
}

## Setup -----------------------------------------------------------------------

option_list <- list(
  make_option(
    c("-d", "--directory"),
    action = "store",
    default = NA,
    type = "character",
    help = "Directory to store log files in."
  )
)
opts <- parse_args(OptionParser(option_list = option_list))

## Create logger
## Make sure a directory was given!
if (!is.na(opts$directory)) {
  logfile_name <- glue::glue("{year(today())}-{month(today())}")
  logpath <- glue::glue("{opts$directory}/{logfile_name}.log")
  logger <- create.logger(logfile = logpath, level = "INFO")
} else {
  print("No directory for storing logs given. Retry with -d <directory>.")
}

## Synapse client and logging in
synapseclient <- reticulate::import("synapseclient")
syntab <- reticulate::import("synapseclient.table")
syn <- synapseclient$Synapse()
tryCatch(
  {
    syn$login()
  },
  error = function(e) {
    failure_message <- glue::glue(
      "Log in error:\n  {e$message}"
    )
    error(logger, failure_message)
    quit(status = 1)
  }
)

## Hard-coded variables ------------------------------------------

## Parent folder to store publication entities to
parent = "syn20463015"
## Table with grants to query
## Requires columns `Grant Number`, `grantSerialNumber`, `Program`
table_id <- "syn17024229"
## File view scoped to the parent folder
pub_table <- "syn20448807"
## Folder object in Synapse for tracking success/fail
task_folder <- "syn25582553"

task_annots <- tryCatch(
  {
    syn$get_annotations(task_folder)
  },
  error = function(e) {
    # Assuming error might be from not being logged in --
    # in this case, should exit now instead of doing more work that will fail;
    # however, this might not be the problem and too extreme of a response.
    failure_message <- glue::glue(
      "Could not pull annotations from task folder:\n  {e$message}"
    )
    error(logger, failure_message)
    quit(status = 1)
  }
)

## Grab grants ---------------------------------------------------

grants <- tryCatch(
  {
    syn$tableQuery(
      glue::glue(
        "SELECT \"Grant Number\", grantSerialNumber, Program FROM {table_id}"
      )
    )$asDataFrame()
  },
  error = function(e) {
    failure_message <- glue::glue(
      "Failed at querying grant table:\n  {e$message}"
    )
    error(logger, failure_message)
    update_task_annotation(syn = syn, annots = task_annots, success = "false")
    quit(status = 1)
  }
)
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
dat <- tryCatch(
  {
    dat %>%
      group_by(pmid) %>%
      mutate(grant = glue::glue_collapse(unique(.data$`Grant Number`), ", ")) %>%
      mutate(consortium = glue::glue_collapse(unique(.data$Program), ", ")) %>%
      select(!c(`Grant Number`, Program, grantSerialNumber)) %>%
      rename(pubmed_id = pmid, DOI = doi, Program = consortium) %>%
      distinct()
  },
  error = function(e) {
    failure_message <- glue::glue(
      "Failed to tidy data -- something is wrong:\n  {e$message}"
    )
    error(logger, failure_message)
    update_task_annotation(syn = syn, annots = task_annots, success = "false")
    quit(status = 1)
  }
)

## Hacky cleaning
## Included in hacky_cleaning is conversion to ascii and removing html formatting
dat$title <- hacky_cleaning(dat$title)
dat$authors <- hacky_cleaning(dat$authors)
dat$journal <- hacky_cleaning(dat$journal)
dat$abstract <- hacky_cleaning(dat$abstract)
## Remove common, unallowed characters from entity name; includes hacky_cleaning
dat$entity_name <- remove_unacceptable_characters(dat$entity_name)

## Add columns that need to exist for now; should remove later
dat["long_amp_ad_grants"] <- dat$grant
dat["doi"] <- dat$DOI
dat["consortium"] <- dat$Program

## Set up multi-annotation columns correctly
dat <- set_up_multiannotations(dat, "grant")
dat <- set_up_multiannotations(dat, "Program")

## Store publications --------------------------------------------

dat_list <- purrr::transpose(dat)
tryCatch(
  {
    store_as_annotations(parent = parent, dat_list)
  },
  error = function(e) {
    failure_message <- glue::glue(
      "Failed to store all of the publications:\n  {e$message}"
    )
    error(logger, failure_message)
    update_task_annotation(syn = syn, annots = task_annots, success = "false")
    quit(status = 1)
  }
)

## Force file view update
syn$tableQuery(glue::glue("SELECT * FROM {pub_table} LIMIT 1"))

## Win -----------------------------------------------------------

info(logger, "Publications updated without incident")
update_task_annotation(syn = syn, annots = task_annots, success = "true")
quit(status = 0)
