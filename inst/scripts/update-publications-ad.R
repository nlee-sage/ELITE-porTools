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
##     --log_dir <synID> \                           ##
##     [--auth_token <Synapse PAT>] \                ##
##     [--pub_table <synID>] \                       ##
##     [--task_id <synID>] \                         ##
##     [--task_table <synID>]                        ##
##                                                   ##
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
#' @param task_view synID for the task file view (default: `NA`)
update_task_annotation <- function(syn, annots, success, task_view = NA) {
  annots['completed_successfully'] <- success
  syn$set_annotations(annots)
  ## Force file view update, if task_view is given
  if (!is.na(task_view)) {
    syn$tableQuery(glue::glue("SELECT * FROM {task_view}"))
  }
}

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
  ),
  make_option(
    "--log_dir",
    action = "store",
    default = NA,
    type = "character",
    help = "Directory to store log files in."
  ),
  make_option(
    "--task_id",
    action = "store",
    default = NA,
    type = "character",
    help = "Synapse synID of task entity for tracking success/fail."
  ),
  make_option(
    "--task_table",
    action = "store",
    default = NA,
    type = "character",
    help = "Synapse synID of file view scoped to include `task_id`."
  )
)
opts <- parse_args(OptionParser(option_list = option_list))
# nolint end

## Create logger
## Make sure directory exists; create if doesn't
if (!is.na(opts$log_dir)) {
  if (!dir.exists(opts$log_dir)) {
    dir.create(opts$log_dir)
  }
  logfile_name <- glue::glue("{year(today())}-{month(today())}")
  logpath <- glue::glue("{opts$log_dir}/{logfile_name}.log")
  logger <- create.logger(logfile = logpath, level = "INFO")
} else {
  stop("No log directory given. Retry with --log_dir <directory>.")
}

## Synapse client and logging in
synapseclient <- reticulate::import("synapseclient")
syntab <- reticulate::import("synapseclient.table")
syn <- synapseclient$Synapse()
tryCatch(
  {
    if(opts$auth_token) {
      syn$login(authToken = opts$auth_token)
    } else {
      syn$login()
    }
  },
  error = function(e) {
    failure_message <- glue::glue(
      "Log in error:\n  {e$message}"
    )
    error(logger, failure_message)
    quit(status = 1)
  }
)

## Grab annotations on task, if provided with task_id
## If not provided with task_id, don't update
update_task <- FALSE
task_annots <- NA
if (!is.na(opts$task_id)) {
  tryCatch(
    {
      task_annots <<- syn$get_annotations(opts$task_id)
      update_task <<- TRUE
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
}

## Grab grants ---------------------------------------------------

grants <- tryCatch(
  {
    syn$tableQuery(
      glue::glue(
        "SELECT \"Grant Number\", grantSerialNumber, Program ",
        "FROM {opts$grant_table}"
      )
    )$asDataFrame()
  },
  error = function(e) {
    failure_message <- glue::glue(
      "Failed at querying grant table:\n  {e$message}"
    )
    error(logger, failure_message)
    if (update_task) {
      update_task_annotation(
        syn = syn,
        annots = task_annots,
        success = "false",
        task_view = opts$task_table
      )
    }
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
      mutate(
        grant = glue::glue_collapse(unique(.data$`Grant Number`), ", ")
      ) %>%
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
    if (update_task) {
      update_task_annotation(
        syn = syn,
        annots = task_annots,
        success = "false",
        task_view = opts$task_table
      )
    }
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

## Set up multi-annotation columns correctly
dat <- set_up_multiannotations(dat, "grant")
dat <- set_up_multiannotations(dat, "Program")

## Store publications --------------------------------------------

dat_list <- purrr::transpose(dat)
tryCatch(
  {
    store_as_annotations(parent = opts$parent, dat_list)
  },
  error = function(e) {
    failure_message <- glue::glue(
      "Failed to store all of the publications:\n  {e$message}"
    )
    error(logger, failure_message)
    if (update_task) {
      update_task_annotation(
        syn = syn,
        annots = task_annots,
        success = "false",
        task_view = opts$task_table
      )
    }
    quit(status = 1)
  }
)

## Force file view update
if (!is.na(opts$pub_table)) {
  syn$tableQuery(glue::glue("SELECT * FROM {opts$pub_table}"))
}

## Win -----------------------------------------------------------

info(logger, "Publications updated without incident")
if (update_task) {
  update_task_annotation(
    syn = syn,
    annots = task_annots,
    success = "true",
    task_view = opts$task_table
  )
}
quit(status = 0)
