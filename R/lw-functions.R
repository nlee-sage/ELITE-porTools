### REWORK
# dat <- query_list_general(grants$grantSerialNumber)
# FIXME: Use rentrez instead of easyPubMed
# FIXME: More useful messaging to the consol (what is happening while we wait?)
# FIXME: Move away from nested structure if possible. Hard to debug.

#' Query PubMed for Publication Metadata
#'
#' @param grant_serial_nums A vector of grant unique IDs.
#' @return A dataframe of all publications that correspond to provided grant serial numbers. Includes pmid, doi, title, abstract, year, journal, entity_name
#'
#' @export

query_pubmed <- function(grant_serial_nums) {
  # query pubmed using grant unique ID for the pmid of each publication associated with the grant
  pub_pmids_list <- grant_query(grant_serial_nums)
  # query pubmed using the pubmed ids collected above and parse for important metadata
  # output dataframe with important metadata
  metadata_df <- pub_query(pub_pmids_list)
  return(metadata_df)
}
#' Query Grant Serial Numbers
#'
#' @param grant_serial_nums A vector of grant unique IDs.
#' @param max Number of results to return. Maxes out at 99999.
#' @return A list of vectors of publication PubMed IDs for each grant serial number supplied.
grant_query <- function(grant_serial_nums, max = 99999) {
  # iterate over vector of grant serial numbers
  pub_pmids <- lapply(grant_serial_nums, function(uid) {
    message(paste0("Getting publication pmids for grant serial number: ", uid))
    # search pubmed by grant serial number
    # returns list of esearch objs with pubi
    search_res <- rentrez::entrez_search(db = "pubmed",
                                         term = paste0(uid, "[GRNT]"),
                                         retstart = 0,
                                         retmax = max)
    search_res$ids
    })

  # name list elements
  names(pub_pmids) <- grant_serial_nums

  # return list of publication pmids / grant id
  return(pub_pmids)
}

#' Query Publications PubMed IDs
#'
#' @param pub_pmids A list of vectors of pubmed IDs.
#' @return Vector of integer Pubmed IDs associated with the query.

# Takes a list of publication pmids and pulls metadata
# outputs: pmid, doi, title, abstract, year, journal, entity_name (see entity name function)
pub_query <- function(pub_pmids_list) {
  # for each element in pub_pmids get summary info for all pmids
  # returns a large list of summary information per
  pub_summary_list <- lapply(seq_along(pub_pmids_list), function(i) {
    message(paste0("fetching summary obj for ", names(pub_pmids_list)[[i]]))

    summary_obj_list <- tryCatch({
      rentrez::entrez_summary(db = "pubmed",
                              id = pub_pmids_list[[i]],
                              always_return_list = TRUE)
      }, error = function(e) {
        return(NA)
        })

    # parse summary object list
    # this function can handle NA input
    metadata_df <- suppressWarnings(parse_summary_obj_list(summary_obj_list))

  })

  # name pub_summary_list
  names(pub_summary_list) <- names(pub_pmids_list)

  # collapse list of dataframes into a single df
  bind_rows(pub_summary_list, .id = "grantserialnumber")
}

#' Parse Summary Obj
#'
#' @param summary_obj_list list of esummary records output from \code{`entrez_summary()`}
#' @return dataframe containing authors and doi for each summary object in the list.
#'

# TODO FIX WARNGING : In if (suppressWarnings(!is.na(summary_obj_list))) { :
# the condition has length > 1 and only the first element will be used

parse_summary_obj_list <- function(summary_obj_list) {
  # check that summary objec is not NA
  # pull out author doi
  if (!is.na(summary_obj_list)){

    # pull out author and doi (nested dataframes)
    author_doi_list <- lapply(seq_along(summary_obj_list), function(i) {

      message(paste0("parsing pub: ", summary_obj_list[[i]]["uid"]))

      summary_obj <- summary_obj_list[[i]]
      # get author df
      authors_df <- summary_obj[["authors"]]
      # if no authors provided, NA
      if (purrr::is_empty(authors_df)) {
        authors_string <- NA
      } else {
        # remove any rows that do not have authtype == "author"
        authors_df <- authors_df[grepl("Author", authors_df$authtype),]
        # collapse author names, separate by ,
        authors_string <- paste0(authors_df$name, collapse = ", ")
      }

      # get article IDs df (contains doi)
      articleids_df <- summary_obj[["articleids"]]
      doi_string <- articleids_df[grepl("doi", articleids_df$idtype), "value"]
      # check that doi_string is present
      if (purrr::is_empty(doi_string)) {
        doi_string <- NA
      }

      #return a dataframe with author string and doi
      return(tibble(authors = authors_string, doi = doi_string))
    })

    # unlist into a single dataframe
    author_doi_df <- purrr::map_df(author_doi_list, I)

    ## parse summary obj list for rest of metadata
    # pimd, pubdate, title, and fulljournalname
    summary_df <- summary_obj_list %>%
      tibble(
        pmid = purrr::map_chr(., "uid"),
        pubdate = purrr::map_chr(., "pubdate"),
        title = purrr::map_chr(., "title"),
        fulljournalname = purrr::map_chr(., "fulljournalname"),
      )

    # bind together summary dataframe and author/doi dataframe
    # remove first row of summary df
    out <- bind_cols(summary_df[,-1], author_doi_df)
  } else {
    out <- tibble(
      pmid = NA,
      pubdate = NA,
      title = NA,
      fulljournalname = NA,
      authors = NA,
      doi = NA
    )
  }

  return(out)
}

#' Make Entity Names
#' Set first author, journal, year and Pubmed ID as the Synapse entity name.
#'
#' @param df Data frame from \code{"easyPubMed::article_to_df()"}.
#' @export

# this function is from an earlier version of this script
make_entity_name <- function(df){
  first <- df[1,]
  # Entity names must be < 256 characters; shorten author, journal section
  # Need to leave space for year and pubmed ID
  # Arbitrarily set to 200 characters
  short_name <- stringr::str_trunc(
    glue::glue("{first$lastname} {first$journal}"),
    width = 200
  )
  name <- glue::glue("{short_name} {first$year} (Pubmed ID {first$pmid})")
  df <- dplyr::mutate(df, entity_name = name)
  df
}
