#' Make Entity Names
#' Set first author, journal, year and Pubmed ID as the Synapse entity name.
#'
#' @param df Data frame from \code{"easyPubMed::article_to_df()"}.
#' @export
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
#' Query PubMed for publication data
#' Returns a data frame with one author per row. Error handling for
#' publications without an author list.
#' @param pmid PubmedId as a string.
#' @param sleep_time Amount of time, in seconds, to sleep between queries.
#' Without APIKey, Pubmed limits rate of queries to 3 per second. Default of
#' 0.35 seconds.
#' @export
query_pm <- function(pmid, sleep_time = 0.35) {
  structured_pmid_query <- easyPubMed::get_pubmed_ids(pmid)
  text_blob <- easyPubMed::fetch_pubmed_data(structured_pmid_query)
  df <- easyPubMed::article_to_df(text_blob)
  if (is.null(df)){
    df <- easyPubMed::article_to_df(text_blob, getAuthors = FALSE)
  }
  # Add a wait to get around rate limiting of 3 requests/sec
  Sys.sleep(sleep_time)
  df
}
#' Mutate author naming convention
#' `easyPubMed` returns complete names. This function preserves first initial and
#' last name.
#' @param df Data frame that is the output of `query_pm`.
#' @export
collapse_names <- function(df) {
  firstInitials <- strsplit(df$firstname, " ")
  firstInitials <- sapply(firstInitials, function(x){
    paste(substring(x, 1, 1), collapse = "")
  })

  df$initials <- firstInitials

  df <- df %>%
    mutate(fullname = paste0(lastname, " ", initials))
  df
}
#' Restructure Query
#' `easyPubMed` returns one row per author per publication. This function creates a
#' comma-separated list of authors, and one row per publication.
#' @inheritParams collapse_names
#' @export
collapse_pm_query <- function(df) {
  entry <- dplyr::group_by(df, pmid, doi, title, abstract, year, journal, entity_name)
  entry <- summarise(entry, authors = toString(fullname))
  if (nchar(entry$authors) > 500) {
    entry$authors <- stringr::str_trunc(entry$authors, width = 500)
  }
  entry
}
#' Wrapper for Pubmed Query using Pubmed IDs
#' This function wraps several functions:
#' - query Pubmed
#' - create an entity name from first author, journal, year and Id
#' - abbreviates the author names by first initial, last name
#' - creates one row per PubmedId
#' @param vec A character vector of PudmedIds.
#' @param sleep_time Amount of time, in seconds, to sleep between queries.
#' Without APIKey, Pubmed limits rate of queries to 3 per second. Default of
#' 0.35 seconds.
#' @export
query_list_pmids <- function(vec, sleep_time = 0.35) {
  vec <- purrr::map(vec, ~ query_pm(., sleep_time = sleep_time))
  vec <- purrr::map(vec, ~ make_entity_name(.))
  vec <- purrr::map(vec, ~ collapse_names(.))
  vec <- purrr::map(vec, ~ collapse_pm_query(.))
  df <- data.frame(Reduce(rbind, vec))
  df
}
#' Wrapper for Pubmed Query using general queries
#' This function wraps several functions:
#' - query Pubmed using `query_general`
#' - create an entity name from first author, journal, year and Id
#' - abbreviates the author names by first initial, last name
#' - creates one row per PubmedId
#' - query without associated PubmedIds will be left out of returned data
#' - query added as an additional column
#' @param queries A character vector of queries.
#' @param sleep_time Amount of time, in seconds, to sleep between queries.
#' Without APIKey, Pubmed limits rate of queries to 3 per second. Default of
#' 0.35 seconds.
#' @export
query_list_general <- function(queries, sleep_time = 0.35) {
  vec <- purrr::map(queries, ~ query_general(., sleep_time = sleep_time))
  # Limit queries to only those associated with pubmedIDs
  queries <- queries[!is.na(vec)]
  vec <- vec[!is.na(vec)]
  dfs <- purrr::map(vec, ~ query_list_pmids(.))
  dfs <- purrr::map2(dfs, queries, function (df, query) {
    tibble::add_column(df, query = query)
  })
  df <- Reduce(rbind, dfs)
  df
}
#' Query for Pubmed IDs only
#'
#' @param query Character string with query. The query can be simple
#' (e.g. "alzheimer disease") or complex
#' (e.g. "((alzheimer disease[TIAB]) AND (cat[TW]) AND (dog[TW]))". See the
#' PubMed guide for search field descriptions and tags:
#' https://pubmed.ncbi.nlm.nih.gov/help/#search-tags.
#' @param sleep_time Amount of time, in seconds, to sleep between queries.
#' Without APIKey, Pubmed limits rate of queries to 3 per second. Default of
#' 0.35 seconds.
#' @return Vector of integer Pubmed IDs associated with the query.
query_general <- function(query, sleep_time = 0.35) {
  pmids <- easyPubMed::get_pubmed_ids(query)
  # return NA if received nothing
  if(pmids$Count < 1) {
    return(NA)
  }
  # Add a wait to get around rate limiting of 3 requests/sec
  Sys.sleep(sleep_time)
  pmids <- easyPubMed::fetch_all_pubmed_ids(pmids)
  as.integer(pmids)
}
#' Store Pubmed Query output as annotations
#' This function is constrained by the `easyPubMed` output schema. DOI is stored as
#' the entity path; visualized as a hyperlink. The Synapse entity name is an
#' abbreviated entity name. emaining variables are non-specifically stored as
#' annotaitons.
#' @param parent Synapse Folder Id that will store the entities.
#' @param list List of publications.
#' @export
store_as_annotations <- function(parent, list) {
  entity <- purrr::map(list, ~ synapseclient$File(
    path = glue::glue("http://doi.org/{.$DOI}"),
    name = .$entity_name,
    parent = parent,
    synapseStore = FALSE,
    annotations = .
  ))
  # entity
  purrr::map(entity, ~ syn$store(., forceVersion = FALSE))
}
#' Format multi-value annotations
#' Reticulate and Python client can turn vectors into multiple value annotations.
#' @param df A data frame, the output of `query_list_pmids`
#' @param value The column name to parse for multiple value annotations.
#' @export
set_up_multiannotations <- function(df, value) {
  vec <- df[[rlang::as_string(value)]]
  vec <- stringr::str_split(vec, pattern = ", |,")
  df[[rlang::as_string(value)]] <- vec
  df
}
#' Get Synapse table
#'
#' Get the contents of a Synapse table as a data frame and preserve the row IDs.
#'
#' @param synID The Synapse ID of a table to query from. Defaults to
#'   "syn10242922"
#' @param syn Synapse client object
#' @return Data frame of table contents
#' @export
get_synapse_table_rows <- function(synID, syn) {
  query_result <- syn$tableQuery(
    glue::glue("select * from {synID}"),
    includeRowIdAndRowVersion = TRUE
  )
  dat <- utils::read.csv(
    query_result$filepath,
    na.strings = "",
    stringsAsFactors = FALSE
  )
  dat
}
