#' Make Entity Names
#' Set first author, journal, year and Pubmed ID as the Synapse entity name.
#'
#' @param df Data frame from \code{"easyPubMed::article_to_df()"}.
#' @export
make_entity_name <- function(df){
  first <- df[1,]
  name <- glue::glue("{first$lastname} {first$journal} {first$year} (Pubmed ID {first$pmid})")
  name
}
#' Query PubMed for publication data
#' Returns a data frame with one author per row. Error handling for
#' publications without an author list.
#' @param pmid PubmedId as a string.
#' @export
query_pm <- function(pmid) {
  structured_pmid_query <- easyPubMed::get_pubmed_ids(pmid)
  text_blob <- easyPubMed::fetch_pubmed_data(structured_pmid_query)
  df <- easyPubMed::article_to_df(text_blob)
  if (is.null(df)){
    df <- easyPubMed::article_to_df(text_blob, getAuthors = FALSE)
  }
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
    entry$authors <- str_trunc(entry$authors, width = 500)
  }
  entry
}
#' Wrapper for Pubmed Query
#' This function wraps several functions:
#' - query Pubmed
#' - create an entity name from first author, journal, year and Id
#' - abbreviates the author names by first initial, last name
#' - creates one row per PubmedId
#' @param vec A character vector of PudmedIds.
#' @export
query_list_pmids <- function(vec) {
  vec <- purrr::map(vec, ~ query_pm(.))
  vec <- purrr::map(vec, ~ make_entity_name(.))
  vec <- purrr::map(vec, ~ collapse_names(.))
  vec <- purrr::map(vec, ~ collapse_pm_query(.))
  df <- data.frame(Reduce(rbind, vec))
  df
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
  entity <- purrr::map(list, ~ synapseclient$File(path = .$DOI,
                                                  name = .$entity_name,
                                                  parent = parent,
                                                  synapseStore = FALSE,
                                                  annotations = .)
  )
  entity
  purrr::map(entity, ~ syn$store(.))
}
#' Format multi-value annotations
#' Reticulate and Python client can turn vectors into multiple value annotations.
#' @param df A data frame, the output of `query_list_pmids`
#' @param value The column name to parse for multiple value annotations.
#' @export
set_up_multiannotations <- function(df, value) {
  vec <- df[[rlang::as_string(value)]]
  vec <- stringr::str_split(vec, pattern = ",")
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
