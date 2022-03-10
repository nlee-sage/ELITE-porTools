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
  vec <- stringr::str_split(vec, pattern = ", |,| ,")
  vec <- purrr::map(vec, trimws)
  df[[rlang::as_string(value)]] <- vec
  df
}

##### lw note: is this function actually ever called? If not we could remove...

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
