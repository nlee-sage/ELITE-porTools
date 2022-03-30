#' Convert special characters to ASCII
#'
#' @noRd
#' @param text String, or vector of strings, that might have special
#' characters
#' @return If the text has special characters, then the orignal
#' string will be returned with the special characters changed into ASCII
#' characters. If the text string has no special characters, then the
#' text string is returned.
convert_to_ascii <- function(text) {
  unlist(purrr::map(text, function(x) {
    conv <- iconv(x, from = 'UTF-8', to = 'ASCII//TRANSLIT')
    ## Will recieve NA back if there was nothing to convert
    if (!is.na(conv)) {
      return(conv)
    }
    return(x)
  }))
}
#' Remove hmtl formatting
#'
#' @noRd
#' @param text String, or vector of strings, that might have html
#' formatting
#' @return If the text string has hmtl formatting, then the
#' text will be returned with the html formatting removed. If the
#' text string does not have html formatting, then the
#' text string is returned.
remove_hmtl_formatting <- function(text) {
  gsub("<.*?>", "", text)
}
#' Remove unacceptable characters
#'
#' The primary goal for this function is to clean up entity names, which may
#' only contain: letters, numbers, spaces, underscores, hyphens, periods,
#' plus signs, apostrophes, and parentheses. Anything outside of this list is
#' considered an 'unacceptable character'. This will fix anything included in
#' [porTools::hacky_cleaning()].
#' Additionally, it will fix ':' (replaced with hyphen),
#' ';' (replaced with period), '/' (replaced with space), ',' (removed), and
#' '&amp;', '&', amp;' (all replaced with 'and').
#'
#' @export
#' @param text String, or vector of strings, that might have
#' unacceptable characters.
#' @return The text with replacement, if needed.
remove_unacceptable_characters <- function(text) {
  conv <- hacky_cleaning(text = text)
  conv <- gsub("&", "and", conv)
  conv <- gsub(":", "-", conv)
  conv <- gsub(";", ".", conv)
  conv <- gsub("/", " ", conv)
  conv <- gsub(",", "", conv)
  return(conv)
}
#' Clean up funky text
#'
#' Removes html formatting, converts non-ascii characters to ascii, and does
#' several replacements:
#' Replaces '&amp;', amp;' with 'and'. Replaces '&#39;' with an apostrophe.
#' Replaces '&quot;' with single quote.
#'
#' @export
#' @param text String to update
#' @return The text with replacement, if needed.
hacky_cleaning <- function(text) {
  conv <- convert_to_ascii(text = text)
  conv <- remove_hmtl_formatting(text = conv)
  conv <- gsub("&amp;|amp;", "and", conv)
  conv <- gsub("&#39;|&quot;", "'", conv)
  conv <- gsub("&gt;", "Greater Than ", conv)
  conv <- str_trunc(text, width = 500)
  return(conv)
}
