library(tidyverse)
synapseclient <- reticulate::import("synapseclient")
syntab <- reticulate::import("synapseclient.table")
syn <- synapseclient$Synapse()
syn$login()

# Once study folders are annotated, this script will find those annotations and merge them
# into the studies table that creates the study cards in the portal.

### functions
coalesceJoin <- function(x, y,
                         by = NULL, suffix = c(".x", ".y"),
                         join = dplyr::left_join, ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output
  cols <- union(names(x), names(y))

  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and de-duplicate
  to_coalesce <- unique(substr(
    to_coalesce,
    1,
    nchar(to_coalesce) - nchar(suffix_used)
  ))

  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]],
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce

  dplyr::bind_cols(joined, coalesced)[cols]
}

update_synapse_table <- function(table_id, update_df, syn, syntab) {
  current_rows <- syn$tableQuery(glue::glue("SELECT * FROM {table_id}"))
  syn$delete(current_rows)
  tmpfile <- fs::file_temp("rows.csv")
  write_csv(update_df, tmpfile)
  update_rows <- syntab$Table(table_id, tmpfile)
  syn$store(update_rows)
}
###

# update studies table
# force view to rebuild
trigger <- syn$tableQuery("select * from syn21990011")

table <- dccvalidator::get_synapse_table("syn21783965", syn)
fv <- dccvalidator::get_synapse_table("syn21990011", syn)

# Parse rows from file view that contain annotations to be captured in the
# PEC studies table
to_update <- fv[!is.na(fv$studyDescription),]

to_update <- rename(to_update, key = id,
                    studyName = name)
table <- rename(table, key = study)

# join on synId
updated <- coalesceJoin(to_update, table, by = "key", join = full_join)

# change to required schema
updated <- rename(updated, study = key)

# NAs must be changed to empty strings
dat <- updated %>% mutate_all(function(x) ifelse(is.na(x),"",x))

# order cards alphabetically
dat <- dat[order(dat$studyName),]

#order schema
dat <- dplyr::select(dat, studyType, isModelSystem, numberOfIndividuals, species, study, studyDescription, studyName, nucleicAcidSource, contributingInstitution, dataTypes, diagnosis, grants, phase, methods, relatedStudies, tissue)

update_synapse_table("syn21783965", dat, syn, syntab)
