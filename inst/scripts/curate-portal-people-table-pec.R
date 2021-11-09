library(tidyverse)
library(purrr)
synapseclient <- reticulate::import("synapseclient")
syntab <- reticulate::import("synapseclient.table")
syn <- synapseclient$Synapse()
syn$login()

## functions
update_synapse_table <- function(table_id, update_df, syn, syntab) {
  current_rows <- syn$tableQuery(glue::glue("SELECT * FROM {table_id}"))
  syn$delete(current_rows)
  tmpfile <- fs::file_temp("rows.csv")
  write_csv(update_df, tmpfile)
  update_rows <- syntab$Table(table_id, tmpfile)
  syn$store(update_rows)
}
make_df <- function(list, column_name) {
  df <- tibble::enframe(list) %>%
    tidyr::unnest(cols = c(value), keep_empty = TRUE)
  df <- dplyr::select(df, value) %>%
    dplyr::rename((!!column_name) := value)
  df
}
###
people <- read_csv(syn$tableQuery("Select * from syn22096112")$filepath)
team <- syn$getTeamMembers("3323356")
list <- reticulate::iterate(team)
member <- map(list, ~.$get("member"))
firstName <- map(member, ~.$get("firstName"))
lastName <- map(member, ~.$get("lastName"))
userName <- map(member, ~.$get("userName"))

fn <- make_df(firstName, "firstName")
ln <- make_df(lastName, "lastName")
un <- make_df(userName, "userName")

dat <- cbind(fn, ln, un)

new <- setdiff(dat$lastName, people$lastName)
new <- dat[dat$lastName %in% new,]

# the next step requires manual curation because not all members of PEC should
# be represented on the site + some users have not supplied their name in their
# user profile

# get userId
new <- mutate(new, userId = map(userName, ~syn$getUserProfile(.)$ownerId)) %>%
  select(-userName) %>%
  mutate(userId = as.numeric(unlist(userId)))

# bind
update <- bind_rows(people, new)
update <- update[order(update$lastName),]

# NAs must be changed to empty strings
update <- update %>% mutate_all(function(x) ifelse(is.na(x),"",x))

update$ROW_ID <- ""

update_synapse_table("syn22096112", update, syn, syntab)
