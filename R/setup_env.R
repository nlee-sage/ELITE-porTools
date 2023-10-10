# setup env
# Package names
packages <- c("librarian", "knitr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# install.packages("synapser", repos=c("http://ran.synapse.org", "http://cran.fhcrc.org"))

librarian::shelf(
  optparse,
  rentrez,
  rmarkdown,
  reticulate,
  janitor,
  dplyr,
  readr,
  stringr,
  reticulate,
  easyPubMed
)
