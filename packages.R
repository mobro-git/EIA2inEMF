## library() calls go here
library(conflicted)
conflict_prefer("filter", "dplyr")

library(dotenv)
library(targets)
library(tarchetypes)

library(tidyverse)
library(readxl)

library(fs)   # path operations
library(here) # finding project root
library(waldo) # better comparisons

library(eia)   # retrieving from EIA api
library(units) # unit conversions

library(rlang) # quasi-quotation
library(stringr) # string functions

# for lockfile detection
if(FALSE) {
  library(visNetwork)
}

# interactive
# library(usethis) # setup tools
# library(tflow) # github.com/MilesMcBain/tflow
# library(fnmate) # github.com/MilesMcBain/fnmate
