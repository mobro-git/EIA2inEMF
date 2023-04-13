# get_eia_data.R


# Function to divide a vector into increments of x -- returns a list
vector_slice <- function(vec, increment) {
  new_vec <- split(vec, ceiling(seq_along(vec)/increment))
  return(new_vec)
}


#' Download EIA API data based on keys / series_id values
#'
#' @param eia_api_keys character vector of series_ids to download
#' @param start passed on to eia::eia_series
#'
get_eia_api_key_data <- function(eia_api_keys, start = NULL) {

  eia_api_keys_slices <- vector_slice(eia_api_keys, 100)

  data_raw <- map_dfr(eia_api_keys_slices,
                      ~eia::eia_series(id = .x, tidy = TRUE, start = start))

  final_vars <- c("series_id", "name",  "year", "unit", "value")

  arrange_vars <- setdiff(final_vars, c("value"))

  proc_data <- data_raw %>%
    tidyr::unnest(data) %>%
    rename(unit = units) %>%
    arrange(across(any_of(arrange_vars))) %>%
    select(all_of(final_vars))

  proc_data
}


# standardize_eia_units <- function(x) {
#   case_when(
#     x == "MMmt CO2" ~ "MMTCO2e",
#     x == "million metric tons" ~ "MMTCO2e", #STEO
#     x == "billion 2012 $" ~ "billion dollars",
#     x == "billion chained 2012 dollars (seasonally-adjusted annual rate)" ~ "billion dollars", #STEO
#     x == "MMst" ~ "million short_tons",
#     x == "MMb/d" ~ "million barrels/day",
#     TRUE ~ x
#   )
# }
