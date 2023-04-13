# config.R
# analysis configuration information

# Adding parameters to configure EIA series IDs -- will be inserted in relevant
# locations in IDs

AEO_combos <- expand_grid(
  EIA_dataset = c("AEO.2020"),
  EIA_scenario = c("REF2020")
)

config <- list(

  template_filepath = path("data-raw", "templates",
    "CES_DATA_Template_Round1_revised.xlsx"),

  eia_aeo_mapping_filepath = path("data-raw", "mapping", "AEO_mapping.csv"),
  eia_steo_mapping_filepath = path("data-raw", "mapping", "STEO_mapping.csv"),
  eia_historical_mapping_filepath = path("data-raw", "mapping", "Historical_mapping.csv")
)

aeo_stub <- "AEO.2020"

# core scenarios
valid_aeo2020_scenarios <- c(
  "REF2020",
  "HIGHMACRO",
  "LOWMACRO",
  "HIGHPRICE",
  "LOWPRICE",
  "HIGHOGS",
  "LOWOGS",
  "HIRENCST",
  "LORENCST"
  )

valid_aeo_regions <- c("USA", state.abb)
