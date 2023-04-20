library(targets)
library(tarchetypes)

# https://github.com/jameelalsalam/eia2
# devtools::install_github("jameelalsalam/eia2)
# library(eia2)

## Load your packages, e.g. library(targets).
source("packages.R")

# devtools::load_all(".") # load all function definitions in /R

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

AEO_combos <- expand_grid(
  EIA_dataset = c("AEO.2020","AEO.2021","AEO.2022"),
  EIA_scenario = c("REF2020","REF2021","REF2022")
)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(

  ### Input files as format = "file" to watch for changes in these files

  # EMF Template
  tar_target(emf_template_xlsx, path("data-raw", "templates", "EMF37_data_template_R2_v1.xlsx"), format = "file"),

  # STEO and Historic Mapping
  tar_target(eia_steo_mapping_csv, path("data-raw", "mapping", "STEO_mapping.csv"), format = "file"),
  tar_target(eia_hist_mapping_csv, path("data-raw", "mapping", "Historical_mapping.csv"), format = "file"),

  # AEO Mapping - w/ automation for industrial subsector series IDs
  tar_target(eia_aeo_mapping_template_csv, path("data-raw", "mapping", "AEO_mapping_template.csv"), format = "file"),
  tar_target(industry_mapping_csv, path("data-raw", "mapping", "EMF-EIA_industry_mapping.csv"), format = "file"),
  tar_target(industry_var_mapping_csv, path("data-raw", "mapping", "EMF-EIA_industry_var_mapping.csv"), format = "file"),

  tar_target(eia_aeo_mapping_template_expanded,
             expand_map(eia_aeo_mapping_template_csv,industry_mapping_csv,industry_var_mapping_csv)),

  #### Output data template -------------------------------
  emf_template = read_emf_template_xlsx(emf_template_xlsx),
  # emf_template_beta = {emf_template %>%
  #   filter(round == "beta", tier %in% c(1, 2))},

  # Pull in the mapping template
  aeo_mapping_template = read_eia_mapping(eia_aeo_mapping_template_expanded),
  steo_mapping_raw = read_eia_mapping(eia_steo_mapping_csv),
  historical_mapping_raw = read_eia_mapping(eia_hist_mapping_csv),

  # Make historic data consistent with AEO mapping
  historical_mapping_data =  {
    historical_mapping_raw %>%
    mutate(dataset = "EIAHistoric") %>%
    mutate(scenario = 'Historic',
           region   = 'USA') %>%
    select(c(dataset, scenario, region, series_id, variable, factor))},

  # STEO data
  steo_mapping_data = steo_mapping_raw  %>%
    mutate(dataset = 'EIA_STEO') %>%
    mutate(scenario = 'Ref',
           region   = 'USA') %>%
    select(c(dataset, scenario, region, series_id, variable, factor)),

  # Paste the config parameters for EIA_scenario and EIA_dataset for AEO
  # potentially a grid of values in AEO_combos
  aeo_mapping_data = aeo_mapping_template %>%
    rowwise() %>%
    mutate(
      combo_data = list(AEO_combos),
      series_id = list(interp_eia_key_chr(series_template, replacements = AEO_combos))
    ) %>%
    ungroup() %>%
    unnest(cols = c(combo_data, series_id)) %>%
    mutate(region = "USA") %>%
    rename(
      dataset = EIA_dataset,
      scenario = EIA_scenario
    ) %>%
    dplyr::select(c(dataset, scenario, region, series_id, variable, factor)),

  eia_series_data = bind_rows(aeo_mapping_data, steo_mapping_data, historical_mapping_data),

  #### Access EIA Data API ------------------------

  eia_data_initial = {
    eia_api_keys = unique(eia_series_data$series_id)
    # Get the stored EIA key and throw a warning if not found
    if(is.null(eia::eia_get_key())) warning('No EIA API key is found. Please read the README file for instructions.')
    get_eia_api_key_data(eia_api_keys)
  },

  ##### Clean & Convert to EMF format ----------------------

  # Merge the EIA data with mapping data
  eia_data_with_mapping = full_join(eia_data_initial, eia_series_data, by = 'series_id') %>%
    arrange(series_id, variable, year),

  # For each mapped variable, find the unit in the EMF template
  # Should it warn or error if there are mapped variables not present in EMF template?
  emf_variables_units = {
    template_var_units <- emf_template %>%
      distinct(variable, unit) %>%
      arrange(variable, unit) %>%
      rename(emf_units = unit)

    data_var_units <- eia_data_mapped %>%
      distinct(variable, unit) %>%
      arrange(variable, unit)

    res <- semi_join(template_var_units, data_var_units, by = "variable")
    },

  # Constructing EIA data variables by completing the one to many mappings
  eia_data_mapped = eia_prep(eia_data_with_mapping),
  eia_data_with_emf_units = add_emf_units(eia_data_mapped, emf_variables_units) %>%
    filter(!is.na(value)) %>%
    mutate(unit = ifelse(unit == 'million barrels per day', 'million barrels', unit)) %>% # factor is set at 365
    mutate(unit = ifelse(unit == 'billion cubic feet per day', 'billion cubic_feet', unit)) %>%
    mutate(unit = ifelse(grepl('Emissions|CO2', variable, fixed = TRUE) & unit == 'million metric tons',
                         'million metric tons CO2', unit)) %>%
    mutate(unit = ifelse(grepl('Coal', variable, ignore.case = TRUE) &
                           grepl('short tons', unit, ignore.case = TRUE),
                         gsub('short tons', 'short_tons_coal', unit), unit)) %>%
    mutate(unit = gsub('barrel|barrels', 'barrelnew', unit)),

  ####  Final EIA dataset, compliant with EMF template ----------
  eia_long_converted = eia_data_with_emf_units %>%
    mutate(convert_units(tibble(unit, value), from_unit = unit, to_unit = emf_units)),

  # QA on unit conversion
  tar_target(unit_conv_distinct_crosswalk, {
    unit_conv_distinct_crosswalk <- eia_data_with_emf_units %>%
      select(from_unit = unit, to_unit = emf_units) %>%
      distinct(from_unit, to_unit) %>%
      arrange(from_unit, to_unit)

    unit_conv_distinct_crosswalk_with_text_conv <- unit_conv_distinct_crosswalk %>%
      mutate(from_units_unit = unit_name_standardize(from_unit),
             to_units_unit = unit_name_standardize(to_unit)) %>%
      relocate(from_unit, from_units_unit, to_units_unit, to_unit)

    write_csv(unit_conv_distinct_crosswalk_with_text_conv, "output/QA/unit_conv_distinct_crosswalk.csv")
    "output/QA/unit_conv_distinct_crosswalk.csv"
  }, format = "file"),

  nonmatch = {
    nonmatch <- filter(eia_long_converted, unit != emf_units)
    stopifnot(nrow(nonmatch) == 0)
    nonmatch
    },

  eia_long = eia_long_converted %>%
    select(-emf_units) %>%
    relocate(model, scenario, region, variable, unit, year, value) %>%
    arrange(year, model, scenario, region, variable),

  # Break out datasets to export to separate files
  # AEO
  aeo_wide = {
    eia_long %>%
    filter(grepl('AEO', model, ignore.case = TRUE)) %>%
    pivot_wider(names_from = "year", values_from = "value") %>%

    # Revert the eia data to the template for now (comment out if need to allow
    # multiple potential mapping candidates)
    distinct(model, scenario, region, variable, unit, .keep_all = TRUE)},


  # Historic (for now, all rows not including AEO)
  historic_wide = {
    eia_long %>%
    filter(grepl('Historic', scenario, ignore.case = TRUE)) %>%
    pivot_wider(names_from = "year", values_from = "value") %>%
    mutate(model = "EIA_Historic") %>%

    # Revert the eia data to the template for now (comment out if need to allow
    # multiple potential mapping candidates)
    distinct(model, scenario, region, variable, unit, .keep_all = TRUE)},

  # Historic (for now, all rows not including AEO)
  steo_wide = {
    eia_long %>%
    filter(grepl('STEO', model, ignore.case = TRUE)) %>%
    pivot_wider(names_from = "year", values_from = "value") %>%
    mutate(model = "EIA_STEO") %>%

    # Revert the eia data to the template for now (comment out if need to allow
    # multiple potential mapping candidates)
    distinct(model, scenario, region, variable, unit, .keep_all = TRUE)},


  #### Export data -------------------------------------------

  tar_target(
    output_files_csv, {
    bind_rows(aeo_wide, historic_wide, steo_wide) %>%
      arrange(model, scenario, region, variable) %>%
      write_csv(fs::path("output", "EIAinEMF37.csv"))

    fs::path("output", "EIAinEMF37.csv")
    }, format = "file"),

  tar_target(
    output_change_tracker_tsv,
    {
      # for change tracking, write rounded version to csv
      bind_rows(aeo_wide, historic_wide, steo_wide) %>%
        mutate(across(where(is.numeric), ~round(., 2))) %>%
        select(model, scenario, region, variable, unit, `2020`, `2030`, `2040`) %>%
        arrange(model, scenario, region, variable, unit) %>%
        write_tsv(fs::path("output", "EIA_AEO_REF2020_USA.tsv"))

      fs::path("output", "EIA_AEO_REF2020_USA.tsv")
    }, format = "file"
  )

)
