
# Importing EIA data if option is chosen

# Import mapping template -- assumes a csv file
read_eia_mapping <- function(file){
  mapping <- read_csv(file)
  mapping <- mapping %>%
    select(- matches('Comments')) %>%
    select(- starts_with("notes")) %>%
    janitor::remove_empty(c("rows", "cols")) %>%
    dplyr::rename_with(tolower) %>%
    # info sometimes included with empty EMF variable where the EIA series_id could be useful later
    tidyr::drop_na(any_of(c("variable", "series_id", "series_template")))

  return(mapping)

}

# Construct a dataset with unique combinations of EMF units and variables
construct_emf_variables_units <- function(file) {
  template <- readxl::read_excel(file, sheet = 'AEO_mapping')
  template <- template %>%
    dplyr::select(Variable, EMF_Units) %>%
    dplyr::rename_with(tolower) %>%
    dplyr::distinct_all()
}


# Constructing EIA data variables by completing the one to many mappings
# e_data must have a 'factor' column that is used to scale combinations
# e_data is grouped by dataset, scenario, variable, region, unit, year
eia_prep <- function(e_data) {
  res <- e_data %>%
    dplyr::mutate(value = value * factor) %>%
    dplyr::group_by(dataset, scenario, variable, region, unit, year) %>%
    dplyr::mutate(value = sum(value)) %>%
    dplyr::ungroup() %>%
    rename(model = dataset) %>%
    dplyr::select(model, scenario, region, variable, year, unit, value) %>%
    dplyr::distinct_all()
}

add_emf_units <- function(e_data, template) {
  res_data <- merge(e_data, template, by = 'variable', all = TRUE)
}

# Applying unit conversions

### DONT BELIEVE convert_to_emf_units() is used anywhere in the pipeline....? Not deleting in case it is

convert_to_emf_units <- function(e_data) {
  # standardize unit names to enable the use of the units package
  e2_data <- e_data %>%
    mutate(unit = ifelse(unit %in% names(map_to_emf_template),
                         str_replace_all(unit, coll(map_to_emf_template)),
                         unit)) %>%
    mutate(unit = ifelse(unit %in% names(map_to_units_package),
                         str_replace_all(unit, coll(map_to_emf_template)),
                         unit)) %>%
    mutate(emf_units = ifelse(emf_units %in% names(map_to_units_package),
                              map_to_units_package[emf_units],
                              emf_units)) %>%
    # do not convert data not present in template, but keep in same units
    mutate(emf_units = if_else(is.na(emf_units), unit, emf_units))

  # convert to new units
  # To do: make unit conversions conditional on units requiring conversion so the command does not throw error
  e3_data <- e2_data %>%
    dplyr::mutate(eia_units = unit) %>%
    dplyr::mutate(value_eia = value) %>%
    dplyr::mutate(value_eia = units::mixed_units(value_eia, eia_units)) %>%
    dplyr::mutate(value_eia = units::set_units(value_eia, emf_units)) %>%
    dplyr::select(-c(unit, value, eia_units)) %>%
    dplyr::rename(value = value_eia,
                  unit = emf_units)

  # return to unit names in EMF template & drop s3 units class
  eia_final <- e3_data %>%
    mutate(unit = ifelse(unit %in% names(map_to_emf_template),
                         map_to_emf_template[unit],
                         unit)) %>%
    mutate(value = as.numeric(value))

  eia_final
}

#### Quality Checks ---------------------------------------
# Verify absence of duplicates for a specified group of columns (columns should
# be listed unquoted starting in the second position of the function arguments)
unique_rows <- function(.test_data, ...) {
  .cols <- enquos(...)
  stopifnot(nrow(.test_data) == nrow(distinct(.test_data, !!!.cols)))

}

# Match variable/unit combinations in datasets with template
match_var_unit <- function(.data1, benchmark = ces_var_units, comparison_units = ces_template_unit) {
  comparison_units <- enquo(comparison_units)
  unique_combos <- distinct(.data1, model, scenario, variable, unit)
  diff_units <- left_join(unique_combos, benchmark, by = "variable") %>%
    filter(unit != !!comparison_units)

  return(diff_units)
  stopifnot(nrow(diff_units) == 0)

  }

qu_var_units <- function(.data1) {
  comp_var_unit <- match_var_unit(.data1)

  if(nrow(comp_var_unit) > 0){
    warning('There are differences between the data variable/unit combinations against those of
            the template')
    print(comp_var_unit)
  }
}


