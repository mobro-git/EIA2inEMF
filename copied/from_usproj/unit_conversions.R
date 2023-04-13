# Unit conversion references:
# USDA: https://www.ers.usda.gov/webdocs/publications/41880/33132_ah697_002.pdf?v=0
# IPCC AR5 WG3 metric & methodology: https://www.ipcc.ch/site/assets/uploads/2018/02/ipcc_wg3_ar5_annex-ii.pdf
# TODO: promote to a vignette?

#' Code to create needed conversions for user-defined units and map unit names to achieve consistency between all model results
#'
#' To introduce a new base unit or conversion constant, use the command units::install_unit
#' Note that units cannot end or start in numbers, and cannot contain spaces or special characters
#' Instead of creating conversions for each unit, create a conversion constant (or use an existing constant below)
#'
#' @import units
#'
install_extra_units <- function() {

  # Conversion constants
  units::install_unit("dozen", "12 1", "Dozen")
  units::install_unit("hundred", "1e2 1", "Hundred")
  units::install_unit("thousand", "1e3 1", "Thousand")
  units::install_unit("million", "1e6 1", "Million")
  units::install_unit("billion", "1e9 1", "Billion")
  units::install_unit("trillion", "1e12 1", "Trillion")
  units::install_unit("quadrillion", "1e15 1", "Quadrillion")

  # Population
  # units has base unit 'count' which may be equiv here
  units::install_unit("head")
  units::install_unit("person")
  units::install_unit("capita") # remove? or conversion constant of person?
  units::install_unit("number") # remove?

  # Weight
  units::install_unit("hundredweight", "100 pounds") # see USDA
  units::install_unit("bales") # variation by crop and standard

  # Emissions
  units::remove_unit("kt") # in default units database, kt is a symbol for knots. After removal, kt is recognized as kilotonnes ('k' prefix with symbol 't' for metric tonnes)

  units::install_unit("CO2e", "unitless")
  units::install_unit("tCO2e", "t CO2e")
  units::install_unit("MMTCO2e", "1e6 tCO2e")
  units::install_unit("gCO2e", "g CO2e")
  units::install_unit("MMT", "1e6 tonnes")

  # under this approach, any of the following can be understood by the system:
  # c("MMTCO2e", "TgCO2e", "MtCO2e", "million tonnes CO2e")

  # Energy
  units::install_unit("kWh")

  # TODO: I am thinking the following emissions units should be removed. In general, GWP conversions are too complicated for the units framework. While CO2e is a unit in which any substance can be measured, the others are not generally used that way.

  #units::install_unit("N2O") # is this being used for N2O or N fert?
  #units::install_unit("BOD")


  # Finances
  # TODO: in general, conversions between dollar years need their own approach, probably not handled through the units package
  units::install_unit("dollars")
  units::install_unit("cents", "0.01 dollars")
  #units::install_symbolic_unit("USD2010_")

  # called from onLoad, this object ensures called just once per session
  extra_units_installed <<- TRUE

}


#' Standardize Unit Names by Converting Aliases to Units package names
#'
#' Applies string substitutions to the units in a units column to ensure that the units can be
#' parsed by the units package. This is currently done as a step in handle_units, but it will
#' be moved into data-loading functions in the future.
#'
#' @import units
#'
unit_name_standardize <- function(unit) {

  unit <- case_when(
    unit %in% MMTCO2e_equiv_units ~ MMTCO2e_equiv_units[[1]],
    unit %in% kt_equiv_units ~ kt_equiv_units[[1]],
    TRUE ~ unit)

  unit <- gsub(" per ", "/", unit)
  unit <- gsub("Number", "number", unit)
  unit <- gsub("N", "N2O", unit) # TODO: is this N fertilizer or N2O?
  unit <- gsub("cwt.", "hundredweight", unit)
  unit <- gsub("short ton", "ton", unit)
  unit <- gsub("fraction", "percent/hundred", unit)
  unit <- gsub(" ", "*", unit)

  str_replace_all(unit, coll(units_package_name_aliases))
}

units_package_name_aliases <- c(
  "quads" = "quadrillion Btu",
  "short ton" = "ton"
  # tonne is metric, ton is U.S.
)

#' Takes a dataframe with a value column and a units column and creates a combined column,
#' replacing the original value column. This column contains either units objects or mixed_units
#' objects, depending on the number of different units in the original units column.
#'
#' @import units
#'
handle_units <- function(df, rename_fun = unit_name_standardize) {
  value <- df$value
  unit <- do.call(rename_fun, list(df$unit))
  units_options(set_units_mode = "standard")
  if (length(unique(unit)) > 1) {
    value <- mixed_units(value, unit)
  } else {
    value <- set_units(value, value=unit[[1]])
  }

  df$value <- value
  select(df, -unit) # remove unit column now that value is a unit object
}

#' Takes a dataframe with a value column made up of units objects or mixed_units objects
#' and splits it into two columns, one with units information and one with value information
#'
#' @import units
#'
flatten_units <- function(df) {
  valunit <- df$value
  if (class(valunit)[1] == "units") {
    value <- drop_units(valunit)
    value_length <- length(value)
    unit_str <- deparse_unit(valunit)
    unit <- rep(c(unit_str), value_length)
    df$value <- value
    df$unit <- unit
  } else if  (class(valunit)[1] == "mixed_units") {
    value <- drop_units(valunit)
    value_length <- length(value)
    unit <- sapply(valunit, deparse_unit)
    df$value <- value
    df$unit <- unit
  }
  df
}
