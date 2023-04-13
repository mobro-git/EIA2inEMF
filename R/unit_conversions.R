
#' Convert Units
#'
#' @param from_values numeric vector
#' @param from_units character vector, same length as from_values
#' @param to_units character vector, same length as from_values
#'
#' Units are represented as character vectors, will be searched for within `units` package
#' and custom units defined for this analysis.
#'
#' Steps:
#' 0) Check that all from and to units are recognizable and convertible
#' 1) Translate the character representations to representations recognized by `units` for source and destination units
#' 2) Make units objects, translate to new units
#' 3) Untranslate and simplify objects
#'
convert_units_numeric <- function(from_value, from_unit, to_unit) {

  # build translation to units recognized by `units` package
  translate_source_to_units_unit <- tibble(
    from_unit = from_unit,
    to_unit = to_unit
  ) %>%
    distinct(from_unit, to_unit) %>%
    mutate(from_units_unit = str_replace_all(from_unit, coll(units_package_names)),
           to_units_unit = str_replace_all(to_unit, coll(units_package_names))) %>%

    mutate(conv_factor = as.numeric(set_units(mixed_units(1, from_units_unit), to_units_unit)))

  # apply conversion factors
  res_data <- tibble(
    from_unit = from_unit,
    to_unit = to_unit,
    from_value = from_value
  ) %>%
    left_join(translate_source_to_units_unit, by = c("from_unit", "to_unit")) %>%
    mutate(to_value = from_value * conv_factor)

  res_data$to_value
}

#' Code to create needed conversions for user-defined units and map unit names to achieve consistency between all model results
#'
#' To introduce a new unit, use the command units::install_symbolic_unit
#' To create a conversion between a pair of units, use the command units::install_conversion_constant(unit1, unit2, conversionrate)
#' Note that units cannot end or start in numbers, and cannot contain spaces or special characters
#'
#' @import units
#'
install_extra_units <- function() {

  # NEW UNITS PACKAGE

  # dimensionless
  units::install_unit("thousand", "1e3 1", "Thousand")
  units::install_unit("million", "1e6 1", "Million")
  units::install_unit("billion", "1e9 1", "Billion")
  units::install_unit("quadrillion", "1e15 1", "Quadrillion")
  units::install_unit("trillion", "1e12 1", "Trillion")

  units::install_unit("MMBtu", "1e6 Btu")
  # example for testing unit conversions --> as_units(5000000,"Btu") %>% set_units("MMBtu")

  # OLD UNITS PACKAGE

  units::install_unit("per", "1e-1") # TODO: needed? correctly defined?

  # Energy units
  #units::install_conversion_constant('MMBtu','Btu', 10^6)
  units::install_unit('kwh','1e3 watthour', 'kWh') # upper and lowercase
  units::install_unit('bkwh','1e9 kwh')
  units::install_unit('short_tons_coal','18875000 Btu')
  units::install_unit('cubic_feet','1037 Btu') # From EIA: https://www.eia.gov/tools/faqs/faq.php?id=45&t=8
  units::install_unit('barrelnew','6.011708 GJ') # From EIA calculator (https://www.eia.gov/energyexplained/units-and-calculators/energy-conversion-calculators.php)
  # the units package is not allowing the conversion between barrels and GJ, but since both are individually recognized by the package, we cannot define a conversion between them

  # Emissions
  units::install_unit('Million_ton_CO2_yr') # TODO: this probably should not be dimensionless?

  # Monetary units and conversions
  units::install_unit('cent') # TODO: this probably should not be dimensionless?

  # base dollar being defined as equivalent to 2018 dollars
  units::install_unit(c('USD', 'US2018D'), '100 cent', c('dollar', 'US_2018_Dollar'))

  # NOTE: in units 0.8.0, trailing "s" always dropped from unit names
  # units::install_unit('US2018D', '1 dollar', name = "US_2018_Dollar") # units cannot end or start in numbers
  # units::install_unit('US_2018_Dollars', '1 dollar') # units cannot end or start in numbers

  # Producer Price Index: Total Consumer Goods, from FRED PITGCG01USA661N
  units::install_unit('US_2012_Dollar', '106.10422/101.51688 US_2018_Dollar')
  units::install_unit('US_2016_Dollar', '106.10422/98.47496 US_2018_Dollar')
  units::install_unit('US_2019_Dollar', '106.10422/106.43044 US_2018_Dollar')
  units::install_unit('US_2020_Dollar', '106.10422/103.96754 US_2018_Dollar')
  units::install_unit('US_2021_Dollar', '106.10422/115.35418 US_2018_Dollar')
  units::install_unit('US_2019_cent', '.01 US_2019_Dollar')
  units::install_unit('US_2020_cent', '.01 US_2020_Dollar')
  units::install_unit('US_2021_cent', '.01 US_2021_Dollar')

  extra_units_installed <<- TRUE
}

# Just once per session:
if(!exists("extra_units_installed")) install_extra_units()


# Map unit names to allow comparison between the same variables from different models

# Named vector to make changes withing the character strings of the 'to' and 'from' unit columns
# to be compatible with units package either directly or to be converted using 'map_to_units_package'

#' Convert unit name aliases (character vector form) to standardized units package versions
#'
#' @param x character vector of unit names
#'
unit_name_standardize <- function(x) {
  str_replace_all(x, coll(units_package_names))
}

# API unit name = unit name in units package
units_package_names <- c(
  '2019 $/b' = 'US_2019_Dollar/barrelnew',
  '2020 $/b' = 'US_2020_Dollar/barrelnew',
  '2021 $/b' = 'US_2021_Dollar/barrelnew',

  'dollars' = 'dollar',
  'US$2018' = 'US_2018_Dollar',
  '2012  $' = 'US_2012_Dollar',
  '2012 $'  = 'US_2012_Dollar',

  'chained 2012 dollar (seasonally-adjusted annual rate)' = 'US_2012_Dollar',
  '2019 $/st' = 'US_2019_Dollar/short_ton',
  '2019 $' = 'US_2019_Dollar',
  '2020 $' = 'US_2020_Dollar',
  '2021 $' = 'US_2021_Dollar',
  'cents' = 'cent',
  '2019 cent' = 'US_2019_cent',
  '2020 cent' = 'US_2020_cent',
  '2021 cent' = 'US_2021_cent',
  'kilowatt hour' = 'kWh',
  'quads' = 'quadrillion btu',
  'BkWh' = 'billion kwh',
  'million Btu' = 'MMBtu',
  'MMmt CO2' = 'Million_ton_CO2_yr',
  'Mt CO2/yr' = 'Million_ton_CO2_yr',
  #'million metric tons CO' = 'Million_ton_CO2_yr',
  'million metric tons CO2' = 'Million_ton_CO2_yr',
  'Million Metric Tons of Carbon Dioxide' = 'Million_ton_CO2_yr',
  'metric tons' = 'metric_tons',
  'cubic feet' = 'cubic_feet',
  'short tons' = 'short_tons',
  '/yr' = '',
  'per' = '/'
)

# rename units & eia names -> template names
map_to_emf_template <- c(
  "US_2018_Dollar/GJ"       = "US$2018/GJ",
  "MMmt CO2"                = "Mt CO2/yr",
  "million metric tons CO2" = "Mt CO2/yr",
  "Million_ton_CO2_yr"      = "Mt CO2/yr",
  "EJ"                      = "EJ/yr",
  "Billion_US_2018_Dollar"  = "billion US$2018/yr",
  "US_2018_Dollars/GJ"      = "US$2018/GJ")

