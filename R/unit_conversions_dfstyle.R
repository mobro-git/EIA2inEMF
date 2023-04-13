
#' Convert Units
#'
#' @param x tibble with columns as specified in details
#' @param from_unit unit to convert from
#' @param to_unit unit to convert to
#' @param rename_fun function chr->chr to standardize aliases to unit package names
#'
#' Input tibble must have _at least_ 2 columns:
#' * 'unit' (character vector)
#' * 'value' (usually numeric)
#'
#' Additional columns are returned in the result.
#'
#' Conversion depends on from_unit and to_unit:
#'
#' to_unit could be size 1 (applied to all), size of x (applied rowwise), or
#' if both from_unit and to_unit are supplied, a crosswalk of from/to pairs.
#'
#' If vec_size(to_unit) == vec_size(x) and from_unit not present, then to_unit is taken as a rowwise requirement.
#' Will not attempt to convert values where to_unit is missing, but otherwise
#' any cases where corresponding row of x cannot be converted to to_unit will error.
#'
#' Otherwise, the function will for convenience try to generate a rowwise to_unit specification by either expanding a crosswalk table (if both from_unit and to_unit are present) or testing x$unit for convertibility to to_unit.
#' If from_unit is present, from_unit and to_unit must be the same size and are interpreted as a crosswalk table of conversions.
#'
#' In the convenience case, only recognized unit values are processed. Where conversions do not exist, with to_unit size 1 or crosswalk pairs, original units and values are returned.
#'
#' @return object with same specification as `x`
#' @export
convert_units <- function(x,
                         from_unit = NULL,
                         to_unit = NA_character_,
                         rename_fun = unit_name_standardize) {

  stopifnot(is.data.frame(x))
  stopifnot(all(c("unit", "value") %in% names(x)))
  stopifnot(is.character(to_unit))

  # ensure all units are recognizable?
  # stopifnot(all(x$unit %in% c(MMTCO2e_equiv_units, kt_equiv_units)))

  # decide case based on params:
  conv_case <- conv_units_case(x, from_unit, to_unit)

  # for cases other than full rowwise, generate a crosswalk
  if(conv_case == "test all input x for conversion to_unit length 1") {
    test_crosswalk <- tibble(from_unit = unique(x$unit), to_unit = to_unit) %>%
      mutate(
        from_units_unit = do.call(rename_fun, list(from_unit)),
        to_units_unit = do.call(rename_fun, list(to_unit))) %>%
      mutate(ud_convertible = if_else(
        map2_lgl(from_units_unit, to_units_unit, units:::ud_are_convertible),
        TRUE, FALSE))

    crosswalk <- test_crosswalk %>%
      filter(ud_convertible) %>%
      select(-ud_convertible)

  } else if(conv_case %in% c(
    "crosswalk table",
    "convert set of from_unit to to_unit length 1")) {
    crosswalk <- tibble(from_unit, to_unit)

    # TODO: move this check into conv_units_case
    dist_crosswalk <- distinct(crosswalk, from_unit, to_unit)
    if(length(dist_crosswalk$from_unit) > length(unique(dist_crosswalk$from_unit))) stop("Supplied from_unit must uniquely map to to_unit.")
  }

  # now generate full rowwise spec
  full_conv_spec <-
  if(conv_case == "full rowwise spec") {
    tibble(from_unit = x$unit, to_unit = to_unit)

  } else {
    tibble(from_unit = x$unit) %>%
      left_join(crosswalk, by = "from_unit")
  }

  # now apply full_conv_spec:

  # simplify down to unique combos to pull conversion factors:
  conv_factors_unique <- full_conv_spec %>%

    distinct(from_unit, to_unit) %>% # unique combinations
    filter(!is.na(to_unit)) %>%      # NA to_unit is ignored
    filter(to_unit != from_unit) %>% # no conversion necessary

    # use supplied function to rename non-standard names to
    # units package standard names
    mutate(from_units_unit = do.call(rename_fun, list(from_unit)),
           to_units_unit = do.call(rename_fun, list(to_unit))) %>%

    # pull conversion factors on min size table with mixed_units
    mutate(conv_factor = as.numeric(units::set_units(units::mixed_units(1, from_units_unit), to_units_unit)))

  # choose appropriate conversion factors for each row
  # apply conversion factors
  res_data <- x %>%
    left_join(conv_factors_unique, by = c("unit" = "from_unit")) %>%
    mutate(to_value = value * conv_factor) %>%
    transmute(
      unit = if_else(is.na(conv_factor), unit, to_unit),
      value = if_else(is.na(conv_factor), value, to_value)
    )

  # Carry extra columns in x in the result
  x %>%
    mutate(res_data)
}


conv_units_case <- function(x,
                           from_unit = NULL,
                           to_unit = NA_character_) {

  if(nrow(x) == length(to_unit) && is.null(from_unit)) {
    "full rowwise spec"
  } else if(length(to_unit) == 1 && is.null(from_unit)) {
    "test all input x for conversion to_unit length 1"
  } else if(length(to_unit) == 1 && length(from_unit) > 0) {
    "convert set of from_unit to to_unit length 1"
  } else if(length(to_unit > 1) && length(from_unit) == length(to_unit)) {
    "crosswalk table"
  } else if(length(to_unit > 1) && length(from_unit > 1) && length(to_unit != length(from_unit))) {
    stop("If both from_unit and to_unit are supplied, they must be of compatible size (same size or one of length 1).")
  } else if(length(to_unit) > 1 && is.null(from_unit)) {
    stop("If only to_unit is supplied, it must be length 1 or same length as x")
  } else "unknown"
}



