# templates.R

#' Read an excel file into a requirements template object
#'
#' @param filepath to an xlsx or xls template
#'
#' @return requirements template tibble
read_emf_template_xlsx <- function(filepath) {

  readxl::read_excel(
    path = filepath,
    sheet = "variable_definitions"
  ) %>%
    rename_with(.fn = str_to_lower)
}
