# convert EPA GHGI data to standard format

ghgi_raw <- read_csv("data-raw/mapping/2022 GHGI emissions data mapping.csv", col_names = TRUE)

ghgi_data <- ghgi_raw %>%
  filter(variable !="") %>%
  select(-var_coverage,-notes,-chapter,-table,-rownum, -rowlabel,-category,-gas) %>%
  filter(unit == "MMTCO2e") %>%
  mutate(unit = "MMTCO2/yr") %>%
  mutate(model = "EPA.GHGI") %>%
  mutate(scenario = "2022") %>%
  arrange(model, scenario, region, variable, unit)


# ghgi_convert_kt <- ghgi_data %>%
#   filter(unit == "kt") %>%
#   pivot_longer(cols = 1990:2020, values_to = "year")
#

write.csv(ghgi_data, "output/EPA_GHGI.csv", row.names = FALSE)

