# eia_create_api_id.R

# script creates EIA API ID and names from lists


#### read mapping files ----

industry_mapping <- read.csv("data-raw/mapping/EMF-EIA_industry_mapping.csv", header = TRUE) %>%
  filter(EMF_industry_name != "")

industry_var_mapping <- read.csv("data-raw/mapping/EMF-EIA_industry_var_mapping.csv", header = TRUE)

# industry_var_list <- industry_mapping %>%
#   select(EMF_industry_name)


#### expand industry list and create variables for each industry ----

EMF_subcategory_list <- industry_var_mapping %>%
  select(EMF_subcategory)

AEO_industry_mapping_raw <- industry_mapping %>%
  expand(industry_mapping,EMF_subcategory_list) %>%
  left_join(industry_var_mapping, by = "EMF_subcategory") %>%

  mutate(Series_template =
           paste0("{EIA_dataset}.{EIA_scenario}.",
                  EIA_prefix,
                  EIA_abbrev,
                  "_",
                  EIA_energy_code,
                  EIA_suffix)
         ) %>%
  mutate(Series_Name =
          if_else(EIA_subsector != "",
                   paste0(EIA_main_sector," : ",EIA_subsector, " : ",EIA_category_name),
                   paste0(EIA_main_sector," : ",EIA_category_name)
          )) %>%
  mutate(Variable =

           if_else(EMF_category == "Energy",
                   paste0("Final Energy|Industry|",EMF_main_sector,"|",EMF_industry_name,"|", EMF_subcategory),
           if_else(EMF_category == "Emissions",
                  paste0("Emissions|CO2|Energy|Demand|Industry|", EMF_main_sector,"|", EMF_industry_name),
           if_else(EMF_category == "Output",
                   paste0("Output|Industry|", EMF_main_sector,"|", EMF_industry_name, "|Value"),
                   "NA")),
              )
           ) %>%
  mutate(notes = paste0(notes_var_map,". ", notes_ind_map))

AEO_industry_mapping <- AEO_industry_mapping_raw %>%
  select(Variable, Series_template, Series_Name, notes) %>%
  mutate(Factor = 1) %>%
  mutate(notes_variable = "")



#### read  files ----

AEO_mapping_template_raw <- read.csv("data-raw/mapping/AEO_mapping_template.csv", header = TRUE)

AEO_mapping_template_expanded <- bind_rows(AEO_mapping_template_raw, AEO_industry_mapping) %>% arrange(desc(Variable))

write.csv(AEO_mapping_template_expanded, "data-raw/mapping/AEO_mapping_template_expanded.csv", row.names = FALSE)
