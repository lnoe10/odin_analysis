library(tidyverse)

setwd("C:/Users/loren/Documents/R work")

# All ODIN scores

odin_2022_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 7) |>
  filter(!is.na(Region)) |>
  janitor::clean_names() |>
  mutate(across(indicator_coverage_and_disaggregation:overall_score, ~as.numeric(str_remove(.x, "-")))) |>
  pivot_longer(indicator_coverage_and_disaggregation:overall_score, names_to = "element", values_to = "score") |>
  mutate(element = str_to_sentence(str_replace_all(element, "_", " ")))

odin_2020_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 5) |>
  filter(!is.na(Region)) |>
  janitor::clean_names()|>
  mutate(across(indicator_coverage_and_disaggregation:overall_score, ~as.numeric(str_remove(.x, "-")))) |>
  pivot_longer(indicator_coverage_and_disaggregation:overall_score, names_to = "element", values_to = "score") |>
  mutate(element = str_to_sentence(str_replace_all(element, "_", " ")))

odin_2018_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 3) |>
  filter(!is.na(Region)) |>
  janitor::clean_names()|>
  mutate(across(indicator_coverage_and_disaggregation:overall_score, ~as.numeric(str_remove(.x, "-")))) |>
  pivot_longer(indicator_coverage_and_disaggregation:overall_score, names_to = "element", values_to = "score") |>
  mutate(element = str_to_sentence(str_replace_all(element, "_", " ")))

odin_2017_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 2) |>
  filter(!is.na(Region)) |>
  janitor::clean_names()|>
  mutate(across(indicator_coverage_and_disaggregation:overall_score, ~as.numeric(str_remove(.x, "-")))) |>
  pivot_longer(indicator_coverage_and_disaggregation:overall_score, names_to = "element", values_to = "score") |>
  mutate(element = str_to_sentence(str_replace_all(element, "_", " ")))

odin_2016_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 1) |>
  filter(!is.na(Region)) |>
  janitor::clean_names()|>
  mutate(across(indicator_coverage_and_disaggregation:overall_score, ~as.numeric(str_remove(.x, "-")))) |>
  pivot_longer(indicator_coverage_and_disaggregation:overall_score, names_to = "element", values_to = "score") |>
  mutate(element = str_to_sentence(str_replace_all(element, "_", " ")))

wb_codes <- read_csv("Input data/wb_countries_fy24.csv", show_col_types = F) %>% 
  janitor::clean_names() %>%
  filter(!is.na(region)) %>%
  mutate(income_group = fct_relevel(income_group, "Low income", "Lower middle income", "Upper middle income", "High income"))

odin_all <- odin_2022_scores |>
  bind_rows(odin_2020_scores) |>
  bind_rows(odin_2018_scores) |>
  bind_rows(odin_2017_scores) |>
  bind_rows(odin_2016_scores) |>
  rename(data_category = data_categories) |>
  mutate(data_category = str_remove(data_category, "\\s\\r\\n$|\\r\\n"),
         macro_sector = case_when(
           data_category %in% c("Population & vital statistics", "Education facilities",
                                  "Education outcomes", "Health facilities", "Health outcomes",
                                  "Reproductive health", "Food security & nutrition", "Gender statistics",
                                  "Crime & justice", "Poverty & income")  ~ "Social statistics",
           data_category %in% c("National accounts", "Labor", "Price indexes", "Government finance",
                                  "Money & banking", "International trade", "Balance of payments") ~ "Economic and financial statistics",
           data_category %in% c("Agriculture and land use", "Land use", "Resource use", "Energy", "Energy use", "Pollution", "Built environment") ~ "Environmental statistics",
           TRUE ~ "Sub or Overall Scores"
         ),
         # Create macro element group for use in graphing/analysis
         macro_element = case_when(
           element %in% c("Indicator coverage and disaggregation", "Data available last 5 years",
                          "Data available last 10 years", "First administrative level", "Second administrative level") ~ "Coverage elements",
           element %in% c("Non proprietary", "Metadata available", "Machine readable", "Download options", "Terms of use") ~ "Openness elements",
           TRUE ~ "Sub or Overall Scores"
         )) |>
  left_join(wb_codes |> 
              select(country_code = code, income_group))

odin_all |> 
  # Dropping income group
  select(year, country, country_code, region, region_code, macro_sector, data_category, macro_element, element, score) |> 
  arrange(desc(year), country_code, macro_sector, data_category, macro_element, element) |> 
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/ODIN Data 2016-2022 full dataset.xlsx", sheetName = "Data", showNA = FALSE)

data.frame(c("Last updated: August, 9 2023",
             "©2020 Open Data Watch - Licensed under a Creative Commons Attribution 4.0 license",
             "Please cite any uses of these data as: Open Data Watch -- Open Data Inventory http://www.opendatawatch.com'",
"year", "country", "country_code", "region", "region_code", "macro_sector", "data_category", "macro_element", "element", "score"), 
           c(NA_character_, NA_character_, NA_character_, "Year of ODIN assessment", "country", "ISO 3166-1 alpha-3 country codes", "M49 region", "M49 region codes", 
             "Sorting variable that groups data categories by type (Economic and Financial Statistics, Environmental Statistics, Social Statistics)", 
             "ODIN data category", 
             "Sorting variable that groups elements by type (elements that contribute to coverage assessment, elements that contribute to openness assessment, and overall scores)", 
             "ODIN assessment for coverage element, openness element, or overall", 
             "score for element*category combination")) |>
  xlsx::write.xlsx("Output Data/ODIN Data 2016-2022 full dataset.xlsx", sheetName = "MetaData", append = TRUE, showNA = FALSE)


gdc_data <- readxl::read_excel("Input Data/GDC_data_full.xlsx") |>
  janitor::clean_names() |>
  filter(indicator == "All Indicators") |>
  mutate(region = str_remove(region, "\\r\\n"),
         continent = case_when(
    str_detect(region, "Africa") ~ "Africa",
    str_detect(region, "Asia|Pacific Islands|Australia and New Zealand") ~ "Asia",
    TRUE ~ "Other regions"
  )) 

gdc_data |>
  group_by(continent) |>
  summarize(across(sex_disaggregation:availability_and_openness_score, ~mean(.x, na.rm = TRUE))) |>
  ungroup() |>
  write_csv("Output Data/GDC continent element averages.csv", na = "")
