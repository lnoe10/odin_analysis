library(tidyverse)

setwd("C:/Users/loren/Documents/R work")

# SDG report data story numbers check

# WB income groups
wb_codes <- read_csv("Input data/wb_countries_fy24.csv", show_col_types = F) %>% 
  janitor::clean_names() %>%
  filter(!is.na(region)) %>%
  mutate(income_group = fct_relevel(income_group, "Low income", "Lower middle income", "Upper middle income", "High income"),
         custom_inc = case_when(
           income_group %in% c("Low income", "Lower middle income") ~ "Low or Lower middle income",
           TRUE ~ income_group
         ))

# GDC
gdc <- readxl::read_excel("Input Data/GDC_data_full.xlsx", sheet = 1) |>
  janitor::clean_names() |>
  # income group info available for all but Anguilla
  left_join(wb_codes, by = c("country_code" = "code"))

# Average element scores by categories
gdc |>
  filter(str_detect(indicator, "Indicators")) |>
  group_by(indicator) |>
  summarize(across(sex_disaggregation:availability_and_openness_score, ~mean(.x, na.rm = TRUE))) |>
  ungroup() |>
  arrange(desc(geographic_disaggregation))

# Average element scores by income groups
gdc |>
  filter(str_detect(indicator, "All Indicators")) |>
  group_by(income_group) |>
  summarize(across(sex_disaggregation:availability_and_openness_score, ~mean(.x, na.rm = TRUE))) |>
  ungroup() |>
  arrange(desc(availability_score))
  
# ODIN

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

odin_all <- odin_2022_scores |>
  bind_rows(odin_2020_scores) |>
  bind_rows(odin_2018_scores) |>
  bind_rows(odin_2017_scores) |>
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
              select(country_code = code, income_group, custom_inc))

# The average score of data openness increased from 43 in 2017 to 53 in 2022
odin_all |>
  filter(data_category == "All Categories", element == "Openness subscore") |>
  group_by(year) |>
  summarize(mean_openness = mean(score, na.rm = TRUE)) |>
  ungroup()
# Technically true

# Steady sample
sample_2017_2022 <- odin_all |>
  filter(year> 2016) |>
  distinct(country_code, year) |>
  group_by(country_code) |>
  summarize(num_obs = n()) |>
  ungroup() |>
  filter(num_obs == 4) |>
  select(country_code)

odin_all |>
  filter(data_category == "All Categories", element == "Openness subscore") |>
  semi_join(sample_2017_2022) |>
  group_by(year) |>
  summarize(mean_openness = mean(score, na.rm = TRUE)) |>
  ungroup()

# Openness score by income group
odin_all |>
  filter(year == 2022, data_category == "All Categories", element == "Openness subscore") |>
  group_by(custom_inc) |>
  summarize(mean_openness = mean(score, na.rm = TRUE)) |>
  ungroup()

# Openness elements by income group
odin_all |>
  filter(year == 2022, data_category == "All Categories", macro_element %in% c("Openness elements", "Sub or Overall Scores")) |>
  group_by(custom_inc, element) |>
  summarize(mean_openness = mean(score, na.rm = TRUE)) |>
  ungroup() |>
  bind_rows(
    odin_all |>
      filter(year == 2022, data_category == "All Categories", macro_element %in% c("Openness elements", "Sub or Overall Scores")) |>
      group_by(element) |>
      summarize(mean_openness = mean(score, na.rm = TRUE)) |>
      ungroup() |>
      mutate(custom_inc = "World")
  ) |>
  filter(!element %in% c("Coverage subscore", "Overall score")) |>
  print(n = Inf)
