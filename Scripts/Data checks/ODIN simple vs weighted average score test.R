library(tidyverse)

setwd("C:/Users/loren/Documents/R work")

# ODIN 2022 scores
odin_2022_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 7) |>
  filter(!is.na(Region)) |>
  janitor::clean_names()

# Create separate dataset for just overall and subscores
summary_scores <- odin_2022_scores |> 
  filter(str_detect(data_categories, "subscore|Categories"))

# Recreate subscores and overall scores using element scores
# The mean coverage, openness, and overall scores for each category are already calculated in this dataset
odin_2022_scores |>
  filter(!str_detect(data_categories, "subscore|Categories")) |>
  # Clean data by assigning sector tag to categories and clean admin 1 and 2 element scores
  mutate(macro_sector = case_when(
    data_categories %in% c("Population & vital statistics", "Education facilities",
                           "Education outcomes", "Health facilities", "Health outcomes",
                           "Reproductive health", "Food security & nutrition", "Gender statistics",
                           "Crime & justice", "Poverty & income")  ~ "Social statistics",
    data_categories %in% c("National accounts", "Labor", "Price indexes", "Government finance",
                           "Money & banking", "International trade", "Balance of payments") ~ "Economic and financial statistics",
    data_categories %in% c("Agriculture and land use", "Land use", "Resource use", "Energy", "Energy use", "Pollution", "Built environment") ~ "Environmental statistics",
    TRUE ~ NA_character_
  ),
  first_administrative_level = as.numeric(str_remove(first_administrative_level, "-")),
  second_administrative_level = as.numeric(str_remove(first_administrative_level, "-"))) |>
  # simple average across categories for each sector for each country
  group_by(country, country_code, macro_sector) |>
  summarize(across(indicator_coverage_and_disaggregation:overall_score, ~ mean(.x, na.rm = TRUE))) |>
  ungroup() |>
  # simple average across sectors for each country
  group_by(country, country_code) |>
  summarize(across(indicator_coverage_and_disaggregation:overall_score, ~ mean(.x, na.rm = TRUE))) |>
  ungroup() |>
  # reshape for easier comparison
  pivot_longer(cols = c(indicator_coverage_and_disaggregation:overall_score), names_to = "element", values_to = "simple_avg") |>
  # Merge in official scores
  full_join(summary_scores |>
              filter(data_categories == "All Categories") |>
              mutate(first_administrative_level = as.numeric(str_remove(first_administrative_level, "-")),
                     second_administrative_level = as.numeric(str_remove(first_administrative_level, "-"))) |>
              pivot_longer(cols = c(indicator_coverage_and_disaggregation:overall_score), names_to = "element", values_to = "official")) |>
  mutate(diff = simple_avg - official) |>
  group_by(element) |>
  summarize(min_diff = min(diff, na.rm = TRUE), mean_diff = mean(diff, na.rm = TRUE), max_diff = max(diff, na.rm = TRUE)) |>
  ungroup()