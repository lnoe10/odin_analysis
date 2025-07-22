library(tidyverse)

setwd("C:/Users/loren/Documents/R work")

odin_2022_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 7) |>
  filter(!is.na(Region)) |>
  janitor::clean_names() |>
  filter(data_categories == "All Categories") |>
  select(year, region, region_code, country, country_code, overall_score, coverage_subscore, openness_subscore)

odin_2020_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 5) |>
  filter(!is.na(Region)) |>
  janitor::clean_names()|>
  filter(data_categories == "All Categories") |>
  select(year, region, region_code, country, country_code, overall_score, coverage_subscore, openness_subscore)

odin_2018_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 3) |>
  filter(!is.na(Region)) |>
  janitor::clean_names()|>
  filter(data_categories == "All Categories") |>
  select(year, region, region_code, country, country_code, overall_score, coverage_subscore, openness_subscore)

odin_2017_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 2) |>
  filter(!is.na(Region)) |>
  janitor::clean_names()|>
  filter(data_categories == "All Categories") |>
  select(year, region, region_code, country, country_code, overall_score, coverage_subscore, openness_subscore)

odin_2016_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 1) |>
  filter(!is.na(Region)) |>
  janitor::clean_names()|>
  filter(data_categories == "All Categories") |>
  select(year, region, region_code, country, country_code, overall_score, coverage_subscore, openness_subscore)

wb_codes <- read_csv("Input data/wb_countries_fy24.csv", show_col_types = F) %>% 
  janitor::clean_names() %>%
  filter(!is.na(region)) %>%
  mutate(income_group = fct_relevel(income_group, "Low income", "Lower middle income", "Upper middle income", "High income"))

odin_all <- odin_2022_scores |>
  bind_rows(odin_2020_scores) |>
  bind_rows(odin_2018_scores) |>
  bind_rows(odin_2017_scores) |>
  bind_rows(odin_2016_scores) |>
  left_join(wb_codes |> 
              select(country_code = code, income_group))

# ODM info
odm_df <- read_csv("Input Data/ODM2023_results_historical.csv") |>
  janitor::clean_names() |>
  pivot_longer(x2018_policy:x2023_overall, names_to = "indicator", values_to = "odm_pct") |>
  mutate(year = as.numeric(str_extract(indicator, "[0-9]{4}")),
         indicator = str_remove(indicator, "x[0-9]{4}_"), 
         country_code = countrycode::countrycode(country_year_dimension, "country.name", "iso3c")) |>
  pivot_wider(id_cols = c(country_year_dimension, country_code, year), names_from = indicator, names_prefix = "ogdm_", values_from = odm_pct)

data_for_cor <- odin_all |>
  left_join(odm_df) |>
  semi_join(odm_df) |>
  select(overall_score, coverage_subscore, openness_subscore, ogdm_policy, ogdm_portal, ogdm_impact, ogdm_quality, ogdm_overall)

cor_data <- cor(data_for_cor, use = "pairwise.complete.obs")
