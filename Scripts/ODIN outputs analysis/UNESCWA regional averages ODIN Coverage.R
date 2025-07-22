library(tidyverse)

setwd("C:/Users/loren/Documents/R work")

# To create regional averages for ODIN coverage using country groupings specified by UNESCWA

# ODIN all data:
odin_all <- readxl::read_excel("Output Data/ODIN scores 2016-2024 April 2.xlsx") |> 
  filter(data_category == "All Categories", element == "Coverage subscore")

## Import ODIN 2022 overall, openness, and coverage scores
#odin_2022_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 7) |>
#  filter(!is.na(Region)) |>
#  janitor::clean_names() |>
#  filter(data_categories == "All Categories") |>
#  select(year, region, region_code, country, country_code, overall_score, openness_subscore, coverage_subscore)
#
#odin_2020_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 5) |>
#  filter(!is.na(Region)) |>
#  janitor::clean_names()|>
#  filter(data_categories == "All Categories") |>
#  select(year, region, region_code, country, country_code, overall_score, coverage_subscore, openness_subscore)
#
#odin_2018_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 3) |>
#  filter(!is.na(Region)) |>
#  janitor::clean_names()|>
#  filter(data_categories == "All Categories") |>
#  select(year, region, region_code, country, country_code, overall_score, coverage_subscore, openness_subscore)
#
#odin_2017_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 2) |>
#  filter(!is.na(Region)) |>
#  janitor::clean_names()|>
#  filter(data_categories == "All Categories") |>
#  select(year, region, region_code, country, country_code, overall_score, coverage_subscore, openness_subscore)
#
#odin_2016_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 1) |>
#  filter(!is.na(Region)) |>
#  janitor::clean_names()|>
#  filter(data_categories == "All Categories") |>
#  select(year, region, region_code, country, country_code, overall_score, coverage_subscore, openness_subscore)
#
#odin_all <- odin_2022_scores |>
#  bind_rows(odin_2020_scores) |>
#  bind_rows(odin_2018_scores) |>
#  bind_rows(odin_2017_scores) |>
#  bind_rows(odin_2016_scores)

# Create dummy variables for regional categories
# Comoros and Sudan are not part of ODIN 2022 (have not been for years) but are included in country list, in case they show up in the future.
odin_all_groups <- odin_all |>
  mutate(Arab = case_when(
    country %in% c("Algeria", "Bahrain", "Comoros", "Djibouti", "Egypt", "Iraq", "Jordan", "Kuwait", "Lebanon", "Libya", "Morocco", "Mauritania", "Oman", "Qatar", "Saudi Arabia", "Somalia", "Palestine", "Sudan", "Syrian Arab Republic", "Tunisia", "United Arab Emirates", "Yemen") ~ "ESCWA: Arab countries",
    TRUE ~ "None"
  ),
  ESCWA_GCC = case_when(
    country %in% c("Bahrain", "Kuwait", "Oman", "Qatar", "Saudi Arabia", "United Arab Emirates") ~ "ESCWA: Gulf Cooperation Council (GCC)",
    TRUE ~ "None"
  ),
  ESCWA_MASHREQ = case_when(
    country %in% c("Iraq", "Jordan", "Lebanon", "Syrian Arab Republic", "Egypt", "Palestine") ~ "ESCWA: Mashreq subregion",
    TRUE ~ "None"
  ),
  ESCWA_MAGHREB = case_when(
    country %in% c("Algeria", "Libya", "Morocco", "Tunisia") ~ "ESCWA: Maghreb subregion",
    TRUE ~ "None"
  ),
  ESCWA_LDC = case_when(
    country %in% c("Comoros", "Djibouti", "Mauritania", "Somalia", "Sudan", "Yemen") ~ "ESCWA: Arab LDCs subregion",
    TRUE ~ "None"
  ),
  ESCWA_CONFLICT = case_when(
    country %in% c("Iraq", "Libya", "Somalia", "Sudan", "Syrian Arab Republic", "Palestine", "Yemen") ~ "ESCWA: Arab countries in-conflict",
    TRUE ~ "None"
  ),
  ESCWA_NOCONFLICT_MID = case_when(
    country %in% c("Algeria", "Jordan", "Lebanon", "Morocco", "Tunisia", "Egypt") ~ "ESCWA: Arab non-conflict and non-LDC middle income countries",
    TRUE ~ "None"
  ),
  ESCWA_ARAB_MID = case_when(
    country %in% c("Algeria", "Egypt", "Iraq", "Jordan", "Lebanon", "Libya", "Morocco", "Palestine", "Tunisia", "Comoros", "Djibouti", "Mauritania") ~ "ESCWA: Arab Middle-Income Countries (MICs)",
    TRUE ~ "None"
  ),
  ESCWA_ARAB_LOW = case_when(
    country %in% c("Somalia", "Syrian Arab Republic", "Sudan", "Yemen") ~ "ESCWA: Arab Low-Income Countries (LICs)",
    TRUE ~ "None"
  ),
  ESCWA_ARAB_HIGH = case_when(
    country %in% c("Bahrain", "Kuwait", "Oman", "Qatar", "Saudi Arabia", "United Arab Emirates") ~ "ESCWA: Arab High-Income Countries (HICs)",
    TRUE ~ "None"
  )) |>
  # UN country codes
  left_join(m49_codes, by = c("country_code" = "iso_alpha3_code")) |>
  mutate(
  # Special fixes for countries
  score = case_when(
    country_code == "HKG" & year == 2024 ~ 81.1,
    country_code == "PHL" & year == 2024 ~ 68.9,
    country_code == "SGP" & year == 2024 ~ 83.5,
    country_code == "GEO" & year == 2024 ~ 68.3,
    country_code == "ARE" & year == 2024 ~ 73.6,
    country_code == "POL" & year == 2024 ~ 81.9,
    TRUE ~ score
  ))

# Create simple average by regional aggregate and append to other regional aggregates
odin_all_groups |>
  group_by(Arab, year) |>
  summarize(SDG_17_18_1_ODIN_COV = mean(score, na.rm = TRUE)) |>
  ungroup() |>
  filter(Arab != "None") |>
  mutate(`RC Region` = "Arab", RC_UNSDCode = 98501) |>
  select(`RC Region`, `RC Region Name` = Arab, RC_UNSDCode, year, SDG_17_18_1_ODIN_COV) |>
  bind_rows(
    odin_all_groups |>
      group_by(ESCWA_GCC, year) |>
      summarize(SDG_17_18_1_ODIN_COV = mean(score, na.rm = TRUE)) |>
      ungroup() |>
      filter(ESCWA_GCC != "None") |>
      mutate(`RC Region` = "ESCWA_GCC", RC_UNSDCode = 98502) |>
      select(`RC Region`, `RC Region Name` = ESCWA_GCC, RC_UNSDCode, year, SDG_17_18_1_ODIN_COV)
  ) |>
  bind_rows(
    odin_all_groups |>
      group_by(ESCWA_MASHREQ, year) |>
      summarize(SDG_17_18_1_ODIN_COV = mean(score, na.rm = TRUE)) |>
      ungroup() |>
      filter(ESCWA_MASHREQ != "None") |>
      mutate(`RC Region` = "ESCWA_MASHREQ", RC_UNSDCode = 98503) |>
      select(`RC Region`, `RC Region Name` = ESCWA_MASHREQ, RC_UNSDCode, year, SDG_17_18_1_ODIN_COV)
  ) |>
  bind_rows(
    odin_all_groups |>
      group_by(ESCWA_MAGHREB, year) |>
      summarize(SDG_17_18_1_ODIN_COV = mean(score, na.rm = TRUE)) |>
      ungroup() |>
      filter(ESCWA_MAGHREB != "None") |>
      mutate(`RC Region` = "ESCWA_MAGHREB", RC_UNSDCode = 98504) |>
      select(`RC Region`, `RC Region Name` = ESCWA_MAGHREB, RC_UNSDCode, year, SDG_17_18_1_ODIN_COV)
  ) |>
  bind_rows(
    odin_all_groups |>
      group_by(ESCWA_LDC, year) |>
      summarize(SDG_17_18_1_ODIN_COV = mean(score, na.rm = TRUE)) |>
      ungroup() |>
      filter(ESCWA_LDC != "None") |>
      mutate(`RC Region` = "ESCWA_LDC", RC_UNSDCode = 98505) |>
      select(`RC Region`, `RC Region Name` = ESCWA_LDC, RC_UNSDCode, year, SDG_17_18_1_ODIN_COV)
  ) |>
  bind_rows(
    odin_all_groups |>
      group_by(ESCWA_CONFLICT, year) |>
      summarize(SDG_17_18_1_ODIN_COV = mean(score, na.rm = TRUE)) |>
      ungroup() |>
      filter(ESCWA_CONFLICT != "None") |>
      mutate(`RC Region` = "ESCWA_CONFLICT", RC_UNSDCode = 98506) |>
      select(`RC Region`, `RC Region Name` = ESCWA_CONFLICT, RC_UNSDCode, year, SDG_17_18_1_ODIN_COV)
  ) |>
  bind_rows(
    odin_all_groups |>
      group_by(ESCWA_NOCONFLICT_MID, year) |>
      summarize(SDG_17_18_1_ODIN_COV = mean(score, na.rm = TRUE)) |>
      ungroup() |>
      filter(ESCWA_NOCONFLICT_MID != "None") |>
      mutate(`RC Region` = "ESCWA_NOCONFLICT_MID", RC_UNSDCode = 98507) |>
      select(`RC Region`, `RC Region Name` = ESCWA_NOCONFLICT_MID, RC_UNSDCode, year, SDG_17_18_1_ODIN_COV)
  ) |>
  bind_rows(
    odin_all_groups |>
      group_by(ESCWA_ARAB_MID, year) |>
      summarize(SDG_17_18_1_ODIN_COV = mean(score, na.rm = TRUE)) |>
      ungroup() |>
      filter(ESCWA_ARAB_MID != "None") |>
      mutate(`RC Region` = "ESCWA_ARAB_MID", RC_UNSDCode = 98508) |>
      select(`RC Region`, `RC Region Name` = ESCWA_ARAB_MID, RC_UNSDCode, year, SDG_17_18_1_ODIN_COV)
  ) |>
  bind_rows(
    odin_all_groups |>
      group_by(ESCWA_ARAB_LOW, year) |>
      summarize(SDG_17_18_1_ODIN_COV = mean(score, na.rm = TRUE)) |>
      ungroup() |>
      filter(ESCWA_ARAB_LOW != "None") |>
      mutate(`RC Region` = "ESCWA_ARAB_LOW", RC_UNSDCode = 98509) |>
      select(`RC Region`, `RC Region Name` = ESCWA_ARAB_LOW, RC_UNSDCode, year, SDG_17_18_1_ODIN_COV)
  ) |>
  bind_rows(
    odin_all_groups |>
      group_by(ESCWA_ARAB_HIGH, year) |>
      summarize(SDG_17_18_1_ODIN_COV = mean(score, na.rm = TRUE)) |>
      ungroup() |>
      filter(ESCWA_ARAB_HIGH != "None") |>
      mutate(`RC Region` = "ESCWA_ARAB_HIGH", RC_UNSDCode = 98510) |>
      select(`RC Region`, `RC Region Name` = ESCWA_ARAB_HIGH, RC_UNSDCode, year, SDG_17_18_1_ODIN_COV)
  ) |>
  bind_rows(
    odin_all_groups |>
      filter(!is.na(global_code)) |>
      group_by(year) |>
      summarize(SDG_17_18_1_ODIN_COV = mean(score, na.rm = TRUE)) |>
      ungroup() |>
      mutate(`RC Region` = "World", `RC Region Name` = "World", RC_UNSDCode = 1) |>
      select(`RC Region`, `RC Region Name`, RC_UNSDCode, year, SDG_17_18_1_ODIN_COV)
  ) |>
  bind_rows(
    odin_all_groups |>
      filter(!is.na(global_code)) |>
      filter(str_detect(region, "Europe")) |>
      group_by(year) |>
      summarize(SDG_17_18_1_ODIN_COV = mean(score, na.rm = TRUE)) |>
      ungroup() |>
      mutate(`RC Region` = "Europe", `RC Region Name` = "Europe", RC_UNSDCode = 150) |>
      select(`RC Region`, `RC Region Name`, RC_UNSDCode, year, SDG_17_18_1_ODIN_COV)
  ) |>
  writexl::write_xlsx("Output Data/ESCWA 17.18.1 ODIN Coverage regional averages 2016-2024.xlsx", format_headers = FALSE)
