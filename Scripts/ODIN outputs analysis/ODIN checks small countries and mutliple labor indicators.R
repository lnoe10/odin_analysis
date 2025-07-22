library(tidyverse)

setwd("C:/Users/loren/Documents/R work")

odin_small <- read_csv("Input Data/odin countries less than 1500 sq km.csv") |>
  janitor::clean_names() |>
  filter(!is.na(countryname)) |>
  mutate(country_code = countrycode::countrycode(countryname, "country.name", "iso3c"),
         country_code = case_when(
           countryname == "Micronesia" ~ "FSM",
           TRUE ~ country_code
         )) |>
  select(countryname, country_code)

actual_small <- read_csv("Input Data/countries less than 1500 sq km.csv") |>
  janitor::clean_names() |>
  mutate(actually_small = 1)

actual_small |>
  full_join(odin_small) |>
  print(n = Inf)


combined |>
  filter(category_long_name == "Labor", data_available %in% c("Available", "Not Disaggregated")) |>
  group_by(country_name) |>
  mutate(available_labor_count = n_distinct(indicator_number)) |>
  ungroup() |>
  count(available_labor_count)

combined |>
  filter(category_number == 12) |>
  mutate(tag_record_scen1 = case_when(
    indicator_number %in% c(1, 3) & str_detect(disaggregations_available, "Sex") ~ 1,
    indicator_number == 2 & data_available == "Available" ~ 1,
    TRUE ~ 0
  )) |>
  arrange(country_name, iso_3, indicator_number, desc(tag_record_scen1)) |>
  group_by(country_name, iso_3, indicator_number) |>
  mutate(tag_record_scen1 = case_when(
    row_number() > 1 ~ 0,
    TRUE ~ tag_record_scen1
  )) |>
  ungroup() |>
  group_by(country_name, iso_3) |>
  mutate(sum_indicators = sum(tag_record_scen1, na.rm = TRUE)) |>
  ungroup() |>
  filter(sum_indicators == 3) |>
  arrange(country_name, iso_3, indicator_number, indicator_record_number) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/labor indicators with additional records.xlsx", row.names = F, sheetName = "12.1 and 12.3 sex disagg")

combined |>
  filter(category_number == 12) |>
  mutate(tag_record_scen1 = case_when(
    indicator_number %in% c(2, 3) & str_detect(disaggregations_available, "Sex") ~ 1,
    indicator_number == 1 & data_available == "Available" ~ 1,
    TRUE ~ 0
  )) |>
  arrange(country_name, iso_3, indicator_number, desc(tag_record_scen1)) |>
  group_by(country_name, iso_3, indicator_number) |>
  mutate(tag_record_scen1 = case_when(
    row_number() > 1 ~ 0,
    TRUE ~ tag_record_scen1
  )) |>
  ungroup() |>
  group_by(country_name, iso_3) |>
  mutate(sum_indicators = sum(tag_record_scen1, na.rm = TRUE)) |>
  ungroup() |>
  filter(sum_indicators == 3) |>
  arrange(country_name, iso_3, indicator_number, indicator_record_number) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/labor indicators with additional records.xlsx", row.names = F, sheetName = "12.2 and 12.3 sex disagg", append = TRUE)
