setwd("C:/Users/loren/Documents/R work")

library(tidyverse)

# Mapping ODIN 2024/2025 sign up countries against donor focus lists

signup_status <- readxl::read_excel("Input Data/ODIN Country Review Sign up_10 June.xlsx", sheet = 1) |>
  janitor::clean_names() |>
  mutate(country_code = countrycode::countrycode(country_name, "country.name", "iso3c"),
         country_code = case_when(
           country_name == "Kosovo" ~ "XKX",
           TRUE ~ country_code
         ),
         signed_up = "Yes")

focus_countries <- readxl::read_excel("Input Data/WB Hewlett BMGF focus countries ODIN 2024 sign up.xlsx", sheet = 1) |>
  janitor::clean_names() |>
  mutate(country_code = countrycode::countrycode(countryname, "country.name", "iso3c"))


combined <- signup_status |>
  full_join(focus_countries) |>
  mutate(world_bank_strengthening_gender_statistics = case_when(
    is.na(world_bank_strengthening_gender_statistics) ~ 0,
    TRUE ~ world_bank_strengthening_gender_statistics
  ),
  hewlett_inclusive_governance_and_eip = case_when(
    is.na(hewlett_inclusive_governance_and_eip) ~ 0,
    TRUE ~ hewlett_inclusive_governance_and_eip
  ),
  gates_foundation = case_when(
    is.na(gates_foundation) ~ 0,
    TRUE ~ gates_foundation
  ),
  iaeg_sdg = case_when(
    is.na(iaeg_sdg) ~ 0,
    TRUE ~ iaeg_sdg
  ),
  signed_up = case_when(
    is.na(signed_up) ~ "No",
    TRUE ~ signed_up
  ))

#SGS
combined |>
  count(world_bank_strengthening_gender_statistics, signed_up)

combined |>
  filter(world_bank_strengthening_gender_statistics == 1) |>
  select(country_name, countryname, signed_up) |>
  arrange(signed_up)

#Hewlett
combined |>
  count(hewlett_inclusive_governance_and_eip, signed_up)

combined |>
  filter(hewlett_inclusive_governance_and_eip == 1) |>
  select(country_name, countryname, signed_up) |>
  arrange(signed_up)

#Gates
combined |>
  count(gates_foundation, signed_up)

combined |>
  filter(gates_foundation == 1) |>
  select(country_name, countryname, signed_up) |>
  arrange(signed_up)

# IAEG_SDG
combined |>
  count(iaeg_sdg, signed_up)

combined |>
  filter(iaeg_sdg == 1) |>
  select(country_name, countryname, signed_up) |>
  arrange(signed_up, countryname) |>
  print(n = Inf)
