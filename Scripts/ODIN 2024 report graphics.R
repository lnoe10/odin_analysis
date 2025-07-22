library(tidyverse)

setwd("C:/Users/loren/Documents/R work")


##### ODIN 2024 report graphics ####

# Overall scores since 2017
odin_all |>
  semi_join(countries_2017_2024) |>
  filter(data_category == "All Categories", str_detect(element, "score")) |>
  group_by(year, element) |>
  summarize(mean_score = mean(score, na.rm = TRUE)) |>
  ungroup() |>
  pivot_wider(names_from = year, values_from = mean_score) |>
  write_csv("Output Data/ODIN 2024 report Overall coverage openness score 2017-2024.csv", na = "")

# Overall score by region since 2022
odin_all |>
  semi_join(countries_2022_2024) |>
  left_join(m49_codes |> select(m49_region = region_name, country_code = iso_alpha3_code)) |>
  filter(data_category == "All Categories", element == "Overall score", !is.na(m49_region), year > 2021) |>
  group_by(m49_region, year) |>
  summarize(mean_score = mean(score, na.rm = TRUE)) |>
  ungroup() |>
  pivot_wider(names_from = m49_region, values_from = mean_score) |>
  write_csv("Output Data/ODIN 2022 - 2024 Overall score by M49 region.csv", na = "")

