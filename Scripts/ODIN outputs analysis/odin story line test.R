library(tidyverse)

setwd("C:/Users/loren/Documents/R work")

odin_scores <- readxl::read_excel("Input Data/ODIN_scores_31Jan2024.xlsx") |>
  janitor::clean_names() |>
  filter(!is.na(region)) |>
  mutate(first_administrative_level = as.numeric(str_remove(first_administrative_level, "-")),
         second_administrative_level = as.numeric(str_remove(second_administrative_level, "-"))) |>
  pivot_longer(cols = c(indicator_coverage_and_disaggregation:overall_score), names_to = "element", values_to = "score") |>
  mutate(element = str_to_sentence(str_replace_all(element, "_", " ")))

odin_scores |>
  filter(str_detect(element, "score"), data_categories == "All Categories") |>
  group_by(element) |>
  summarize(mean_score = mean(score, na.rm = TRUE)) |>
  ungroup()

odin_scores |>
  filter(element == "Indicator coverage and disaggregation") |>
  group_by(data_categories) |>
  summarize(mean_score = mean(score, na.rm = TRUE)) |>
  ungroup() |>
  arrange(mean_score)
