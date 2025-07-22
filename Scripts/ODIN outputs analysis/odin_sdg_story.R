library(tidyverse)

setwd("C:/Users/loren/Documents/R work")

odin_2020 <- readxl::read_excel("Input Data/ODIN_2020_2022.xlsx", sheet = 1) |>
  janitor::clean_names() |>
  filter(!is.na(region_code), data_categories == "Coverage subscore") |>
  select(year, region, country, country_code, coverage_score = overall_score)

odin_2022 <- readxl::read_excel("Input Data/ODIN_2020_2022.xlsx", sheet = 2) |>
  janitor::clean_names() |>
  filter(!is.na(region_code), data_categories == "Coverage subscore") |>
  select(year, region, country, country_code, coverage_score = overall_score)

# Same number of countries
odin_20_22 <-
  odin_2020 |>
  bind_rows(odin_2022) |>
  group_by(country_code) |>
  summarize(data_point = n()) |>
  ungroup() |>
  filter(data_point == 2) |>
  distinct(country_code)


odin_2020 |>
  bind_rows(odin_2022) |>
  semi_join(odin_20_22) |>
  mutate(macro_region = case_when(
    str_detect(region, "Africa") ~ "Africa",
    str_detect(region, "Asia") ~ "Asia",
    str_detect(region, "Europe") ~ "Europe",
    str_detect(region, "America|Caribbean") ~ "Americas",
    TRUE ~ "Oceania"
  )) |>
  group_by(macro_region, region, year) |>
  summarize(median_score = median(coverage_score, na.rm = TRUE), 
            mean_score = mean(coverage_score, na.rm = TRUE)) |>
  ungroup() |>
  arrange(region, year) |>
  group_by(region) %>%
  mutate(diff = max(mean_score - lag(mean_score), na.rm = TRUE),
         rank_2022 = max(case_when(year == 2022 ~ mean_score, TRUE ~ NA_real_), na.rm = TRUE),
         rank_2020 = max(case_when(year == 2020 ~ mean_score, TRUE ~ NA_real_), na.rm = TRUE)) %>%
  ungroup() |>
  mutate(yr_label = case_when(
    region == "South America" ~ year,
    TRUE ~ NA_real_
  )) |>
  ggplot(aes(x = mean_score, y = fct_reorder(region, -rank_2020))) +
  geom_line(aes(group = region)) +
  geom_point(aes(color = as.factor(year)), size = 2) +
  geom_text(aes(label = as.factor(yr_label), color = as.factor(yr_label)), hjust = "outward") +
  facet_grid(macro_region ~ ., scales = "free_y") +
  labs(y = "", x = "Mean Coverage score for All Categories") +
  theme(legend.position = "off")

