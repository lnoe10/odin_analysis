library(tidyverse)

setwd("C:/Users/loren/Documents/R work")

odin_2022_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 7) |>
  filter(!is.na(Region)) |>
  janitor::clean_names() |>
  filter(data_categories == "All Categories") |>
  select(year, region, region_code, country, country_code, overall_score, coverage_subscore)

odin_2020_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 5) |>
  filter(!is.na(Region)) |>
  janitor::clean_names()|>
  filter(data_categories == "All Categories") |>
  select(year, region, region_code, country, country_code, overall_score, coverage_subscore)

odin_2018_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 3) |>
  filter(!is.na(Region)) |>
  janitor::clean_names()|>
  filter(data_categories == "All Categories") |>
  select(year, region, region_code, country, country_code, overall_score, coverage_subscore)

odin_2017_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 2) |>
  filter(!is.na(Region)) |>
  janitor::clean_names()|>
  filter(data_categories == "All Categories") |>
  select(year, region, region_code, country, country_code, overall_score, coverage_subscore)

odin_2016_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 1) |>
  filter(!is.na(Region)) |>
  janitor::clean_names()|>
  filter(data_categories == "All Categories") |>
  select(year, region, region_code, country, country_code, overall_score, coverage_subscore)

wb_codes <- read_csv("Input data/wb_countries_fy24.csv", show_col_types = F) %>% 
  janitor::clean_names() %>%
  filter(!is.na(region)) %>%
  mutate(income_group = fct_relevel(income_group, "Low income", "Lower middle income", "Upper middle income", "High income"))

odin_all <- odin_2022_scores |>
  bind_rows(odin_2020_scores) |>
  bind_rows(odin_2018_scores) |>
  bind_rows(odin_2017_scores) |>
  group_by(country_code) |>
  mutate(datapoints = n()) |>
  ungroup() |>
  filter(datapoints == 4) |>
  left_join(wb_codes |> 
              select(country_code = code, income_group))

odin_all |>
  group_by(year, income_group) |>
  summarize(mean_cov = mean(coverage_subscore, na.rm = TRUE)) |>
  ungroup() |>
  filter(!is.na(income_group), year >= 2017) |>
  ggplot(aes(x = year, y = mean_cov, color = income_group, group = income_group)) +
  geom_line()

odin_all |>
  group_by(year, income_group) |>
  summarize(mean_cov = mean(coverage_subscore, na.rm = TRUE)) |>
  ungroup() |>
  filter(!is.na(income_group)) |>
  pivot_wider(id_cols = income_group, names_from = year, values_from = mean_cov) |>
  write_csv("Output Data/ODIN coverage scores by income 2017-2022.csv", na = "")


odin_both |>
  group_by(year, income_group) |>
  summarize(mean_cov = mean(coverage_subscore, na.rm = TRUE)) |>
  ungroup() |>
  ggplot(aes(x = income_group, y = mean_cov, fill = year)) +
  geom_col(position = position_dodge2())

odin_both |>
  group_by(income_group, year) |>
  summarize(mean_cov = mean(coverage_subscore, na.rm = TRUE)) |>
  ungroup() |>
  filter(!is.na(income_group)) |>
  write_csv("Output Data/Mean coverage by income group by year.csv", na = "")

odin_both |>
  group_by(income_group, year) |>
  summarize(mean_cov = mean(coverage_subscore, na.rm = TRUE)) |>
  ungroup() |>
  group_by(income_group) |>
  mutate(diff = mean_cov - lag(mean_cov)) |>
  filter(!is.na(diff), !is.na(income_group)) |>
  ggplot(aes(x = income_group, y = diff)) +
  geom_col()

odin_both |>
  group_by(income_group, year) |>
  summarize(mean_cov = mean(coverage_subscore, na.rm = TRUE)) |>
  ungroup() |>
  group_by(income_group) |>
  mutate(diff = mean_cov - lag(mean_cov)) |>
  filter(!is.na(diff), !is.na(income_group)) |>
  select(income_group, `Difference between 2022 coverage and 2020 coverage scores` = diff) |>
  write_csv("Output Data/Difference in mean coverage by income group 2020-2022.csv", na = "")


crvs_gender <- read_csv("Input Data/CRVS tags.csv") |>
  mutate(all_agree = case_when(
    `Found in old list of CRVS 54 Indicator` == "Yes" & `Found in Mills 2017` == "Yes" & `Old 54 list CRVS feature` == "Yes" & `Old Mills list CRVS feature` != "0" ~ "All agree",
    `Found in old list of CRVS 54 Indicator` == "No" & `Found in Mills 2017` == "Yes" & `Old Mills list CRVS feature` != "0" ~ "Mills agree",
    `Found in old list of CRVS 54 Indicator` == "Yes" & `Found in Mills 2017` == "No" & `Old 54 list CRVS feature` == "Yes" ~ "Old 54 Gender-CRVS list agree",
    `Found in old list of CRVS 54 Indicator` == "No" & `Found in Mills 2017` == "No" ~ "Not in indicator list",
    
    TRUE ~ "No agreement"
  ))