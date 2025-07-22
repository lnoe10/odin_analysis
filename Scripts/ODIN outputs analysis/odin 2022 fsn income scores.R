library(tidyverse)

setwd("C:/Users/loren/Documents/R work")

odin_2022_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 7) |>
  filter(!is.na(Region)) |>
  janitor::clean_names() 

odin_2022_scores |>
  left_join(wb_codes |> 
              select(country_code = code, income_group)) |>
  filter(data_categories == "Food security & nutrition") |>
  group_by(income_group) |> 
  summarize(fsn_coverage_score = mean(coverage_subscore, na.rm = TRUE),
            fsn_openness_score = mean(openness_subscore, na.rm = TRUE),
            fsn_overall_score = mean(overall_score, na.rm = TRUE)) |>
  ungroup()
  

wb_codes <- read_csv("Input data/wb_countries_fy24.csv", show_col_types = F) %>% 
  janitor::clean_names() %>%
  filter(!is.na(region)) %>%
  # Venezuela is not classified in FY2022. Manually assign to last year's income classification "UM"
  mutate(income_group = case_when(
    str_detect(economy, "Venezuela") ~ "Upper middle income",
    TRUE ~ income_group
  ),
  income_group = fct_relevel(income_group, "Low income", "Lower middle income", "Upper middle income", "High income"))

