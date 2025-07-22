# Setup
library(tidyverse)
library(modelr)

setwd("C:/Users/loren/Documents/R work")

##### ODIN admin 1 pop and area #####

wb_data <- wbstats::wb_data(indicator = c("pop_total" = "SP.POP.TOTL", "surface_area" = "AG.SRF.TOTL.K2", "land_area" = "AG.LND.TOTL.K2"), start_date = 2021, end_date = 2021)

# Import sub-regions according to our subnational divisions
region_count <- readxl::read_excel("Input Data/ODIN_Subnational_List.xlsx") |>
  janitor::clean_names() |>
  mutate(country_code = countrycode::countrycode(country, "country.name", "iso3c"), .after = country,
         country_code = case_when(country == "Kosovo" ~ "XKX", TRUE ~ country_code)) |>
  left_join(wb_data |> select(country_code = iso3c, pop_total, land_area)) |>
  mutate(land_area = case_when(country_code == "XKX" ~ 10887, country_code == "AIA" ~ 91, country_code == "TWN" ~ 36197, TRUE ~ land_area),
         pop_total = case_when(country_code == "TWN" ~ 23570000, country_code == "AIA" ~ 15920, TRUE ~ pop_total),
         pop_per_region = pop_total/sum_area, land_per_region = land_area/sum_area)

region_count |>
  summarize(avg_admin1 = mean(sum_area, na.rm = TRUE), med_admin1 = median(sum_area, na.rm = TRUE), 
            max_admin1 = max(sum_area, na.rm = TRUE), min_admin1 = min(sum_area, na.rm = TRUE),
            avg_pop_per_region = mean(pop_per_region, na.rm = TRUE), med_pop_per_region = median(pop_per_region, na.rm = TRUE), 
            max_pop_per_region = max(pop_per_region, na.rm = TRUE), min_pop_per_region = min(pop_per_region, na.rm = TRUE), 
            avg_land_per_region = mean(land_per_region, na.rm = TRUE), med_land_per_region = median(land_per_region, na.rm = TRUE), 
            max_land_per_region = max(land_per_region, na.rm = TRUE), min_land_per_region = min(land_per_region, na.rm = TRUE))

region_count |>
  write_csv("Output Data/ODIN_region_pop_and_land.csv", na = "")


# ODIN scores
df <- readxl::read_excel("Input Data/ODIN_scores_Jan26.xlsx") |>
  janitor::clean_names() |>
  filter(!is.na(data_categories)) |>
  mutate(first_administrative_level = as.numeric(str_remove(first_administrative_level, "-")),
         second_administrative_level = as.numeric(str_remove(second_administrative_level, "-")))

df_high_level <- df |>
  filter(data_categories == "All Categories")

df_high_level |>
  summarize(correlation = cor(indicator_coverage_and_disaggregation, first_administrative_level))

df_high_level |>
  ggplot(aes(x = indicator_coverage_and_disaggregation, y = first_administrative_level)) + 
  geom_point() +
  geom_smooth(method = "lm")

m1 <- lm(first_administrative_level ~ indicator_coverage_and_disaggregation, data = df_high_level)

df_high_level <- df_high_level |>
  add_residuals(m1)
