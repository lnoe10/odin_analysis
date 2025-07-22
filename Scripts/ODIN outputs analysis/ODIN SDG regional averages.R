setwd("C:/Users/loren/Documents/R work")

library(tidyverse)

odin_data <- readxl::read_excel("Input Data/SDG_database_17_18_1.xlsx", sheet = "Goal17") |>
  janitor::clean_names() |>
  filter(series_code == "SG_STT_ODIN") |>
  mutate(value = as.numeric(value))

m49_codes <- readxl::read_excel("Input Data/unsd_codes.xlsx") |>
  janitor::clean_names()

odin_data_codes <- odin_data |>
  left_join(m49_codes, by = c("geo_area_code" = "m49_code")) |>
  mutate(sdg_region = case_when(
    str_detect(sub_region_name, "Northern Africa|Western Asia") ~ "Northern Africa and Western Asia",
    str_detect(sub_region_name, "Central Asia|Southern Asia") ~ "Central and Southern Asia",
    str_detect(sub_region_name, "Eastern Asia|South-eastern Asia") ~ "Eastern and South-Eastern Asia",
    region_name == "Oceania" & sub_region_name != "Australia and New Zealand" ~ "Oceania (exc. Australia and New Zealand)",
    region_name == "Europe" | sub_region_name == "Northern America" ~ "Europe and Northern America",
    TRUE ~ NA_character_
  ))

global_avg <- odin_data_codes |>
  group_by(global_code, global_name) |>
  summarize(odin_avg = mean(value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(annex_sort_no = 1) |>
  filter(!is.na(global_code)) |>
  rename(GeoAreaCode = global_code, GeoAreaName = global_name) 

sub_regions <- odin_data_codes |>
  group_by(sub_region_code, sub_region_name) |>
  summarize(odin_avg = mean(value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(annex_sort_no = case_when(
    sub_region_name == "Sub-Saharan Africa" ~ 2,
    sub_region_name == "Northern Africa" ~ 4,
    sub_region_name == "Western Asia" ~ 5,
    sub_region_name == "Central Asia" ~ 7,
    sub_region_name == "Southern Asia" ~ 8,
    sub_region_name == "Eastern Asia" ~ 10,
    sub_region_name == "South-eastern Asia" ~ 11,
    sub_region_name == "Latin America and the Caribbean" ~ 12,
    sub_region_name == "Australia and New Zealand" ~ 14,
    sub_region_name == "Northern America" ~ 18,
    TRUE ~ NA_real_
  )) |>
  filter(!is.na(sub_region_name)) |>
  rename(GeoAreaCode = sub_region_code, GeoAreaName = sub_region_name)

intermediate_regions <- odin_data_codes |>
  group_by(intermediate_region_code, intermediate_region_name) |>
  summarize(odin_avg = mean(value, na.rm = TRUE)) |>
  ungroup() |>
  filter(!is.na(intermediate_region_name)) |>
  rename(GeoAreaCode = intermediate_region_code, GeoAreaName = intermediate_region_name)

continents <- odin_data_codes |>
  group_by(region_name, region_code) |>
  summarize(odin_avg = mean(value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(annex_sort_no = case_when(
    region_name == "Oceania" ~ 13,
    region_name == "Europe" ~ 17,
    TRUE ~ NA_real_
  )) |>
  filter(!is.na(region_name)) |>
  rename(GeoAreaName = region_name, GeoAreaCode = region_code)

sdg_regions <- odin_data_codes |>
  group_by(sdg_region) |>
  summarize(odin_avg = mean(value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(annex_sort_no = case_when(
    sdg_region == "Northern Africa and Western Asia" ~ 3,
    sdg_region == "Central and Southern Asia" ~ 6,
    sdg_region == "Eastern and South-Eastern Asia" ~ 9,
    sdg_region == "Oceania (exc. Australia and New Zealand)" ~ 15,
    sdg_region == "Europe and Northern America" ~ 16,
    TRUE ~ NA_real_
  )) |>
  filter(!is.na(sdg_region)) |>
  rename(GeoAreaName = sdg_region)
  

landlocked <- odin_data_codes |>
  group_by(land_locked_developing_countries_lldc) |>
  summarize(odin_avg = mean(value, na.rm = TRUE)) |>
  ungroup() |>
  filter(land_locked_developing_countries_lldc == "x") |>
  mutate(annex_sort_no = 19, GeoAreaName = "Landlocked developing countries", GeoAreaCode = 432) |>
  select(GeoAreaName, GeoAreaCode, odin_avg, annex_sort_no)

ldc <- odin_data_codes |>
  group_by(least_developed_countries_ldc) |>
  summarize(odin_avg = mean(value, na.rm = TRUE)) |>
  ungroup() |>
  filter(least_developed_countries_ldc == "x") |>
  mutate(annex_sort_no = 20, GeoAreaName = "Least developed countries", GeoAreaCode = 199) |>
  select(GeoAreaName, GeoAreaCode, odin_avg, annex_sort_no)
  
sids <- odin_data_codes |>
  group_by(small_island_developing_states_sids) |>
  summarize(odin_avg = mean(value, na.rm = TRUE)) |>
  ungroup() |>
  filter(small_island_developing_states_sids == "x") |>
  mutate(annex_sort_no = 21, GeoAreaName = "Small island developing States", GeoAreaCode = 722) |>
  select(GeoAreaName, GeoAreaCode, odin_avg, annex_sort_no)

annex_table <- global_avg |>
  select(region_name = GeoAreaName, odin_avg, annex_sort_no) |>
  bind_rows(continents |>
              filter(!is.na(annex_sort_no)) |>
              select(region_name = GeoAreaName, odin_avg, annex_sort_no)) |>
  bind_rows(sub_regions |>
              filter(!is.na(annex_sort_no)) |>
              select(region_name = GeoAreaName, odin_avg, annex_sort_no)) |>
  bind_rows(sdg_regions |>
              filter(!is.na(annex_sort_no)) |>
              rename(region_name = GeoAreaName)) |>
  bind_rows(landlocked |>
              select(region_name = GeoAreaName, odin_avg, annex_sort_no)) |>
  bind_rows(ldc |>
              select(region_name = GeoAreaName, odin_avg, annex_sort_no)) |>
  bind_rows(sids |>
              select(region_name = GeoAreaName, odin_avg, annex_sort_no)) |>
  arrange(annex_sort_no) |>
  mutate(odin_avg = round(odin_avg, 1)) |>
  select(Regions = region_name, `2022` = odin_avg)
  
all_regional_averages <- global_avg |>
  bind_rows(continents) |>
  bind_rows(sub_regions) |>
  bind_rows(intermediate_regions) |>
  bind_rows(landlocked) |>
  bind_rows(ldc) |>
  bind_rows(sids) |>
  select(-annex_sort_no)

all_region_for_export <- readxl::read_excel("Input Data/SDG_database_17_18_1.xlsx", sheet = "Goal17") |>
  filter(SeriesCode == "SG_STT_ODIN") |>
  distinct(Goal, Target, Indicator, SeriesCode, SeriesDescription, TimePeriod, Time_Detail, TimeCoverage, UpperBound, LowerBound, BasePeriod, Source, GeoInfoUrl, FootNote, Nature, `Reporting Type`, Units) |>
  bind_cols(all_regional_averages) |>
  relocate(c(GeoAreaCode, GeoAreaName), .before = TimePeriod) |>
  relocate(Value = odin_avg, .before = Time_Detail) |>
  mutate(Nature = "N")

# Export
annex_table |> 
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/SDG Report and SDG Database regional aggregates 17_18_1 - ODIN Coverage Index.xlsx", sheetName = "Statistical Annex 17.18.1 ODIN", row.names = FALSE, showNA = FALSE)

all_region_for_export |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/SDG Report and SDG Database regional aggregates 17_18_1 - ODIN Coverage Index.xlsx", sheetName = "SDG Database reg 17.18.1 ODIN", append = TRUE, row.names = FALSE, showNA = FALSE)
