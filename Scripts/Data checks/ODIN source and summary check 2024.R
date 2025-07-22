setwd("C:/Users/loren/Documents/R work")

library(tidyverse)

# Full data, update and note dates on issue exports throughout!
full_source_data <- readxl::read_excel("Input Data/source_data_20250203.xlsx") |>
  janitor::clean_names()
full_summary_data <- readxl::read_excel("Input Data/summary_data_20250203.xlsx") |>
  janitor::clean_names()

combined <- full_source_data |>
  full_join(full_summary_data)

# Import required disaggregations
list_of_disaggregations <- read_csv("Input Data/ODIN 2024-2025 list of disaggregations.csv") |>
  janitor::clean_names()

combined <- combined |>
  left_join(list_of_disaggregations)

# Check data available column

# Remove problem records
combined <- combined |>
  filter(coverage_2_data_within_5_years != "N/A" | is.na(coverage_2_data_within_5_years)) |>
  mutate(across(coverage_2_data_within_5_years:coverage_3_data_within_10_years, as.numeric))

# Test elements
# National data available
combined |>
  count(national_level_data_available == coverage_1_national_level_data_available)

# Disaggregations available
combined |>
  count(disaggregations_available == coverage_1_other_disaggregations_available)

# Unique set of disaggregations
combined |>
  separate_rows(disaggregations_available, sep = ",") |>
  count(disaggregations_available) |>
  print(n = Inf)

# Check for duplicate disaggregations for same record
combined |>
  separate_rows(disaggregations_available, sep = ",") |> 
  group_by(unique_record_id, disaggregations_available) |> 
  mutate(num_disaggregation = n()) |> 
  ungroup() |> 
  filter(num_disaggregation > 1) |> 
  select(country_name, unique_record_id, disaggregations_available) |> 
  print(n = Inf)

# Export:
combined |>
  semi_join(combined |>
              separate_rows(disaggregations_available, sep = ",") |> 
              group_by(unique_record_id, disaggregations_available) |> 
              mutate(num_disaggregation = n()) |> 
              ungroup() |> 
              filter(num_disaggregation > 1) |> 
              distinct(country_name, unique_record_id, disaggregations_available) |>
              select(unique_record_id)) |>
  select(country_name, unique_record_id, disaggregations_available) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 1204 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "duplicate disaggregations")
   
# check proper disaggregations for each record
combined |>
  mutate(test_disaggregations_available = str_to_lower(disaggregations_available),
         test_disaggregations_available = str_split(test_disaggregations_available, ","),
         categorical_disaggregations = case_when(categorical_disaggregations == "not applicable" ~ NA_character_, 
                                                 TRUE ~ categorical_disaggregations), 
         list_disaggregations = str_split(categorical_disaggregations, ";  |; ")) |>
  rowwise() |>
  mutate(test_disag_sum = sum(!test_disaggregations_available %in% list_disaggregations)) |>
  filter(!is.na(disaggregations_available), !is.na(categorical_disaggregations), test_disag_sum > 0, !str_detect(categorical_disaggregations, "school stage|proficiency level")) |>
# category_long_name == "Agriculture and land use") |>
  select(unique_record_id, category_long_name, indicator_name_long, disaggregations_available, categorical_disaggregations, test_disag_sum) |>
  writexl::write_xlsx("Output Data/AIM wrong disaggregations 21 Jan.xlsx")
  
# Check supplementary information
combined |>
  left_join(readxl::read_excel("Input Data/summary_data_20241111.xlsx", sheet = 2) |>
              janitor::clean_names()) |>
  filter(!is.na(url)) |>
  count(data_available)

# count when there are no bulk download options for some but not all records for an indicator
combined |> 
  mutate(no_bulk = case_when(
#    str_detect(download_options, "No download options") ~ NA_real_,
    !str_detect(download_options, "Bulk Download") ~ 0,
    is.na(download_options) ~ NA_real_,
    TRUE ~  1
  )) |>
  group_by(country_name, indicator_name_short) |>
  mutate(status_download_options = mean(no_bulk, na.rm = TRUE)) |>
  ungroup() |>
  filter(status_download_options > 0, status_download_options < 1) |>
  arrange(year, iso_3, broad_sector, category_number, indicator_number, indicator_record_number) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output data/missing bulk download in indicator.xlsx", sheetName = "with No download options", append = TRUE)

# Instances where there is only one scorable record for an indicator 
# (excludes “Not Available” and “Not Disaggregated” records) and that 
# one record does not have bulk download selected. Almost always should 
# a single record for an indicator have bulk download selected. 
combined |>
  group_by(country_name, iso_3, indicator_name_short) |>
  mutate(num_records = n(), num_available = sum(data_available == "Available", na.rm = TRUE)) |>
  ungroup() |>
  filter(num_available == 1, !str_detect(download_options, "Bulk Download")) |>
  write_csv("Output Data/one scoreable record without bulk download options.csv", na = "")
  

# Frequency
combined |>
  count(frequency == coverage_1_frequency)

combined |>
  filter(frequency != coverage_1_frequency) |>
  select(unique_record_id, indicator_record_number, category_long_name, indicator_name_long, frequency, coverage_1_frequency) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 0821 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "Frequency")

# Year counts 5 year
last_five_years <- list("2018/2019", "2019/2020", "2020/2021", "2021/2022", "2022/2023", "2019", "2020", "2021", "2022", "2023")

combined |>
  mutate(manual_year_count = str_extract_all(years_of_data, pattern = "(?<!\\/)20[0-9]{2}(?!\\/)|20[0-9]{2}\\/20[0-9]{2}")) |>
  rowwise() |>
  mutate(count_5_years = sum(manual_year_count %in% last_five_years),
         diff = count_5_years - coverage_2_data_within_5_years) |>
  ungroup() |>
  count(count_5_years, coverage_2_data_within_5_years)

combined |>
  mutate(manual_year_count = str_extract_all(years_of_data, pattern = "(?<!\\/)20[0-9]{2}(?!\\/)|20[0-9]{2}\\/20[0-9]{2}")) |>
  rowwise() |>
  mutate(count_5_years = sum(manual_year_count %in% last_five_years),
         diff = count_5_years - coverage_2_data_within_5_years) |>
  ungroup() |>
  filter(!is.na(years_of_data), is.na(coverage_2_data_within_5_years)) |>
  select(unique_record_id, indicator_record_number, category_long_name, indicator_name_long, years_of_data, coverage_2_data_within_5_years, checked_count = count_5_years) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 0821 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "Year 5 Availability", append = TRUE)
  
# Year counts 10 year
combined |>
  mutate(manual_year_count = str_count(years_of_data, pattern = ",") + 1) |>
  count(manual_year_count, coverage_3_data_within_10_years)

combined |>
  mutate(manual_year_count = str_count(years_of_data, pattern = ",") + 1) |>
  filter(manual_year_count != coverage_3_data_within_10_years) |>
  bind_rows(
    combined |>
      mutate(manual_year_count = str_count(years_of_data, pattern = ",") + 1) |>
      filter(!is.na(years_of_data), is.na(coverage_3_data_within_10_years))
  ) |>
  select(unique_record_id, indicator_record_number, category_long_name, indicator_name_long, years_of_data, coverage_3_data_within_10_years, checked_count = manual_year_count) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 0821 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "Year 10 Availability", append = TRUE)

# Admin 1
combined |>
  count(geographic_level, coverage_4_data_at_first_admin_level)

combined |>
  mutate(admin1_check = case_when(
    str_detect(geographic_level, "Subnational 1") ~ "Yes",
    is.na(geographic_level) ~ NA_character_,
    TRUE ~ "No"
  )) |>
  filter(coverage_4_data_at_first_admin_level != admin1_check | (is.na(coverage_4_data_at_first_admin_level) & !is.na(admin1_check))) |>
  select(unique_record_id, indicator_record_number, category_long_name, indicator_name_long, geographic_level, coverage_4_data_at_first_admin_level, admin1_check) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 0821 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "Admin 1 availability", append = TRUE)

# Admin 2
combined |>
  count(geographic_level, coverage_5_data_at_second_admin_level)

combined |>
  mutate(admin2_check = case_when(
    str_detect(geographic_level, "Subnational 2") ~ "Yes",
    is.na(geographic_level) ~ NA_character_,
    TRUE ~ "No"
  )) |>
  filter(coverage_5_data_at_second_admin_level != admin2_check | (is.na(coverage_5_data_at_second_admin_level) & !is.na(admin2_check))) |>
  select(unique_record_id, indicator_record_number, category_long_name, indicator_name_long, geographic_level, coverage_5_data_at_second_admin_level, admin2_check) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 0821 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "Admin 2 availability", append = TRUE)

# machine-readable
combined |>
  mutate(readability = case_when(
    str_detect(data_format, "\\(r") ~ "Yes",
    is.na(data_format) ~ NA_character_,
    TRUE ~ "No")) |>
  count(readability, openness_1_machine_readable_data)

combined |>
  mutate(readability = case_when(
    str_detect(data_format, "\\(r") ~ "Yes",
    is.na(data_format) ~ NA_character_,
    TRUE ~ "No")) |>
  filter(readability != openness_1_machine_readable_data | (!is.na(readability) & is.na(openness_1_machine_readable_data)) | (is.na(readability) & !is.na(openness_1_machine_readable_data))) |>
  select(unique_record_id, indicator_record_number, category_long_name, indicator_name_long, data_format, openness_1_machine_readable_data, check_readability = readability) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 1204 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "Openness 1 machine-readability", append = TRUE)

# non-proprietary
combined |>
  mutate(nonprop = case_when(
    str_detect(data_format, "np\\)") ~ 1,
    is.na(data_format) ~ NA_real_,
    TRUE ~ 0)) |>
  count(nonprop, openness_2_non_proprietary_data)

combined |>
  mutate(nonprop = case_when(
    str_detect(data_format, "np\\)") ~ 1,
    is.na(data_format) ~ NA_real_,
    TRUE ~ 0)) |>
  filter(nonprop != openness_2_non_proprietary_data | (!is.na(nonprop) & is.na(openness_2_non_proprietary_data))| (is.na(nonprop) & !is.na(openness_2_non_proprietary_data))) |>
  select(unique_record_id, indicator_record_number, category_long_name, indicator_name_long, data_format, openness_2_non_proprietary_data, check_nonproprietary = nonprop) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 0121 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "Openness 2 nonproprietariness", append = TRUE)

# download options
combined |>
  count(openness_3_download_options, download_options)

combined |>
  mutate(check_download = case_when(
    str_count(download_options, ",") == 2 ~ 3,
    str_count(download_options, ",") == 1 & str_detect(download_options, "Bulk Download") ~ 2,
    (str_count(download_options, ",") == 0 & download_options != "No download options") | (str_count(download_options, ",") == 1 & !str_detect(download_options, "Bulk Download")) ~ 1,
    download_options == "No download options" ~ 0,
    TRUE ~ NA_real_
  )) |>
  filter(check_download != openness_3_download_options | (is.na(check_download) & !is.na(openness_3_download_options))) |>
  select(unique_record_id, indicator_record_number, category_long_name, indicator_name_long, download_options, openness_3_download_options, check_download) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 0918 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "Openness 3 download options", append = TRUE)


# metadata options
combined |>
  count(openness_4_metadata, metadata)
combined |>
  filter(metadata != "None", openness_4_metadata == 0) |>
  select(category_long_name, indicator_name_long, indicator_record_number, unique_record_id, dataset_name, metadata, openness_4_metadata)

combined |>
  mutate(check_metadata = case_when(
    str_count(metadata, ",") == 2 ~ 2,
    str_count(metadata, ",") == 1 ~ 1,
    !is.na(metadata) & metadata != "None" ~ 1,
    metadata == "None" ~ 0,
    TRUE ~ NA_real_
  )) |>
  filter(check_metadata != openness_4_metadata | (!is.na(metadata) & is.na(openness_4_metadata))) |>
  select(unique_record_id, indicator_record_number, category_long_name, indicator_name_long, metadata, openness_4_metadata, check_metadata) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 0821 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "Openness 4 metadata", append = TRUE)

# Terms of Uses
combined |>
  count(openness_5_terms_of_use, terms_of_use)

combined |>
  filter((!is.na(terms_of_use) & is.na(openness_5_terms_of_use)) | terms_of_use == "None selected") |>
  select(unique_record_id, indicator_record_number, category_long_name, indicator_name_long, terms_of_use, openness_5_terms_of_use) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 0821 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "Openness 5 terms of use", append = TRUE)

### Check missing data from export
# disaggregations available
combined |>
  count(national_level_data_available == "Yes", is.na(disaggregations_available))

combined |>
  filter(national_level_data_available == "Yes", is.na(disaggregations_available)) |>
  select(unique_record_id, indicator_record_number, category_long_name, indicator_name_long, national_level_data_available, disaggregations_available)

# Years of data
combined |>
  count(national_level_data_available == "Yes", is.na(years_of_data))

combined |>
  filter(national_level_data_available == "Yes", is.na(years_of_data)) |>
  select(unique_record_id, indicator_record_number, category_long_name, indicator_name_long, national_level_data_available, years_of_data)

# frequency
combined |>
  count(national_level_data_available == "Yes", is.na(frequency))

combined |>
  filter(national_level_data_available == "Yes", is.na(years_of_data)) |>
  select(unique_record_id, indicator_record_number, category_long_name, indicator_name_long, national_level_data_available, years_of_data)

# Geographic level
combined |>
  count(national_level_data_available == "Yes", is.na(geographic_level))

combined |>
  filter(national_level_data_available == "Yes", is.na(geographic_level)) |>
  select(unique_record_id, indicator_record_number, category_long_name, indicator_name_long, national_level_data_available, geographic_level)

# Data Format
combined |>
  count(national_level_data_available == "Yes", is.na(data_format))

combined |>
  filter(national_level_data_available == "Yes", is.na(data_format)) |>
  select(unique_record_id, indicator_record_number, category_long_name, indicator_name_long, national_level_data_available, data_format)

# Download Options
combined |>
  count(national_level_data_available == "Yes", is.na(download_options))

combined |>
  filter(national_level_data_available == "Yes", is.na(download_options)) |>
  select(unique_record_id, indicator_record_number, category_long_name, indicator_name_long, national_level_data_available, download_options)
# No download options

# Metadata
combined |>
  count(national_level_data_available == "Yes", is.na(metadata))

combined |>
  filter(national_level_data_available == "Yes", is.na(metadata)) |>
  select(unique_record_id, indicator_record_number, category_long_name, indicator_name_long, national_level_data_available, metadata)

# Terms of use
combined |>
  count(national_level_data_available == "Yes", is.na(terms_of_use))

combined |>
  filter(national_level_data_available == "Yes", is.na(terms_of_use)) |>
  select(unique_record_id, indicator_record_number, category_long_name, indicator_name_long, national_level_data_available, terms_of_use)
# Not available


#### Scoring attempts ####
full_summary_data |>
  filter(category_short_name == "PV") |>
  mutate(record_contrib = case_when(
    coverage_1_national_level_data_available == "Yes" & str_detect(coverage_1_other_disaggregations_available, "Sex") == TRUE ~ 1,
    indicator_name_short == "POPU" & str_detect(coverage_1_other_disaggregations_available, "5-Year Age Groups") & str_detect(coverage_1_other_disaggregations_available, "Sex") ~ 1,
    TRUE ~ 0
  )) |>
  select(country_name, indicator_name_short, coverage_1_other_disaggregations_available, record_contrib)

# Start with whether all data are available, since that will root out score 1.

# For score == 1
# All indicators
coverage_1_national_level_data_available == "Yes"
# AND All
str_detect(coverage_1_other_disaggregations_available, "Sex") == TRUE
# AND
indicator_name_short == "POPU" & str_detect(coverage_1_other_disaggregations_available, "5-Year Age Groups")

# For score == 0.5
indicator_name_short == "POPU" & str_detect(coverage_1_other_disaggregations_available, ",")
# OR
indicator_name_short == "BIRT" & !is.na(coverage_1_other_disaggregations_available)
# AND 
indicator_name_short == "MORT" & !is.na(coverage_1_other_disaggregations_available)

# 0.5 Publish indicator (1.1) with two categorical disaggregations or indicators (1.2) and (1.3) with one categorical disaggregation each.

# 1.0 Publish all indicators disaggregated by sex. Indicator (1.1) must also be disaggregated by 5-year age groups.

# Indicators where there are more than 6 records
combined |> 
  filter(national_level_data_available == "Yes" | data_available == "Available") |>
  group_by(indicator_name_long, country_name) |> 
  mutate(count_obs = n_distinct(indicator_record_number)) |>
  filter(count_obs > 6) |>
  arrange(country_name, indicator_name_long, indicator_record_number) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 1204 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "More than 6 records per indicator", append = TRUE)

# All three indicators in Health Facilities
combined |> 
  filter((national_level_data_available == "Yes"  | data_available == "Available"), category_long_name == "Health facilities") |>
  group_by(country_name) |> 
  summarize(count_ind = n_distinct(indicator_number)) |>
  filter(count_ind > 2) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 1204 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "All three indicators in Health facilities", append = TRUE)

# Countries that have all 3 indicators published for national accounts (since only 2 are required)
combined |> 
  filter((national_level_data_available == "Yes"  | data_available == "Available"), category_long_name == "National accounts") |>
  group_by(country_name) |> 
  summarize(count_ind = n_distinct(indicator_number)) |>
  filter(count_ind > 2) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 1204 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "All three indicators in National accounts", append = TRUE)

# Countries that have all 5 indicators published for reproductive health (only 4 required)
combined |> 
  filter((national_level_data_available == "Yes"  | data_available == "Available"), category_long_name == "Reproductive health") |>
  group_by(country_name) |> 
  summarize(count_ind = n_distinct(indicator_number)) |>
  filter(count_ind > 4) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 1204 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "All five indicators in Reproductive health", append = TRUE)

# Countries that have all 4 indicators published for food security and nutrition (only 3 required)
combined |> 
  filter((national_level_data_available == "Yes"  | data_available == "Available"), category_long_name == "Food security & nutrition") |>
  group_by(country_name) |> 
  summarize(count_ind = n_distinct(indicator_number)) |>
  filter(count_ind > 3) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 1204 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "All four indicators in Food security & nutrition", append = TRUE)

# Countries that have all 4 indicators published for resource use (only 3required)
combined |> 
  filter((national_level_data_available == "Yes"  | data_available == "Available"), category_long_name == "Resource use") |>
  group_by(country_name) |> 
  summarize(count_ind = n_distinct(indicator_number)) |>
  filter(count_ind > 3) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 1204 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "All four indicators in Resource use", append = TRUE)

# Countries that have all 3 indicators published for pollution (only 2 required)
combined |> 
  filter((national_level_data_available == "Yes"  | data_available == "Available"), category_long_name == "Pollution") |>
  group_by(country_name) |> 
  summarize(count_ind = n_distinct(indicator_number)) |>
  filter(count_ind > 2) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 1204 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "All three indicators in Pollution", append = TRUE)

# Countries that publish 4 or 5 indicators in Built Environment (only 3required)
combined |> 
  filter((national_level_data_available == "Yes"  | data_available == "Available"), category_long_name == "Built environment") |>
  group_by(country_name) |> 
  summarize(count_ind = n_distinct(indicator_number)) |>
  filter(count_ind > 3) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 1204 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "Four or five indicators in Built environment", append = TRUE)
