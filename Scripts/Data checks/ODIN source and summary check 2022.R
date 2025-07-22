setwd("C:/Users/loren/Documents/R work")

library(tidyverse)

# original export
source_data <- readxl::read_excel("Input Data/source_data_20240307.xlsx") |>
  janitor::clean_names()
summary_data <- readxl::read_excel("Input Data/summary_data_20240307.xlsx") |>
  janitor::clean_names()

# Full data, update and note dates on issue exports throughout!
full_source_data <- readxl::read_excel("Input Data/source_data_20240419.xlsx") |>
  janitor::clean_names()
full_summary_data <- readxl::read_excel("Input Data/summary_data_20240423.xlsx") |>
  janitor::clean_names()

combined <- full_source_data |>
  full_join(full_summary_data)

# AIM data
aim_source_data <- readxl::read_excel("Input Data/source_data_20240517_aim.xlsx") |>
  janitor::clean_names()
aim_summary_data <- readxl::read_excel("Input Data/summary_data_20240517_aim.xlsx") |>
  janitor::clean_names()

aim_combined <- aim_source_data |>
  full_join(aim_summary_data)

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
  print(n = 46)

# Frequency
combined |>
  count(frequency == coverage_1_frequency)

# Year counts 5 year
last_five_years <- list("2016/2017", "2017/2018", "2018/2019", "2019/2020", "2020/2021", "2017", "2018", "2019", "2020", "2021")

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
  filter((coverage_2_data_within_5_years != count_5_years )|(is.na(years_of_data) & !is.na(coverage_2_data_within_5_years))) |>
  select(unique_record_id, indicator_record_number, category_long_name, indicator_name_long, years_of_data, coverage_2_data_within_5_years, checked_count = count_5_years) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 0418 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "Year 5 Availability")
  
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
      filter(is.na(manual_year_count), !is.na(coverage_3_data_within_10_years))
  ) |>
  select(unique_record_id, indicator_record_number, category_long_name, indicator_name_long, years_of_data, coverage_3_data_within_10_years, checked_count = manual_year_count) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 0418 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "Year 10 Availability", append = TRUE)

# Admin 1
combined |>
  count(geographic_level, coverage_4_data_at_first_admin_level)

combined |>
  mutate(admin1_check = case_when(
    str_detect(geographic_level, "Admin 1") ~ "Yes",
    is.na(geographic_level) ~ NA_character_,
    TRUE ~ "No"
  )) |>
  filter(coverage_4_data_at_first_admin_level != admin1_check | (is.na(coverage_4_data_at_first_admin_level) & !is.na(admin1_check)) | (!is.na(coverage_4_data_at_first_admin_level) & is.na(admin1_check))) |>
  select(unique_record_id, indicator_record_number, category_long_name, indicator_name_long, geographic_level, coverage_4_data_at_first_admin_level, admin1_check) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 0418 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "Admin 1 availability", append = TRUE)

# Admin 2
combined |>
  count(geographic_level, coverage_5_data_at_second_admin_level)

combined |>
  mutate(admin2_check = case_when(
    str_detect(geographic_level, "Admin 2") ~ "Yes",
    is.na(geographic_level) ~ NA_character_,
    TRUE ~ "No"
  )) |>
  filter(coverage_5_data_at_second_admin_level != admin2_check | (is.na(coverage_5_data_at_second_admin_level) & !is.na(admin2_check)) | (!is.na(coverage_5_data_at_second_admin_level) & is.na(admin2_check))) |>
  select(unique_record_id, indicator_record_number, category_long_name, indicator_name_long, geographic_level, coverage_5_data_at_second_admin_level, admin2_check) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 0419 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "Admin 2 availability", append = TRUE)

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
  filter(readability != openness_1_machine_readable_data) |>
  select(unique_record_id, indicator_record_number, category_long_name, indicator_name_long, data_format, openness_1_machine_readable_data, check_readability = readability) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 0313 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "Openness 1 machine-readability", append = TRUE)


# non-proprietary
combined |>
  mutate(nonprop = case_when(
    str_detect(data_format, "np\\)") ~ "Yes",
    is.na(data_format) ~ NA_character_,
    TRUE ~ "No")) |>
  count(nonprop, openness_2_non_proprietary_data)

combined |>
  mutate(nonprop = case_when(
    str_detect(data_format, "np\\)") ~ "Yes",
    is.na(data_format) ~ NA_character_,
    TRUE ~ "No")) |>
  filter(nonprop != openness_2_non_proprietary_data) |>
  select(unique_record_id, indicator_record_number, category_long_name, indicator_name_long, data_format, openness_2_non_proprietary_data, check_nonproprietary = nonprop) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 0313 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "Openness 2 nonproprietariness", append = TRUE)

# download options
combined |>
  count(openness_3_download_options, download_options)

combined |>
  mutate(check_download = case_when(
    str_count(download_options, ",") == 2 ~ 3,
    str_count(download_options, ",") == 1 & str_detect(download_options, "User Selected") ~ 2,
    (str_count(download_options, ",") == 0 & download_options != "No download options") | (str_count(download_options, ",") == 1 & !str_detect(download_options, "User Selected")) ~ 1,
    download_options == "No download options" ~ 0,
    TRUE ~ NA_real_
  )) |>
  filter(check_download != openness_3_download_options | (is.na(check_download) & !is.na(openness_3_download_options))) |>
  select(unique_record_id, indicator_record_number, category_long_name, indicator_name_long, download_options, openness_3_download_options, check_download) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 0419 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "Openness 3 download options", append = TRUE)


# metadata options
combined |>
  count(metadata, openness_4_metadata)
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
  xlsx::write.xlsx("Output Data/Summary file 0401 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "Openness 4 metadata", append = TRUE)

# Terms of Uses
combined |>
  count(terms_of_use, openness_5_terms_of_use)

combined |>
  filter(is.na(terms_of_use), openness_5_terms_of_use == 0) |>
  select(unique_record_id, indicator_record_number, category_long_name, indicator_name_long, terms_of_use, openness_5_terms_of_use) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/Summary file 0418 issues.xlsx", row.names = FALSE, showNA = FALSE, sheetName = "Openness 5 terms of use", append = TRUE)

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

# Small countries
small_countries <- readxl::read_excel("Input Data/small_countries_ODIN22.xlsx") |>
  mutate(iso_3 = countrycode::countrycode(country, "country.name", "iso3c"),
         iso_3 = case_when(
           country == "Micronesia" ~ "FSM",
           TRUE ~ iso_3
         ),
         small_country = TRUE)

# exception categories

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



#### Scoring Coverage element 2 last five years ####
# USE 2022 data to test
odin_2022_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 7) |>
  filter(!is.na(Region)) |>
  janitor::clean_names() |>
  mutate(across(indicator_coverage_and_disaggregation:overall_score, ~as.numeric(str_remove(.x, "-")))) |>
  pivot_longer(indicator_coverage_and_disaggregation:overall_score, names_to = "element", values_to = "score") |>
  mutate(element = str_to_sentence(str_replace_all(element, "_", " ")))

full_summary_data |>
  count(country_name, category_number, coverage_1_national_level_data_available)

# Creating synthetic number of years per indicator based on underlying records and scoring based on that
last_five_years <- list("2016/2017", "2017/2018", "2018/2019", "2019/2020", "2020/2021", "2017", "2018", "2019", "2020", "2021")

combined |>
  group_by(iso_3, country_name, broad_sector, category_long_name, category_short_name, category_number, indicator_number, indicator_name_long, indicator_name_short) |>
  ## Concatenate all years together per indicator
  #summarize(indicator_year = toString(years_of_data)) |>
  #ungroup() |>
  # Extract all years in list format
  # Concatenate all years together per indicator
  summarize(indicator_year = paste(years_of_data, collapse = ",")) |>
  ungroup() |>
  mutate(indicator_year = str_trim(str_remove_all(indicator_year, ",NA|NA,|NA")),
         indicator_year = case_when(
           indicator_year == "" ~ NA_character_,
           TRUE ~ indicator_year
         )) |>
  # create list of unique years
  mutate(manual_year_count = str_extract_all(indicator_year, pattern = "(?<!\\/)20[0-9]{2}(?!\\/)|20[0-9]{2}\\/20[0-9]{2}")) |>
  rowwise() |>
  mutate(year_list_unique = list(unique(manual_year_count)),
         count_5_years = sum(year_list_unique %in% last_five_years)) |>
  ungroup() |>
  # Encode contribution to score. Greater than 3 years = 1, less than 3 but 1 or more = 0.5 otherwise 0.
  mutate(cov_2_indicator = case_when(
    count_5_years >= 3 ~ 1,
    count_5_years >= 1 & count_5_years < 3 ~ 0.5,
    TRUE ~ 0
  )) |>
  # Average scores across category, then assign in-between numbers to 0.5
  group_by(iso_3, country_name, category_long_name, category_number) |>
  summarize(cov_2_cat = mean(cov_2_indicator, na.rm = TRUE)) |>
  ungroup() |>
  mutate(cov_2_cat = case_when(
    cov_2_cat > 0 & cov_2_cat < 1 ~ 50,
    TRUE ~ cov_2_cat*100
  )) |>
  left_join(odin_2022_scores |>
              filter(element %in% c("Data available last 5 years", "Indicator coverage and disaggregation")) |>
              pivot_wider(names_from = element, values_from = score, names_prefix = "ele_") |>
              select(country_code, data_categories, starts_with("ele")), by = c("iso_3" = "country_code", "category_long_name" = "data_categories")) |>
  filter(cov_2_cat != `ele_Data available last 5 years`) |>
  count(`ele_Indicator coverage and disaggregation` < cov_2_cat)

# 10 % error
# ~ 7.5 % if accounting for where Cov element would lead to top-coding

# Tallying by record
full_summary_data |>
  mutate(cov_2_record = case_when(
    coverage_2_data_within_5_years >= 3 ~ 1,
    coverage_2_data_within_5_years >= 1 & coverage_2_data_within_5_years < 3 ~ 0.5,
    TRUE ~ 0
  )) |>
  group_by(country_name, category_long_name, category_number, indicator_number, indicator_name_short) |>
  mutate(cov_2_indicator = max(cov_2_record, na.rm = TRUE)) |>
  ungroup() |>
  group_by(country_name, category_long_name, category_number) |>
  mutate(cov_2_cat = mean(cov_2_indicator, na.rm = TRUE)) |>
  ungroup() |>
  mutate(cov_2_cat = case_when(
    cov_2_cat > 0 & cov_2_cat < 1 ~ 0.5,
    TRUE ~ cov_2_cat
  )) |>
  filter(country_name == "Anguilla") |>
  select(country_name, category_long_name, indicator_number, coverage_2_data_within_5_years, cov_2_record, cov_2_indicator, cov_2_cat) |>
  print(n = Inf)
arrange(category_number, indicator_number) |>
  select(country_name, category_number, indicator_name_short, coverage_2_data_within_5_years, cov_2_score_record, cov_2_score_indicator) |>
  print(n = Inf)


full_summary_data |>
  mutate(cov_2_record = case_when(
    coverage_2_data_within_5_years >= 3 ~ 1,
    coverage_2_data_within_5_years >= 1 & coverage_2_data_within_5_years < 3 ~ 0.5,
    TRUE ~ 0
  )) |>
  group_by(country_name, iso_3, category_long_name, category_number, indicator_number, indicator_name_short) |>
  summarize(cov_2_indicator = max(cov_2_record, na.rm = TRUE)) |>
  ungroup() |>
  group_by(country_name, iso_3, category_long_name, category_number) |>
  summarize(cov_2_cat = mean(cov_2_indicator, na.rm = TRUE)) |>
  ungroup() |>
  mutate(cov_2_cat = case_when(
    cov_2_cat > 0 & cov_2_cat < 1 ~ 50,
    TRUE ~ cov_2_cat*100
  )) |>
  left_join(odin_2022_scores |>
              filter(element %in% c("Data available last 5 years", "Indicator coverage and disaggregation")) |>
              pivot_wider(names_from = element, values_from = score, names_prefix = "ele_") |>
              select(country_code, data_categories, starts_with("ele")), by = c("iso_3" = "country_code", "category_long_name" = "data_categories")) |>
  filter(cov_2_cat != `ele_Data available last 5 years`) |>
  count(`ele_Indicator coverage and disaggregation`, cov_2_cat)

# There are 148 observations where manual check of last five years does not match AIM and cov element 1 isn't holding back score.
# Check computation for these 148 country-categories. They may be problem with my algorithm or AIM's!

#### Scoring Coverage element 3 Availability of data in the last ten years ####
# USE 2022 data to test

# Creating synthetic number of years per indicator based on underlying records and scoring based on that

last_ten_years <- list("2011/2012", "2012/2013", "2013/2014", "2014/2015", "2015/2016", "2016/2017", "2017/2018", "2018/2019", "2019/2020", "2020/2021", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")

combined |>
  group_by(iso_3, country_name, broad_sector, category_long_name, category_short_name, category_number, indicator_number, indicator_name_long, indicator_name_short) |>
  # Concatenate all years together per indicator
  summarize(indicator_year = paste(years_of_data, collapse = ",")) |>
  ungroup() |>
  mutate(indicator_year = str_trim(str_remove_all(indicator_year, ",NA|NA,|NA")),
         indicator_year = case_when(
           indicator_year == "" ~ NA_character_,
           TRUE ~ indicator_year
         )) |>
  # create list of unique years
  mutate(manual_year_count = str_extract_all(indicator_year, pattern = "(?<!\\/)20[0-9]{2}(?!\\/)|20[0-9]{2}\\/20[0-9]{2}")) |>
  rowwise() |>
  mutate(year_list_unique = list(unique(manual_year_count)),
         count_10_years = sum(year_list_unique %in% last_ten_years)) |>
  ungroup() |>
  # Encode contribution to score. Greater than 6 years = 1, less than 6 but 1 or more = 0.5 otherwise 0.
  mutate(cov_3_indicator = case_when(
    count_10_years >= 6 ~ 1,
    count_10_years >= 1 & count_10_years < 6 ~ 0.5,
    TRUE ~ 0
  )) |>
  # Average scores across category, then assign in-between numbers to 0.5
  group_by(iso_3, country_name, category_long_name, category_number) |>
  summarize(cov_3_cat = mean(cov_3_indicator, na.rm = TRUE)) |>
  ungroup() |>
  mutate(cov_3_cat = case_when(
    cov_3_cat > 0 & cov_3_cat < 1 ~ 50,
    TRUE ~ cov_3_cat*100
  )) |>
  left_join(odin_2022_scores |>
              filter(element %in% c("Data available last 10 years", "Indicator coverage and disaggregation")) |>
              pivot_wider(names_from = element, values_from = score, names_prefix = "ele_") |>
              select(country_code, data_categories, starts_with("ele")), by = c("iso_3" = "country_code", "category_long_name" = "data_categories")) |>
  filter(cov_3_cat != `ele_Data available last 10 years`) |>
  count(`ele_Indicator coverage and disaggregation` < cov_3_cat)


# Tallying by record
full_summary_data |>
  mutate(cov_3_record = case_when(
    coverage_3_data_within_10_years >= 6 ~ 1,
    coverage_3_data_within_10_years >= 3 & coverage_3_data_within_10_years < 6 ~ 0.5,
    TRUE ~ 0
  )) |>
  # Collapse to indicator
  group_by(country_name, iso_3, category_long_name, category_number, indicator_number, indicator_name_short) |>
  summarize(cov_3_indicator = max(cov_3_record, na.rm = TRUE)) |>
  ungroup() |>
  # Collapse to category
  group_by(country_name, iso_3, category_long_name, category_number) |>
  summarize(cov_3_cat = mean(cov_3_indicator, na.rm = TRUE)) |>
  ungroup() |>
  # Assign 0.5 scores for all values between 0 and 1, as these will be where "at least 1 indicator" condition is met
  mutate(cov_3_cat = case_when(
    cov_3_cat > 0 & cov_3_cat < 1 ~ 50,
    TRUE ~ cov_3_cat*100
  )) |>
  left_join(odin_2022_scores |>
              filter(element %in% c("Data available last 10 years", "Indicator coverage and disaggregation")) |>
              pivot_wider(names_from = element, values_from = score, names_prefix = "ele_") |>
              select(country_code, data_categories, starts_with("ele")), by = c("iso_3" = "country_code", "category_long_name" = "data_categories")) |>
  filter(cov_3_cat != `ele_Data available last 10 years`, cov_3_cat <= `ele_Indicator coverage and disaggregation`) |>
  count(`ele_Indicator coverage and disaggregation`, cov_3_cat)

# There are 216 observations where manual check of last five years does not match AIM and cov element 1 isn't holding back score.
# Check computation for these 216 country-categories. They may be problem with my algorithm or AIM's!

# Coverage element 4 Availability of data at the first administrative level
full_summary_data |>
  # attach score to element summary at record level
  mutate(cov_4_record = case_when(
    coverage_4_data_at_first_admin_level == "Yes" ~ 1,
    coverage_4_data_at_first_admin_level == "No" ~ 0,
    TRUE ~ NA_real_
  )) |>
  # Collapse to indicator
  group_by(country_name, iso_3, category_long_name, category_number, indicator_number, indicator_name_short) |>
  summarize(cov_4_indicator = max(cov_4_record, na.rm = TRUE),
            cov_4_indicator = case_when(
              cov_4_indicator %in% c(0,1) ~ cov_4_indicator,
              TRUE ~ NA_real_
            )) |>
  ungroup() |>
  # Collapse to category
  group_by(country_name, iso_3, category_long_name, category_number) |>
  summarize(cov_4_cat = mean(cov_4_indicator, na.rm = TRUE)) |>
  ungroup() |>
  # Assign 0.5 scores for all values between 0 and 1, as these will be where "at least 1 indicator" condition is met
  mutate(cov_4_cat = case_when(
    cov_4_cat > 0 & cov_4_cat < 1 ~ 50,
    TRUE ~ cov_4_cat*100
  )) |>
  left_join(odin_2022_scores |>
              filter(element %in% c("First administrative level", "Indicator coverage and disaggregation")) |>
              pivot_wider(names_from = element, values_from = score, names_prefix = "ele_") |>
              select(country_code, data_categories, starts_with("ele")), by = c("iso_3" = "country_code", "category_long_name" = "data_categories")) |>
  filter(cov_4_cat != `ele_First administrative level`, cov_4_cat <= `ele_Indicator coverage and disaggregation`) |>
  count(`ele_Indicator coverage and disaggregation`, cov_4_cat)


# Coverage element 5 Availability of data at the second administrative level
# Coverage element 4 Availability of data at the first administrative level
full_summary_data |>
  # attach score to element summary at record level
  mutate(cov_5_record = case_when(
    coverage_5_data_at_second_admin_level == "Yes" ~ 1,
    coverage_5_data_at_second_admin_level == "No" ~ 0,
    TRUE ~ NA_real_
  )) |>
  # Collapse to indicator
  group_by(country_name, iso_3, category_long_name, category_number, indicator_number, indicator_name_short) |>
  summarize(cov_5_indicator = max(cov_5_record, na.rm = TRUE),
            cov_5_indicator = case_when(
              cov_5_indicator %in% c(0,1) ~ cov_5_indicator,
              TRUE ~ NA_real_
            )) |>
  ungroup() |>
  # Collapse to category
  group_by(country_name, iso_3, category_long_name, category_number) |>
  summarize(cov_5_cat = mean(cov_5_indicator, na.rm = TRUE)) |>
  ungroup() |>
  # Assign 0.5 scores for all values between 0 and 1, as these will be where "at least 1 indicator" condition is met
  mutate(cov_5_cat = case_when(
    cov_5_cat > 0 & cov_5_cat < 1 ~ 50,
    TRUE ~ cov_5_cat*100
  )) |>
  left_join(odin_2022_scores |>
              filter(element %in% c("Second administrative level", "Indicator coverage and disaggregation")) |>
              pivot_wider(names_from = element, values_from = score, names_prefix = "ele_") |>
              select(country_code, data_categories, starts_with("ele")), by = c("iso_3" = "country_code", "category_long_name" = "data_categories")) |>
  filter(cov_5_cat != `ele_Second administrative level`, cov_5_cat <= `ele_Indicator coverage and disaggregation`) |>
  count(`ele_Indicator coverage and disaggregation`, cov_5_cat)

#Openness element 1
full_summary_data |>
  # attach score to element summary at record level
  mutate(open_1_record = case_when(
    openness_1_machine_readable_data == "Yes" ~ 1,
    openness_1_machine_readable_data == "No" ~ 0,
    TRUE ~ NA_real_
  )) |>
  # Collapse to category
  group_by(country_name, iso_3, category_long_name, category_number) |>
  summarize(open_1_cat = mean(open_1_record, na.rm = TRUE)) |>
  ungroup() |>
  # Assign 0.5 scores for all values between 0 and 1, as these will be where "at least 1 indicator" condition is met
  mutate(open_1_cat = case_when(
    open_1_cat > 0 & open_1_cat < 1 ~ 50,
    TRUE ~ open_1_cat*100
  )) |>
  left_join(odin_2022_scores |>
              filter(element %in% c("Machine readable", "Indicator coverage and disaggregation")) |>
              pivot_wider(names_from = element, values_from = score, names_prefix = "ele_") |>
              select(country_code, data_categories, starts_with("ele")), by = c("iso_3" = "country_code", "category_long_name" = "data_categories")) |>
  filter(open_1_cat != `ele_Machine readable`) |>
  count()


#Openness element 2
full_summary_data |>
  # attach score to element summary at record level
  mutate(open_2_record = case_when(
    openness_2_non_proprietary_data == "Yes" ~ 1,
    openness_2_non_proprietary_data == "No" ~ 0,
    TRUE ~ NA_real_
  )) |>
  # Collapse to category
  group_by(country_name, iso_3, category_long_name, category_number) |>
  summarize(open_2_cat = mean(open_2_record, na.rm = TRUE)) |>
  ungroup() |>
  # Assign 0.5 scores for all values between 0 and 1, as these will be where "at least 1 indicator" condition is met
  mutate(open_2_cat = case_when(
    open_2_cat > 0 & open_2_cat < 1 ~ 50,
    TRUE ~ open_2_cat*100
  )) |>
  left_join(odin_2022_scores |>
              filter(element %in% c("Non proprietary", "Indicator coverage and disaggregation")) |>
              pivot_wider(names_from = element, values_from = score, names_prefix = "ele_") |>
              select(country_code, data_categories, starts_with("ele")), by = c("iso_3" = "country_code", "category_long_name" = "data_categories")) |>
  filter(open_2_cat != `ele_Non proprietary`) |>
  count()

#Openness element 3
full_summary_data |>
  # attach score to element summary at record level
  mutate(open_3_record = case_when(
    openness_3_download_options %in% c(2,3) ~ 1,
    openness_3_download_options == 1 ~ 0.5,
    openness_3_download_options == 0 ~ 0,
    TRUE ~ NA_real_
  )) |>
  # Collapse to category
  group_by(country_name, iso_3, category_long_name, category_number) |>
  summarize(open_3_cat = mean(open_3_record, na.rm = TRUE)) |>
  ungroup() |>
  # Assign 0.5 scores for all values between 0 and 1, as these will be where "at least 1 indicator" condition is met
  mutate(open_3_cat = case_when(
    open_3_cat > 0 & open_3_cat < 1 ~ 50,
    TRUE ~ open_3_cat*100
  )) |>
  left_join(odin_2022_scores |>
              filter(element %in% c("Download options", "Indicator coverage and disaggregation")) |>
              pivot_wider(names_from = element, values_from = score, names_prefix = "ele_") |>
              select(country_code, data_categories, starts_with("ele")), by = c("iso_3" = "country_code", "category_long_name" = "data_categories")) |>
  count(open_3_cat , `ele_Download options`)

#Openness element 4
full_summary_data |>
  # attach score to element summary at record level
  mutate(open_4_record = case_when(
    openness_4_metadata == 2 ~ 1,
    openness_4_metadata == 1 ~ 0.5,
    openness_4_metadata == 0 ~ 0,
    TRUE ~ NA_real_
  )) |>
  # Collapse to category
  group_by(country_name, iso_3, category_long_name, category_number) |>
  summarize(open_4_cat = mean(open_4_record, na.rm = TRUE)) |>
  ungroup() |>
  # Assign 0.5 scores for all values between 0 and 1, as these will be where "at least 1 indicator" condition is met
  mutate(open_4_cat = case_when(
    open_4_cat > 0 & open_4_cat < 1 ~ 50,
    TRUE ~ open_4_cat*100
  )) |>
  left_join(odin_2022_scores |>
              filter(element %in% c("Metadata available", "Indicator coverage and disaggregation")) |>
              pivot_wider(names_from = element, values_from = score, names_prefix = "ele_") |>
              select(country_code, data_categories, starts_with("ele")), by = c("iso_3" = "country_code", "category_long_name" = "data_categories")) |>
  count(open_4_cat , `ele_Metadata available`)

#Openness element 5
full_summary_data |>
  # attach score to element summary at record level
  mutate(open_5_record = case_when(
    openness_5_terms_of_use == 3 ~ 1,
    openness_5_terms_of_use == 2 ~ 0.5,
    openness_5_terms_of_use %in% c(0,1) ~ 0,
    TRUE ~ NA_real_
  )) |>
  # Collapse to category
  group_by(country_name, iso_3, category_long_name, category_number) |>
  summarize(open_5_cat = mean(open_5_record, na.rm = TRUE)) |>
  ungroup() |>
  # Assign 0.5 scores for all values between 0 and 1, as these will be where "at least 1 indicator" condition is met
  mutate(open_5_cat = case_when(
    open_5_cat > 0 & open_5_cat < 1 ~ 50,
    TRUE ~ open_5_cat*100
  )) |>
  left_join(odin_2022_scores |>
              filter(element %in% c("Terms of use", "Indicator coverage and disaggregation")) |>
              pivot_wider(names_from = element, values_from = score, names_prefix = "ele_") |>
              select(country_code, data_categories, starts_with("ele")), by = c("iso_3" = "country_code", "category_long_name" = "data_categories")) |>
  count(open_5_cat , `ele_Terms of use`)
