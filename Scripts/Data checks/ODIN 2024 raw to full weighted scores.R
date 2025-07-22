library(tidyverse)

setwd("C:/Users/loren/Documents/R work")

#### 2022 scores as example ####

odin_2022_weight_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 7) |>
  filter(!is.na(Region)) |>
  janitor::clean_names() |>
  mutate(across(indicator_coverage_and_disaggregation:overall_score, ~as.numeric(str_remove(.x, "-")))) |>
  pivot_longer(indicator_coverage_and_disaggregation:overall_score, names_to = "element", values_to = "score") |>
  mutate(element = str_to_sentence(str_replace_all(element, "_", " ")))

odin_2022_raw_scores <- readxl::read_excel("Input Data/ODIN 2022 Raw Scores.xlsx") |>
  filter(!is.na(Region)) |>
  janitor::clean_names() |>
  mutate(macro_sector = case_when(
    data_categories %in% c("Population & vital statistics", "Education facilities",
                           "Education outcomes", "Health facilities", "Health outcomes",
                           "Reproductive health", "Food security & nutrition", "Gender statistics",
                           "Crime & justice", "Poverty & income")  ~ "Social statistics",
    data_categories %in% c("National accounts", "Labor", "Price indexes", "Government finance",
                           "Money & banking", "International trade", "Balance of payments") ~ "Economic and financial statistics",
    data_categories %in% c("Agriculture and land use", "Land use", "Resource use", "Energy", "Energy use", "Pollution", "Built environment") ~ "Environmental statistics",
    TRUE ~ NA_character_
  ),
  across(indicator_coverage_and_disaggregation:overall_score, ~as.numeric(str_remove(.x, "-")))) |>
  pivot_longer(indicator_coverage_and_disaggregation:overall_score, names_to = "element", values_to = "score") |>
  mutate(element = str_to_sentence(str_replace_all(element, "_", " ")),
         macro_element = case_when(
           element %in% c("Indicator coverage and disaggregation", "Data available last 5 years",
                          "Data available last 10 years", "First administrative level", "Second administrative level") ~ "Coverage elements",
           element %in% c("Non proprietary", "Metadata available", "Machine readable", "Download options", "Terms of use") ~ "Openness elements",
           TRUE ~ "Sub or Overall Scores"
         ))

##### Converting 2022 scoring matrix to check against 2022 scores ####
# go straight from scoring matrix
scoring_matrix_2022_large <- read_csv("Input Data/ODIN 2022 Scoring matrix Large countries.csv") |>
  janitor::clean_names() |>
  pivot_longer(indicator_coverage_and_disaggregation:overall_score, names_to = "element", values_to = "score_wt_large") |>
  mutate(element = str_to_sentence(str_replace_all(element, "_", " ")),
         macro_element = case_when(
           element %in% c("Indicator coverage and disaggregation", "Data available last 5 years",
                          "Data available last 10 years", "First administrative level", "Second administrative level") ~ "Coverage elements",
           element %in% c("Non proprietary", "Metadata available", "Machine readable", "Download options", "Terms of use") ~ "Openness elements",
           TRUE ~ "Sub or Overall Scores"
         ))

scoring_matrix_2022_small <- read_csv("Input Data/ODIN 2022 Scoring matrix Small countries.csv") |>
  janitor::clean_names() |>
  select(-starts_with("x")) |>
  filter(!is.na(data_categories)) |>
  pivot_longer(indicator_coverage_and_disaggregation:overall_score, names_to = "element", values_to = "score_wt_small") |>
  mutate(element = str_to_sentence(str_replace_all(element, "_", " ")),
         macro_element = case_when(
           element %in% c("Indicator coverage and disaggregation", "Data available last 5 years",
                          "Data available last 10 years", "First administrative level", "Second administrative level") ~ "Coverage elements",
           element %in% c("Non proprietary", "Metadata available", "Machine readable", "Download options", "Terms of use") ~ "Openness elements",
           TRUE ~ "Sub or Overall Scores"
         ))

small_countries_2022 <- readxl::read_excel("Input Data/small_countries_ODIN22.xlsx") |>
  mutate(country_code = countrycode::countrycode(country, "country.name", "iso3c"),
         country_code = case_when(
           country == "Micronesia" ~ "FSM",
           TRUE ~ country_code
         ),
         small_country = TRUE)

combined <- odin_2022_raw_scores |>
  full_join(scoring_matrix_2022_large |> select(data_categories, element, score_wt_large)) |>
  full_join(scoring_matrix_2022_small |> select(data_categories, element, score_wt_small)) |>
  left_join(small_countries_2022 |> select(country_code, small_country)) |>
  mutate(small_country = case_when(
    is.na(small_country) ~ FALSE,
    TRUE ~ small_country
  ))

element_cat_scores <- combined |>
  filter(!is.na(macro_sector), macro_element != "Sub or Overall Scores") |>
  mutate(weighted_score = case_when(
    small_country == TRUE ~ score*score_wt_small,
    TRUE ~ score*score_wt_large
    ))

element_sub_scores <- element_cat_scores |>
  group_by(country, country_code, region, region_code, data_categories, macro_element, macro_sector) |>
  summarize(weighted_score = sum(weighted_score, na.rm = TRUE)) |>
  ungroup() |>
  mutate(element = case_when(
    macro_element == "Coverage elements" ~ "Coverage subscore",
    TRUE ~ "Openness subscore"
  ))

overall_sub_scores <- element_sub_scores |>
  group_by(country, country_code, region, region_code, data_categories, macro_sector) |>
  summarize(weighted_score = sum(weighted_score, na.rm = TRUE)) |>
  ungroup() |>
  mutate(element = "Overall score")

all_cat_scores <- element_cat_scores |>
  bind_rows(element_sub_scores) |>
  bind_rows(overall_sub_scores)

macro_cat_scores <- all_cat_scores |>
  group_by(country, country_code, region, region_code, macro_sector, element) |>
  summarize(weighted_score = sum(weighted_score, na.rm = TRUE)) |>
  ungroup() |>
  mutate(data_categories = case_when(
    macro_sector == "Economic and financial statistics" ~ "Economic & financial statistics subscore",
    macro_sector == "Social statistics" ~ "Social statistics subscore",
    TRUE ~ "Environment subscore"
  ))

overall_cat_scores <- macro_cat_scores |>
  group_by(country, country_code, region, region_code, element) |>
  summarize(weighted_score = sum(weighted_score, na.rm = TRUE)) |>
  ungroup() |>
  mutate(data_categories = "All Categories")

all_weighted_scores <- all_cat_scores |>
  bind_rows(macro_cat_scores) |>
  bind_rows(overall_cat_scores) |>
  select(-c(macro_element, macro_sector, small_country, score_wt_large, score_wt_small)) |>
  mutate(macro_element = case_when(
    element %in% c("Indicator coverage and disaggregation", "Data available last 5 years",
                   "Data available last 10 years", "First administrative level", "Second administrative level") ~ "Coverage elements",
    element %in% c("Non proprietary", "Metadata available", "Machine readable", "Download options", "Terms of use") ~ "Openness elements",
    TRUE ~ "Sub or Overall Scores"
  ),
  macro_sector = case_when(
    data_categories %in% c("Population & vital statistics", "Education facilities",
                           "Education outcomes", "Health facilities", "Health outcomes",
                           "Reproductive health", "Food security & nutrition", "Gender statistics",
                           "Crime & justice", "Poverty & income")  ~ "Social statistics",
    data_categories %in% c("National accounts", "Labor", "Price indexes", "Government finance",
                           "Money & banking", "International trade", "Balance of payments") ~ "Economic and financial statistics",
    data_categories %in% c("Agriculture and land use", "Land use", "Resource use", "Energy", "Energy use", "Pollution", "Built environment") ~ "Environmental statistics",
    TRUE ~ NA_character_
  )) |>
  full_join(scoring_matrix_2022_large |> select(data_categories, element, score_wt_large)) |>
  full_join(scoring_matrix_2022_small |> select(data_categories, element, score_wt_small)) |>
  left_join(small_countries_2022 |> select(country_code, small_country)) |>
  mutate(small_country = case_when(
    is.na(small_country) ~ FALSE,
    TRUE ~ small_country
  ))

odin_final_scores <- all_weighted_scores |>
  mutate(test_score = case_when(
    small_country == TRUE ~ 100*(weighted_score/score_wt_small),
    TRUE ~ 100*(weighted_score/score_wt_large)
    ))

odin_final_scores |>
  count(test_score) |>
  arrange(desc(test_score))

testing_scores <- odin_final_scores |>
  select(-c(score, weighted_score, score_wt_large, score_wt_small, macro_element, macro_sector, year)) |>
  full_join(odin_2022_weight_scores) |>
  full_join(odin_2022_raw_scores |> select(region, region_code, country, country_code, data_categories, element, raw_score = score)) |>
  mutate(macro_sector = case_when(
    data_categories %in% c("Population & vital statistics", "Education facilities",
                           "Education outcomes", "Health facilities", "Health outcomes",
                           "Reproductive health", "Food security & nutrition", "Gender statistics",
                           "Crime & justice", "Poverty & income")  ~ "Social statistics",
    data_categories %in% c("National accounts", "Labor", "Price indexes", "Government finance",
                           "Money & banking", "International trade", "Balance of payments") ~ "Economic and financial statistics",
    data_categories %in% c("Agriculture and land use", "Land use", "Resource use", "Energy", "Energy use", "Pollution", "Built environment") ~ "Environmental statistics",
    TRUE ~ NA_character_
  ),
  macro_element = case_when(
    element %in% c("Indicator coverage and disaggregation", "Data available last 5 years",
                   "Data available last 10 years", "First administrative level", "Second administrative level") ~ "Coverage elements",
    element %in% c("Non proprietary", "Metadata available", "Machine readable", "Download options", "Terms of use") ~ "Openness elements",
    TRUE ~ "Sub or Overall Scores"
  ))

testing_scores |>
  mutate(score = janitor::round_half_up(score, 1),
         test_score = janitor::round_half_up(test_score, 1)) |>
  count(score!=test_score)

testing_scores |>
  mutate(score = janitor::round_half_up(score, 1),
         test_score = janitor::round_half_up(test_score, 1)) |>
  filter(score!= test_score) |>
  mutate(diff_scores = score - test_score) |>
  summarize(min_diff = min(diff_scores, na.rm = TRUE),
         max_diff = max(diff_scores, na.rm = TRUE),
         mean_diff = mean(diff_scores, na.rm = TRUE),
         median_diff = median(diff_scores, na.rm = TRUE))

##### 2024 scoring matrix to check raw scores ####

# Import 2024 raw scores (edit when we have raw scores)
#odin_2024_raw_scores <- readxl::read_excel("Input Data/ODIN 2022 Raw Scores.xlsx") |>
#  filter(!is.na(Region)) |>
#  janitor::clean_names() |>
#  mutate(macro_sector = case_when(
#    data_categories %in% c("Population & vital statistics", "Education facilities",
#                           "Education outcomes", "Health facilities", "Health outcomes",
#                           "Reproductive health", "Food security & nutrition", "Gender statistics",
#                           "Crime & justice", "Poverty & income")  ~ "Social statistics",
#    data_categories %in% c("National accounts", "Labor", "Price indexes", "Government finance",
#                           "Money & banking", "International trade", "Balance of payments") ~ "Economic and financial statistics",
#    data_categories %in% c("Agriculture and land use", "Land use", "Resource use", "Energy", "Energy use", "Pollution", "Built environment") ~ "Environmental statistics",
#    TRUE ~ NA_character_
#  ),
#  across(indicator_coverage_and_disaggregation:overall_score, ~as.numeric(str_remove(.x, "-")))) |>
#  pivot_longer(indicator_coverage_and_disaggregation:overall_score, names_to = "element", values_to = "score") |>
#  mutate(element = str_to_sentence(str_replace_all(element, "_", " ")),
#         macro_element = case_when(
#           element %in% c("Indicator coverage and disaggregation", "Data available last 5 years",
#                          "Data available last 10 years", "First administrative level", "Second administrative level") ~ "Coverage elements",
#           element %in% c("Non proprietary", "Metadata available", "Machine readable", "Download options", "Terms of use") ~ "Openness elements",
#           TRUE ~ "Sub or Overall Scores"
#         ))

# 2024 scoring matrix for large countries (slight change from 2022 to get rid of SN1 requirement)
scoring_matrix_2024_large <- read_csv("Input Data/ODIN 2024 Scoring matrix Large countries.csv") |>
  janitor::clean_names() |>
  pivot_longer(indicator_coverage_and_disaggregation:overall_score, names_to = "element", values_to = "score_wt_large") |>
  mutate(element = str_to_sentence(str_replace_all(element, "_", " ")),
         macro_element = case_when(
           element %in% c("Indicator coverage and disaggregation", "Data available last 5 years",
                          "Data available last 10 years", "First administrative level", "Second administrative level") ~ "Coverage elements",
           element %in% c("Non proprietary", "Metadata available", "Machine readable", "Download options", "Terms of use") ~ "Openness elements",
           TRUE ~ "Sub or Overall Scores"
         ))

# 2024 scoring matrix for small countries (it's the same as 2022)
scoring_matrix_2024_small <- read_csv("Input Data/ODIN 2022 Scoring matrix Small countries.csv") |>
  janitor::clean_names() |>
  select(-starts_with("x")) |>
  filter(!is.na(data_categories)) |>
  pivot_longer(indicator_coverage_and_disaggregation:overall_score, names_to = "element", values_to = "score_wt_small") |>
  mutate(element = str_to_sentence(str_replace_all(element, "_", " ")),
         macro_element = case_when(
           element %in% c("Indicator coverage and disaggregation", "Data available last 5 years",
                          "Data available last 10 years", "First administrative level", "Second administrative level") ~ "Coverage elements",
           element %in% c("Non proprietary", "Metadata available", "Machine readable", "Download options", "Terms of use") ~ "Openness elements",
           TRUE ~ "Sub or Overall Scores"
         ))

small_countries_2024 <- readxl::read_excel("Input Data/small_countries_ODIN24.xlsx") |>
  mutate(country_code = countrycode::countrycode(country, "country.name", "iso3c"),
         country_code = case_when(
           country == "Micronesia" ~ "FSM",
           TRUE ~ country_code
         ),
         small_country = TRUE)

# Crosswalk for full category names
category_long_names <- read_csv("Input Data/ODIN 2024 categories and category short name.csv") |>
  janitor::clean_names() |>
  mutate(category_short_name = case_when(
    is.na(category_short_name) ~ "NA",
    TRUE ~ category_short_name
  )) |>
  select(data_categories = category_long_name, category_code = category_short_name)

# Eric's raw scores
odin_2024_raw_scores <- read_csv("Input Data/ODIN 2024 Raw Scores March 28.csv") |>
  janitor::clean_names() |>
  mutate(category_code = case_when(
    is.na(category_short_name) ~ "NA",
    TRUE ~ category_short_name
  ),
  macro_sector = case_when(
    macro_sector == "Social Statistics" ~ "Social statistics",
    macro_sector == "Economic Statistics" ~ "Economic and financial statistics",
    TRUE ~ "Environmental statistics"
  ),
  across(indicator_coverage_and_disaggregation:terms_of_use, as.numeric)) |>
  pivot_longer(indicator_coverage_and_disaggregation:terms_of_use, names_to = "element", values_to = "raw_score") |>
  mutate(element = str_to_sentence(str_replace_all(element, "_", " ")),
         macro_element = case_when(
           element %in% c("Indicator coverage and disaggregation", "Data available last 5 years",
                          "Data available last 10 years", "First administrative level", "Second administrative level") ~ "Coverage elements",
           element %in% c("Non proprietary", "Metadata available", "Machine readable", "Download options", "Terms of use") ~ "Openness elements",
           TRUE ~ "Sub or Overall Scores"
         )) |>
  left_join(category_long_names) |>
  rename(country_code = iso_3, country = country_name)

# Merge together
combined_2024 <- odin_2024_raw_scores |>
  left_join(scoring_matrix_2024_large |> select(data_categories, element, score_wt_large)) |>
  left_join(scoring_matrix_2024_small |> select(data_categories, element, score_wt_small)) |>
  left_join(small_countries_2024 |> select(country_code, small_country)) |>
  mutate(small_country = case_when(
    is.na(small_country) ~ FALSE,
    TRUE ~ small_country
  ))

element_cat_scores_2024 <- combined_2024 |>
  filter(!is.na(macro_sector), macro_element != "Sub or Overall Scores") |>
  mutate(weighted_score = case_when(
    small_country == TRUE ~ raw_score*score_wt_small,
    TRUE ~ raw_score*score_wt_large
  ))

element_sub_scores_2024 <- element_cat_scores_2024 |>
  group_by(country, country_code, data_categories, macro_element, macro_sector) |>
  summarize(weighted_score = sum(weighted_score, na.rm = TRUE)) |>
  ungroup() |>
  mutate(element = case_when(
    macro_element == "Coverage elements" ~ "Coverage subscore",
    TRUE ~ "Openness subscore"
  ))

overall_sub_scores_2024 <- element_sub_scores_2024 |>
  group_by(country, country_code, data_categories, macro_sector) |>
  summarize(weighted_score = sum(weighted_score, na.rm = TRUE)) |>
  ungroup() |>
  mutate(element = "Overall score")

all_cat_scores_2024 <- element_cat_scores_2024 |>
  bind_rows(element_sub_scores_2024) |>
  bind_rows(overall_sub_scores_2024)

macro_cat_scores_2024 <- all_cat_scores_2024 |>
  group_by(country, country_code, macro_sector, element) |>
  summarize(weighted_score = sum(weighted_score, na.rm = TRUE)) |>
  ungroup() |>
  mutate(data_categories = case_when(
    macro_sector == "Economic and financial statistics" ~ "Economic & financial statistics subscore",
    macro_sector == "Social statistics" ~ "Social statistics subscore",
    TRUE ~ "Environment subscore"
  ))

overall_cat_scores_2024 <- macro_cat_scores_2024 |>
  group_by(country, country_code, element) |>
  summarize(weighted_score = sum(weighted_score, na.rm = TRUE)) |>
  ungroup() |>
  mutate(data_categories = "All Categories")

all_weighted_scores_2024 <- all_cat_scores_2024 |>
  bind_rows(macro_cat_scores_2024) |>
  bind_rows(overall_cat_scores_2024) |>
  select(-c(macro_element, macro_sector, small_country, score_wt_large, score_wt_small)) |>
  mutate(macro_element = case_when(
    element %in% c("Indicator coverage and disaggregation", "Data available last 5 years",
                   "Data available last 10 years", "First administrative level", "Second administrative level") ~ "Coverage elements",
    element %in% c("Non proprietary", "Metadata available", "Machine readable", "Download options", "Terms of use") ~ "Openness elements",
    TRUE ~ "Sub or Overall Scores"
  ),
  macro_sector = case_when(
    data_categories %in% c("Population & vital statistics", "Education facilities",
                           "Education outcomes", "Health facilities", "Health outcomes",
                           "Reproductive health", "Food security & nutrition", "Gender statistics",
                           "Crime & justice", "Poverty & income")  ~ "Social statistics",
    data_categories %in% c("National accounts", "Labor", "Price indexes", "Government finance",
                           "Money & banking", "International trade", "Balance of payments") ~ "Economic and financial statistics",
    data_categories %in% c("Agriculture and land use", "Land use", "Resource use", "Energy", "Energy use", "Pollution", "Built environment") ~ "Environmental statistics",
    TRUE ~ NA_character_
  )) |>
  full_join(scoring_matrix_2024_large |> select(data_categories, element, score_wt_large)) |>
  full_join(scoring_matrix_2024_small |> select(data_categories, element, score_wt_small)) |>
  left_join(small_countries_2024 |> select(country_code, small_country)) |>
  mutate(small_country = case_when(
    is.na(small_country) ~ FALSE,
    TRUE ~ small_country
  ))

odin_final_scores_2024 <- all_weighted_scores_2024 |>
  mutate(test_score = case_when(
    small_country == TRUE ~ 100*(weighted_score/score_wt_small),
    TRUE ~ 100*(weighted_score/score_wt_large)
  ))

odin_final_scores_2024 |>
  count(test_score) |>
  arrange(desc(test_score))

##### Comparing 2022 and 2024 #####
# overlapping countries only
countries_2022_2024 <- odin_final_scores_2024 |>
  distinct(country_code) |>
  mutate(odin_2022_country = TRUE) |>
  full_join(odin_2022_weight_scores |>
              distinct(country_code) |>
              mutate(odin_2024_country = TRUE)) |>
  filter(odin_2022_country == TRUE, odin_2024_country == TRUE) |>
  select(country_code)

# Compute change in average coverage, subscore, and overall score between 2022 and 2024
odin_2022_weight_scores |> 
  filter(data_categories == "All Categories", 
         element %in% c("Overall score", "Coverage subscore", "Openness subscore")) |> 
  semi_join(countries_2022_2024) |>
  group_by(element) |> 
  summarize(mean_score_2022 = mean(score, na.rm = TRUE), 
            median_score_2022 = median(score, na.rm = TRUE)) |> 
  ungroup() |> 
  full_join(odin_final_scores_2024 |> 
              filter(data_categories == "All Categories", 
                     element %in% c("Overall score", "Coverage subscore", "Openness subscore")) |> 
              semi_join(countries_2022_2024) |>
              group_by(element) |> 
              summarize(mean_score_2024 = mean(test_score, na.rm = TRUE), 
                        median_score_2024 = median(test_score, na.rm = TRUE)) |> 
              ungroup()) |> 
  mutate(diff_mean = mean_score_2024 - mean_score_2022, 
         diff_median = median_score_2024 - median_score_2022) |> 
  select(element, mean_score_2024, mean_score_2022, diff_mean, median_score_2024, median_score_2022, diff_median) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/ODIN 2024 to 2022 comparison Mar 28.xlsx", sheetName = "overall coverage openness", row.names = FALSE, append = FALSE)

# Comparing 2024 and 2022 scores for categorical sub-scores.
odin_2022_weight_scores |> 
  filter(str_detect(data_categories, "subscore"), 
         element %in% c("Overall score", "Coverage subscore", "Openness subscore")) |> 
  semi_join(countries_2022_2024) |>
  group_by(data_categories, element) |> 
  summarize(mean_score_2022 = mean(score, na.rm = TRUE), 
            median_score_2022 = median(score, na.rm = TRUE)) |> 
  ungroup() |> 
  full_join(odin_final_scores_2024 |> 
              filter(str_detect(data_categories, "subscore"), 
                     element %in% c("Overall score", "Coverage subscore", "Openness subscore")) |> 
              semi_join(countries_2022_2024) |>
              group_by(data_categories, element) |> 
              summarize(mean_score_2024 = mean(test_score, na.rm = TRUE), 
                        median_score_2024 = median(test_score, na.rm = TRUE)) |> 
              ungroup()) |> 
  mutate(diff_mean = mean_score_2024 - mean_score_2022, 
         diff_median = median_score_2024 - median_score_2022) |> 
  select(data_categories, element, mean_score_2024, mean_score_2022, diff_mean, median_score_2024, median_score_2022, diff_median) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/ODIN 2024 to 2022 comparison Mar 28.xlsx", sheetName = "categorical sub scores", row.names = FALSE, append = TRUE)

# Comparing 2024 and 2022 coverage scores for each category.
odin_2022_weight_scores |> 
  filter(element == "Coverage subscore") |> 
  semi_join(countries_2022_2024) |>
  group_by(data_categories) |> 
  summarize(mean_score_2022 = mean(score, na.rm = TRUE), 
            median_score_2022 = median(score, na.rm = TRUE)) |> 
  ungroup() |> 
  full_join(odin_final_scores_2024 |> 
              filter(element == "Coverage subscore") |>
              semi_join(countries_2022_2024) |>
              group_by(data_categories) |> 
              summarize(mean_score_2024 = mean(test_score, na.rm = TRUE), 
                        median_score_2024 = median(test_score, na.rm = TRUE)) |> 
              ungroup()) |> 
  mutate(diff_mean = mean_score_2024 - mean_score_2022, 
         diff_median = median_score_2024 - median_score_2022) |> 
  select(data_categories, mean_score_2024, mean_score_2022, diff_mean, median_score_2024, median_score_2022, diff_median) |>
  arrange(desc(diff_mean)) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/ODIN 2024 to 2022 comparison Mar 28.xlsx", sheetName = "category coverage scores", row.names = FALSE, append = TRUE)

# Comparing 2024 and 2022 element scores for just pollution, the biggest change
odin_2022_weight_scores |> 
  filter(data_categories == "Pollution") |> 
  semi_join(countries_2022_2024) |>
  group_by(element) |> 
  summarize(mean_score_2022 = mean(score, na.rm = TRUE), 
            median_score_2022 = median(score, na.rm = TRUE)) |> 
  ungroup() |> 
  full_join(odin_final_scores_2024 |> 
              filter(data_categories == "Pollution") |>
              semi_join(countries_2022_2024) |>
              group_by(element) |> 
              summarize(mean_score_2024 = mean(test_score, na.rm = TRUE), 
                        median_score_2024 = median(test_score, na.rm = TRUE)) |> 
              ungroup()) |> 
  mutate(diff_mean = mean_score_2024 - mean_score_2022, 
         diff_median = median_score_2024 - median_score_2022) |> 
  select(element, diff_mean, diff_median) |>
  arrange(desc(diff_mean))

# Biggest country changes by overall score
odin_2022_weight_scores |> 
  filter(element == "Overall score", data_categories == "All Categories") |> 
  semi_join(countries_2022_2024) |>
  rename(score_2022 = score) |>
  full_join(odin_final_scores_2024 |> 
              filter(element == "Overall score", data_categories == "All Categories") |>
              semi_join(countries_2022_2024) |>
              rename(score_2024 = test_score)) |> 
  mutate(diff_score = score_2024 - score_2022) |> 
  select(country, score_2024, score_2022, diff_score) |>
  arrange(desc(diff_score)) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/ODIN 2024 to 2022 comparison Mar 28.xlsx", sheetName = "country overall scores", row.names = FALSE, append = TRUE)

# Changes for individual country by category
odin_2022_weight_scores |> 
  filter(element == "Coverage subscore", country == "Singapore") |> 
  rename(score_2022 = score) |>
  full_join(odin_final_scores_2024 |> 
              filter(element == "Coverage subscore", country == "Singapore") |>
              rename(score_2024 = test_score)) |> 
  mutate(diff_score = score_2024 - score_2022) |> 
  select(country, data_categories, score_2024, score_2022, diff_score) |>
  arrange(desc(diff_score))

# Changes for individual element
odin_2022_weight_scores |> 
  filter(data_categories == "All Categories") |> 
  semi_join(countries_2022_2024) |>
  mutate(macro_element = case_when(
    element %in% c("Indicator coverage and disaggregation", "Data available last 5 years",
                   "Data available last 10 years", "First administrative level", "Second administrative level") ~ "Coverage elements",
    element %in% c("Non proprietary", "Metadata available", "Machine readable", "Download options", "Terms of use") ~ "Openness elements",
    TRUE ~ "Sub or Overall Scores"
  )) |>
  group_by(macro_element, element) |> 
  summarize(mean_score_2022 = mean(score, na.rm = TRUE), 
            median_score_2022 = median(score, na.rm = TRUE)) |> 
  ungroup() |> 
  full_join(odin_final_scores_2024 |> 
              filter(data_categories == "All Categories") |>
              semi_join(countries_2022_2024) |>
              group_by(macro_element, element) |> 
              summarize(mean_score_2024 = mean(test_score, na.rm = TRUE), 
                        median_score_2024 = median(test_score, na.rm = TRUE)) |> 
              ungroup()) |> 
  mutate(diff_mean = mean_score_2024 - mean_score_2022, 
         diff_median = median_score_2024 - median_score_2022) |> 
  select(macro_element, element, mean_score_2024, mean_score_2022, diff_mean, median_score_2024, median_score_2022, diff_median) |>
  as.data.frame() |>
  xlsx::write.xlsx("Output Data/ODIN 2024 to 2022 comparison Mar 28.xlsx", sheetName = "element scores for all categories", row.names = FALSE, append = TRUE)

