library(tidyverse)

new_odin_scores <- readxl::read_excel("C:/Users/loren/Downloads/ODIN new-old comparison Feb 23.xlsx", sheet = 2) %>%
  janitor::clean_names() %>%
  filter(!is.na(region)) %>%
  # rename variables for appending previous years
  rename(data_categories = elements, coverage_subscore = coverage_subscores, openness_subscore = openness_subscores, overall_score = overall_subscores) %>%
  # Bring spelling of data categories in line with previous years
  mutate(data_categories = case_when(
    data_categories == "All categories" ~ "All Categories",
    data_categories == "Economic & Financial Statistics subscores" ~ "Economic & financial statistics subscore",
    data_categories == "Social Statistics subscore" ~ "Social statistics subscore",
    TRUE ~ data_categories
    ),
  first_administrative_level = as.numeric(str_remove(first_administrative_level, "-")),
  second_administrative_level = as.numeric(str_remove(second_administrative_level, "-")),
  # Fix trailing new line break in region
  region = str_remove_all(region, "\n|\r")) %>%
  pivot_longer(indicator_coverage_and_disaggregation:overall_score, names_to = "element", values_to = "new_score")
  

old_odin_scores <- readxl::read_excel("C:/Users/loren/Downloads/ODIN new-old comparison Feb 23.xlsx", sheet = 3) %>%
  janitor::clean_names() %>%
  filter(!is.na(region)) %>%
  # rename variables for appending previous years
  # rename(coverage_subscore = coverage_subscores, openness_subscore = openness_subscores, overall_score = overall_subscores) %>%
  # Bring spelling of data categories in line with previous years
  mutate(data_categories = case_when(
    data_categories == "All categories" ~ "All Categories",
    data_categories == "Economic & Financial Statistics subscores" ~ "Economic & financial statistics subscore",
    data_categories == "Social Statistics subscore" ~ "Social statistics subscore",
    TRUE ~ data_categories
  ),
  first_administrative_level = as.numeric(str_remove(first_administrative_level, "-")),
  second_administrative_level = as.numeric(str_remove(second_administrative_level, "-")),
  # Fix trailing new line break in region
  region = str_remove(region, "\n")) %>%
  pivot_longer(indicator_coverage_and_disaggregation:overall_score, names_to = "element", values_to = "old_score")

combined <- new_odin_scores %>%
  full_join(old_odin_scores) %>%
  mutate(
    # Convert elements back to sentence case for easier reading
    element = str_to_sentence(str_replace_all(element, "_", " ")),
    # Create macro sector group for use in graphing/analysis
    macro_sector = case_when(
      data_categories %in% c("Population & vital statistics", "Education facilities",
                             "Education outcomes", "Health facilities", "Health outcomes",
                             "Reproductive health", "Food security & nutrition", "Gender statistics",
                             "Crime & justice", "Poverty & income")  ~ "Social statistics",
      data_categories %in% c("National accounts", "Labor", "Price indexes", "Government finance",
                             "Money & banking", "International trade", "Balance of payments") ~ "Economic and financial statistics",
      data_categories %in% c("Agriculture and land use", "Land use", "Resource use", "Energy", "Energy use", "Pollution", "Built environment") ~ "Environmental statistics",
      TRUE ~ NA_character_
    ),
    # Create macro element group for use in graphing/analysis
    macro_element = case_when(
      element %in% c("Indicator coverage and disaggregation", "Data available last 5 years",
                     "Data available last 10 years", "First administrative level", "Second administrative level") ~ "Coverage elements",
      element %in% c("Non proprietary", "Metadata available", "Machine readable", "Download options", "Terms of use") ~ "Openness elements",
      TRUE ~ NA_character_
    ),
    # Create macro element group for use in graphing/analysis
    macro_region = case_when(
      region %in% c("Australia and New Zealand", "Pacific Islands") ~ "Oceania",
      str_detect(region, "Asia") ~ "Asia",
      str_detect(region, "Africa") ~ "Africa",
      str_detect(region, "Europe") ~ "Europe",
      TRUE ~ "America"
    ))

combined %>%
  filter(new_score != old_score, is.na(macro_element)) %>%
  count(element)

combined %>% 
  filter(new_score != old_score, element == "Openness subscore", data_categories == "All Categories") %>% 
  mutate(diff = old_score - new_score)



combined %>% 
  filter(new_score != old_score, element == "Openness subscore", data_categories == "All Categories") %>% 
  mutate(diff = old_score - new_score) %>%
  summarize(min_diff = min(diff, na.rm = TRUE), 
            mean_diff = mean(diff, na.rm = TRUE),
            median_diff = median(diff, na.rm = TRUE),
            max_diff = max(diff, na.rm = TRUE))
# Minimum difference in Openness subscore for all categories is 0.7
# Mean difference in Openness subscore for all categories is 3
# Median difference in Openness subscore for all categories is 2.6
# Maximum difference in Openness subscore for all categories is 8.6

combined %>% 
  filter(new_score != old_score, element == "Overall score", data_categories == "All Categories") %>% 
  mutate(diff = old_score - new_score) %>%
  summarize(min_diff = min(diff, na.rm = TRUE), 
            mean_diff = mean(diff, na.rm = TRUE),
            median_diff = median(diff, na.rm = TRUE),
            max_diff = max(diff, na.rm = TRUE))
# Minimum difference in Overall score for all categories is 0.4
# Mean difference in Overall score for all categories is 1.6
# Median difference in Overall score for all categories is 1.5
# Maximum difference in Overall score for all categories is 5.1

combined %>% 
  filter(new_score != old_score, element == "Openness subscore", data_categories == "Social statistics subscore") %>% 
  mutate(diff = old_score - new_score) %>%
  summarize(min_diff = min(diff, na.rm = TRUE), 
            mean_diff = mean(diff, na.rm = TRUE),
            median_diff = median(diff, na.rm = TRUE),
            max_diff = max(diff, na.rm = TRUE))
# Minimum difference in Openness subscore for Social statistics is 2
# Mean difference in Openness subscore for Social categories is 6.7
# Median difference in Openness subscore for Social categories is 7
# Maximum difference in Openness subscore for Social statistics is 15

combined %>% 
  filter(new_score != old_score, element == "Overall score", data_categories == "Social statistics subscore") %>% 
  mutate(diff = old_score - new_score) %>%
  summarize(min_diff = min(diff, na.rm = TRUE), 
            mean_diff = mean(diff, na.rm = TRUE),
            median_diff = median(diff, na.rm = TRUE),
            max_diff = max(diff, na.rm = TRUE))
# Minimum difference in Overall subscore for Social statistics is 1
# Mean difference in Overall subscore for Social categories is 3.5
# Median difference in Overall subscore for Social categories is 3.5
# Maximum difference in Overall subscore for Social statistics is 7.5

combined %>% 
  filter(new_score != old_score, element == "Overall score", data_categories == "Environment subscore") %>% 
  mutate(diff = old_score - new_score) %>%
  summarize(min_diff = min(diff, na.rm = TRUE), 
            mean_diff = mean(diff, na.rm = TRUE),
            median_diff = median(diff, na.rm = TRUE),
            max_diff = max(diff, na.rm = TRUE))
# Minimum difference in Overall subscore for Social statistics is 1
# Mean difference in Overall subscore for Social categories is 3.5
# Median difference in Overall subscore for Social categories is 3.5
# Maximum difference in Overall subscore for Social statistics is 7.5

combined %>% 
  filter(new_score != old_score, element == "Openness subscore", data_categories == "Environment subscore") %>% 
  mutate(diff = old_score - new_score) %>%
  summarize(min_diff = min(diff, na.rm = TRUE), 
            mean_diff = mean(diff, na.rm = TRUE),
            median_diff = median(diff, na.rm = TRUE),
            max_diff = max(diff, na.rm = TRUE))
# Minimum difference in Overall subscore for Social statistics is 1
# Mean difference in Overall subscore for Social categories is 3.5
# Median difference in Overall subscore for Social categories is 3.5
# Maximum difference in Overall subscore for Social statistics is 7.5

(combined %>% 
  filter(new_score != old_score, element == "Openness subscore", data_categories == "Environment subscore") %>% 
  mutate(diff = (old_score - new_score)*-1) %>% 
  arrange(diff) %>%
  select(country, diff) %>%
  head(n = 10) %>%
  write_csv("C:/Users/loren/Downloads/most affected countries.csv", na = ""))
