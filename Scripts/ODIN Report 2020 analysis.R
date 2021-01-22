#### SETUP ####

library(tidyverse)

# Read in and clean data
odin_scores <- read_csv("Input/ODIN_scores_2020.csv") %>%
  # convert all variable names to snakecase
  janitor::clean_names() %>%
  # Take out missing observations at bottom of the table, it's just metadata on copyright
  filter(!is.na(year)) %>%
  # Add 2018 scores
  bind_rows(read_csv("Input/ODIN_scores_2018.csv") %>%
              # convert all variable names to snakecase
              janitor::clean_names() %>%
              # Take out missing observations at bottom of the table, it's just metadata on copyright
              filter(!is.na(year))) %>%
  # Clean variables and data categories to be able to compare 2020 to 2018
  mutate(second_administrative_level = as.numeric(str_remove(second_administrative_level, "-")),
         data_categories = str_remove_all(data_categories, "\\n$|\\s$"),
         data_categories = case_when(
           data_categories == "Energy use" ~ "Energy",
           data_categories == "Land use" ~ "Agriculture & Land Use",
           TRUE ~ data_categories
         )) %>%
  # Convert to long
  pivot_longer(indicator_coverage_and_disaggregation:overall_score, names_to = "element", values_to = "score") %>%
  # Convert elements back to sentence case for easier reading
  mutate(element = str_to_sentence(str_replace_all(element, "_", " ")))

#### Analysis ####
# Start with shorter general discussion of scores by categories for 
# 2020 and trends since 2016. Are certain coverage or openness lacking 
# in certain categories more than others? Can do something similar to 
# previous year’s report. (Lorenz) 

# Replicate 2020 and 2018 part of Figure 2 from ODIN 2020/2021 Executive Summary
odin_scores %>% 
  filter(data_categories == "All Categories", 
         element %in% c("Overall score", "Coverage subscore", "Openness subscore")) %>% 
  group_by(element, year) %>% 
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup()

# Average coverage by sector
odin_scores %>%
  filter(element == "Coverage subscore") %>%
  group_by(data_categories) %>%
  summarize(mean_availability = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(mean_availability))

# Average openness by sector
odin_scores %>%
  filter(element == "Coverage subscore") %>%
  group_by(data_categories) %>%
  summarize(mean_availability = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(mean_availability))
  

# Health section- Focus on scores from the 3 health categories, 
# identify trends or interesting findings from 2020. Bring in Global 
# Health 50/50 COVID data or other COVID related data (Lorenz) 

# Economic section- Focus on gaps in economic and financial statistics. 
# What categories are lacking? Are there certain coverage or openness 
# elements that are lower across categories? Do SDDS, SDDS+ countries 
# score higher? (Lorenz) 