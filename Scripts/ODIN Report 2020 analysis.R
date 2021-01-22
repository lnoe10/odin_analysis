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
