library(tidyverse)

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
  mutate(second_administrative_level = as.numeric(str_remove(second_administrative_level, "-")))
