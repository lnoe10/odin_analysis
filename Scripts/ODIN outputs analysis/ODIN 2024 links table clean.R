setwd("C:/Users/loren/Documents/R work")

library(tidyverse)

test <- read_csv("Input Data/ODIN Sources Data_reviewform.csv") |>
  filter(!is.na(`Country Code`))

all_fields <- test |>
  # All country code and section combinations
  expand(`Country Code`, Section) |>
  # Add Field Names
  left_join(test |> distinct(Section, `Field Name`))

clean_test <- test |>
  # drop all missing obs (where this drops the only record, we will merge it back with all fields)
  filter(!is.na(`Website Name`)) |>
  # Merge in max, will create additional rows where countries miss fields, now every country has at least 12 rows.
  full_join(all_fields) |>
  arrange(`Country Code`, Section)

clean_test |>
  writexl::write_xlsx("Output Data/ODIN Sources Data_reviewform.xlsx", format_headers = FALSE)
