library(tidyverse)
library(ggrepel)

setwd("C:/Users/loren/Documents/R work")

odin_2022_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 7) |>
  filter(!is.na(Region)) |>
  janitor::clean_names() |>
  filter(data_categories == "All Categories") |>
  select(year, region, region_code, country, country_code, overall_score, openness_subscore, machine_readable, non_proprietary, download_options, metadata_available, terms_of_use)

odin_2020_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 5) |>
  filter(!is.na(Region)) |>
  janitor::clean_names()|>
  filter(data_categories == "All Categories") |>
  select(year, region, region_code, country, country_code, overall_score, openness_subscore, machine_readable, non_proprietary, download_options, metadata_available, terms_of_use)

odin_2018_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 3) |>
  filter(!is.na(Region)) |>
  janitor::clean_names()|>
  filter(data_categories == "All Categories") |>
  select(year, region, region_code, country, country_code, overall_score, openness_subscore, machine_readable, non_proprietary, download_options, metadata_available, terms_of_use)

odin_2017_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 2) |>
  filter(!is.na(Region)) |>
  janitor::clean_names()|>
  filter(data_categories == "All Categories") |>
  select(year, region, region_code, country, country_code, overall_score, openness_subscore, machine_readable, non_proprietary, download_options, metadata_available, terms_of_use)

odin_2016_scores <- readxl::read_excel("Input data/odin scores historical.xlsx", sheet = 1) |>
  filter(!is.na(Region)) |>
  janitor::clean_names()|>
  filter(data_categories == "All Categories") |>
  select(year, region, region_code, country, country_code, overall_score, openness_subscore, machine_readable, non_proprietary, download_options, metadata_available, terms_of_use)

wb_codes <- read_csv("Input data/wb_countries_fy24.csv", show_col_types = F) %>% 
  janitor::clean_names() %>%
  filter(!is.na(region)) %>%
  mutate(income_group = fct_relevel(income_group, "Low income", "Lower middle income", "Upper middle income", "High income"))

odin_all <- odin_2022_scores |>
  bind_rows(odin_2020_scores) |>
  bind_rows(odin_2018_scores) |>
  bind_rows(odin_2017_scores) |>
  bind_rows(odin_2016_scores) |>
  group_by(country_code) |>
  mutate(datapoints = n()) |>
  ungroup() |>
  filter(datapoints == 5) |>
  left_join(wb_codes |> 
              select(country_code = code, income_group))

graph_data <- odin_all |>
  group_by(year) |>
  summarize(across(overall_score:terms_of_use, ~mean(.x, na.rm = TRUE))) |>
  ungroup() |>
  pivot_longer(cols = c(overall_score:terms_of_use), names_to = "element", values_to = "score") |>
  filter(!str_detect(element, "score")) |>
  mutate(element = str_to_title(str_replace_all(element, "_", " ")),
         element = case_when(
           element == "Machine Readable" ~ "Open 1: Machine Readable",
           element == "Non Proprietary" ~ "Open 2: Non Proprietary",
           element == "Download Options" ~ "Open 3: Download Options",
           element == "Metadata Available" ~ "Open 4: Metadata Available",
           TRUE ~ "Open 5: Terms Of Use"
         ))

data_ends <- graph_data |> 
  filter(year == 2022)

graph_data |>
  ggplot(aes(x = year, y = score, color = element, group = element)) +
  geom_line() +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(expand = expansion(mult = c(0, .6)), breaks = c(2016, 2018, 2020, 2022)) +
  labs(x = "", y = "Average score") +
  geom_text_repel(aes(label = element), data = data_ends, segment.color = "gray", segment.linetype = "dotted", segment.alpha = 0.8, nudge_x = 0.25, direction = "y", hjust = "left") +
  theme_odw()
ggsave("Graphs/Openness components 2016-2024.png", width = 6.9, height = 3.7, units = "in", dpi = 400)


# Full data, update and note dates on issue exports throughout!
full_source_data <- readxl::read_excel("Input Data/source_data_20240401.xlsx") |>
  janitor::clean_names()
full_summary_data <- readxl::read_excel("Input Data/summary_data_20240401.xlsx") |>
  janitor::clean_names()

combined <- full_source_data |>
  full_join(full_summary_data)

combined |>
  count(metadata, openness_4_metadata)

combined |>
  count(str_detect(metadata, "Date"))
combined |>
  count(str_detect(metadata, "Source"))
combined |>
  count(str_detect(metadata, "Definition"))

combined |> 
  separate_rows(data_format, sep = "; ") |>
  count(data_format, sort = TRUE, name = "Tally") |>
  filter(str_detect(data_format,"\\(r"))

combined |> 
  count(openness_1_machine_readable_data)

# Amount of options per record
combined |>
  mutate(data_format = str_replace(data_format, "DOCX \\(r, np\\)", "DOCX \\(np\\)"),
         count_machine_read = str_count(data_format, "\\(r")) |>
  count(count_machine_read) |>
  filter(!is.na(count_machine_read), count_machine_read > 0) |>
  mutate(share_of_records = n/sum(n))
