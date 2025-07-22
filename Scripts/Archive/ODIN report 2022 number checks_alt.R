library(tidyverse)

odin_scores_alt %>% 
  filter(year == 2022) %>% 
  select(country, country_code, element, data_categories, new_score = score, macro_sector, macro_element) %>% 
  full_join(odin_scores_report %>% 
              filter(year == 2022) %>% 
              select(country, country_code, element, data_categories, old_score = score)) %>% 
  group_by(macro_element, element, macro_sector, data_categories) %>% 
  summarize(median_old_score = median(old_score, na.rm = TRUE), 
            median_new_score = median(new_score, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(diff = median_old_score - median_new_score) %>% 
  filter(diff!=0) %>% 
  print(n = 91)

odin_scores_alt %>% 
  filter(year == 2022) %>% 
  select(country, country_code, element, data_categories, new_score = score, macro_sector, macro_element) %>% 
  full_join(odin_scores_report %>% 
              filter(year == 2022) %>% 
              select(country, country_code, element, data_categories, old_score = score)) %>%
  mutate(diff = old_score - new_score) %>%
  filter(!is.na(macro_element), !is.na(macro_sector)) %>%
  arrange(diff) %>%
  select(country, element, data_categories, old_score, new_score, diff) %>%
  filter(diff!=0, !is.na(diff)) %>%
  group_by(country) %>%
  mutate(num_diff = n()) %>%
  ungroup() %>%
  arrange(desc(num_diff), country) %>%
  print(n = 50)

odin_scores_newold <- read_csv("Input/ODIN_scores_2022_5MarAft.csv") %>%
  # convert all variable names to snakecase
  janitor::clean_names() %>%
  filter(!is.na(region)) %>%
  mutate(first_administrative_level = as.numeric(str_remove(first_administrative_level, "-")),
         second_administrative_level = as.numeric(str_remove(second_administrative_level, "-")),
         region = str_remove(region, "\n"),
         country = case_when(country_code == "TUR" ~ "Turkey", TRUE ~ country)) %>%
  pivot_longer(cols = c(indicator_coverage_and_disaggregation:overall_subscores), values_to = "new_score") %>%
  full_join(odin_scores_newold <- read_csv("Input/ODIN_scores_2022_5MarAft3pm.csv") %>%
              # convert all variable names to snakecase
              janitor::clean_names() %>%
              filter(!is.na(region)) %>%
              mutate(first_administrative_level = as.numeric(str_remove(first_administrative_level, "-")),
                     second_administrative_level = as.numeric(str_remove(second_administrative_level, "-")),
                     region = str_remove(region, "\n"),
                     country = case_when(country_code == "TUR" ~ "Turkey", TRUE ~ country)) %>%
              pivot_longer(cols = c(indicator_coverage_and_disaggregation:overall_subscores), values_to = "old_score"))

odin_scores_alt %>%
  filter(year == 2022)
  
#### Introduction ####

# Median score change between 2020 and 2022 for Overall, Openness, and Coverage
odin_scores_alt %>% 
  filter(element %in% c("Overall score", "Coverage subscore", "Openness subscore"), 
         year %in% c(2020, 2022), 
         data_categories == "All Categories") %>% 
  semi_join(odin_20_22) %>% 
  group_by(year, element) %>% 
  summarize(median_score = median(score, na.rm = TRUE),
            mean_score = mean(score, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = element, names_from = year, values_from = c(median_score, mean_score)) %>%
  mutate(median_score_change = median_score_2022 - median_score_2020, 
         mean_score_change = mean_score_2022 - mean_score_2020)

odin_scores_alt %>% 
  filter(element %in% c("Overall score", "Coverage subscore", "Openness subscore"), 
         year %in% c(2020, 2022), 
         data_categories == "All Categories") %>% 
  semi_join(odin_20_22) %>%
  pivot_wider(id_cols = c(country_code, element), names_from = year, names_prefix = "year_", values_from = score) %>%
  mutate(score_change = year_2022 - year_2020) %>%
  count(element, score_change<0)

odin_scores %>%
  filter(year == 2022, element == "Overall score", data_categories == "All Categories") %>%
  select(country_code, old_score = score) %>%
  full_join(odin_scores_alt %>% 
              filter(year == 2022, element == "Overall score", data_categories == "All Categories") %>%
              select(country_code, new_score = score)) %>%
  mutate(score_change = new_score - old_score,
         score_band_old = case_when(
           old_score <= 20 ~ 1,
           old_score > 20 & old_score <= 40 ~ 2,
           old_score > 40 & old_score <= 60 ~ 3,
           old_score > 60 & old_score <= 80 ~ 4,
           TRUE ~ 5
         ),
         score_band_new = case_when(
           new_score <= 20 ~ 1,
           new_score > 20 & new_score <= 40 ~ 2,
           new_score > 40 & new_score <= 60 ~ 3,
           new_score > 60 & new_score <= 80 ~ 4,
           TRUE ~ 5
         )) %>%
  filter(score_band_new != score_band_old)

# Ranking changes
odin_scores %>%
  filter(year == 2022, element == "Overall score", data_categories == "All Categories") %>%
  select(country_code, old_score = score) %>%
  full_join(odin_scores_alt %>% 
              filter(year == 2022, element == "Overall score", data_categories == "All Categories") %>%
              select(country_code, new_score = score, region)) %>%
  mutate(score_change = new_score - old_score,
         old_rank = rank(-old_score, ties.method = "min"),
         new_rank = rank(-new_score, ties.method = "max")) %>%
  group_by(region) %>%
  mutate(old_region_rank = rank(-old_score, ties.method = "min"),
         new_region_rank = rank(-new_score, ties.method = "min")) %>%
  ungroup() %>%
  mutate(country = countrycode::countrycode(country_code, "iso3c", "country.name"),
         country = case_when(country_code == "XKX" ~ "Kosovo", TRUE ~ country)) %>%
  write_csv("C:/Users/loren/Downloads/ODIN Mar 7 new data change rankings overall effect.csv", na = "")

# Figure 1: ODIN Overall Scores, 2022
(odin_scores_alt %>%
  filter(year == 2022, element == "Overall score", data_categories == "All Categories") %>%
  select(Year = year, Region = region, `Region Code` = region_code, Country = country, `Country Code` = country_code, `Overall score` = score) %>%
  write.xlsx("Output/ODIN 2022 Figure 1.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig1 data", row.names = FALSE))

# Figure 1 metadata
date_last_updated <- "8 March 2023"
responsible_agency <- "Open Data Watch - Licensed under a Creative Commons Attribution 4.0 license"
definition <- "The Overall ODIN score is the average of coverage and openness elements assessed during the ODIN process"

tibble(definition, date_last_updated, responsible_agency) %>%
  write.xlsx("Output/ODIN 2022 Figure 1.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig1 metadata", append = TRUE, row.names = FALSE)

# Change in overall score over time since 2016
odin_scores_alt %>% 
  filter(element %in% c("Overall score"), 
         data_categories == "All Categories") %>% 
  semi_join(odin_always) %>% 
  group_by(year) %>% 
  summarize(median_score = median(score, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(change = median_score - lag(median_score))
# Result for sample of countries always vs just last two years the same.

# Number of countries with reduced median scores in 2022
odin_scores_alt %>% 
  filter(element %in% c("Overall score"), 
         year %in% c(2020, 2022), 
         data_categories == "All Categories") %>% 
  semi_join(odin_20_22) %>%
  arrange(country_code, year) %>%
  pivot_wider(id_cols = c(country, country_code), names_from = year, values_from = score) %>%
  count(`2020` > `2022`)

# Range of 2022 scores
odin_scores_alt %>%
  filter(year == 2022, element == "Overall score",
         data_categories == "All Categories") %>%
  summarize(min_score = min(score, na.rm = TRUE),
            max_score = max(score, na.rm = TRUE))

#### Results at a Glance ####

# Number of countries that have been added or dropped
odin_scores_alt %>%
  filter(element %in% c("Overall score"), 
         year %in% c(2020, 2022), 
         data_categories == "All Categories") %>%
  pivot_wider(id_cols = c(country, country_code), names_from = year, values_from = score) %>%
  filter(is.na(`2020`)|is.na(`2022`))

# New countries in 2022
odin_scores_alt %>%
  filter(element %in% c("Overall score"), 
         data_categories == "All Categories") %>%
  group_by(country, country_code) %>%
  mutate(num_obs = n()) %>%
  filter(year == 2022, num_obs == 1)

# Difference in median openness score due to change in openness methodology
combined %>%
  semi_join(odin_20_22) %>%
  filter(element == "Openness subscore", 
         data_categories %in% c("All Categories", "Social statistics subscore", "Environment subscore", "Economic & financial statistics subscore")) %>%
  group_by(data_categories) %>%
  summarize(total_openness_old_method = median(new_score, na.rm = TRUE),
            total_openness_new_method = median(old_score, na.rm = TRUE)) %>%
  mutate(change = total_openness_new_method - total_openness_old_method)

# Graph of main three scores since 2016 (Figure 2)
odin_scores_alt %>%
  filter(element %in% c("Overall score", "Coverage subscore", "Openness subscore"), 
         data_categories == "All Categories") %>%
  semi_join(odin_always) %>%
  group_by(element, year) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = median_score, group = element, color = element, label = median_score)) +
  geom_line() +
  geom_text()

# Export data
(odin_scores_alt %>%
  filter(element %in% c("Overall score", "Coverage subscore", "Openness subscore"), 
         data_categories == "All Categories") %>%
  semi_join(odin_always) %>%
  group_by(element, year) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = element, names_from = year, values_from = median_score) %>%
  write_csv("Output/ODIN 2022 Figure 2 data.csv", na = ""))

odin_scores_alt %>%
  filter(element %in% c("Overall score", "Coverage subscore", "Openness subscore"), 
         data_categories == "All Categories") %>%
  semi_join(odin_always) %>%
  group_by(element, year) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = element, names_from = year, values_from = median_score) %>%
  write.xlsx("Output/ODIN 2022 Figure 2.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig2 data", row.names = FALSE)

# Figure 2 metadata
definition <- "The Overall, Coverage, and Openness ODIN scores presented in this chart represent the median of each subscore of ODIN based on a consistent set of 165 countries that have been assessed since 2016"

tibble(definition, date_last_updated, responsible_agency) %>%
  write.xlsx("Output/ODIN 2022 Figure 2.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig2 metadata", append = TRUE, row.names = FALSE)


# Graph of main three scores since 2016 (Figure 3)
odin_scores_alt %>%
  filter(element %in% c("Overall score", "Coverage subscore", "Openness subscore"), 
         data_categories %in% c("All Categories", "Social statistics subscore", "Environment subscore", "Economic & financial statistics subscore")) %>%
  semi_join(odin_always) %>%
  group_by(element, data_categories, year) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(element == "Overall score", str_detect(data_categories, "subscore")) %>%
  ggplot(aes(x = year, y = median_score, group = data_categories, color = data_categories, label = median_score)) +
  geom_line() +
  geom_text()

# Export data
(odin_scores_alt %>%
  filter(element %in% c("Overall score", "Coverage subscore", "Openness subscore"), 
         data_categories %in% c("All Categories", "Social statistics subscore", "Environment subscore", "Economic & financial statistics subscore")) %>%
  semi_join(odin_always) %>%
  group_by(element, data_categories, year) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(element == "Overall score", str_detect(data_categories, "subscore")) %>%
  pivot_wider(id_cols =  data_categories, names_from = year, values_from = median_score) %>%
  write_csv("Output/ODIN 2022 Figure 3 data.csv", na = ""))

odin_scores_alt %>%
  filter(element %in% c("Overall score", "Coverage subscore", "Openness subscore"), 
         data_categories %in% c("All Categories", "Social statistics subscore", "Environment subscore", "Economic & financial statistics subscore")) %>%
  semi_join(odin_always) %>%
  group_by(element, data_categories, year) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(element == "Overall score", str_detect(data_categories, "subscore")) %>%
  pivot_wider(id_cols =  data_categories, names_from = year, values_from = median_score) %>%
  write.xlsx("Output/ODIN 2022 Figure 3.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig3 data", row.names = FALSE)

# Figure 2 metadata
definition <- "The sectoral subscores presented in this chart represent the median of each subscore of ODIN based on a consistent set of 165 countries that have been assessed since 2016. The sectoral subscores are themselves averages of all the associated data categories. See the Methodology section for further information"

tibble(definition, date_last_updated, responsible_agency) %>%
  write.xlsx("Output/ODIN 2022 Figure 3.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig3 metadata", append = TRUE, row.names = FALSE)

# Three main sector subscores since 2020
odin_scores_alt %>%
  filter(element %in% c("Overall score", "Coverage subscore", "Openness subscore"), 
         data_categories %in% c("All Categories", "Social statistics subscore", "Environment subscore", "Economic & financial statistics subscore"),
         year %in% c(2020, 2022)) %>%
  semi_join(odin_20_22) %>%
  group_by(data_categories, element, year) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(data_categories, element), names_from = year, values_from = median_score) %>%
  mutate(score_change = `2022` - `2020`)

# Graph of median ODIN scores by income group for 2020 - 2022 (Figure 4)
odin_scores_alt %>%
  filter(element == "Overall score",
         data_categories == "All Categories",
         year %in% c(2020, 2022)) %>%
  semi_join(odin_20_22) %>%
  group_by(income_group, year) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(income_group)) %>%
  ggplot(aes(x = income_group, y = median_score, fill = as.factor(year), label = median_score)) +
  geom_col(position = position_dodge()) +
  geom_text(position = position_dodge(0.9))

# Export data
(odin_scores_alt %>%
  filter(element == "Overall score",
         data_categories == "All Categories",
         year %in% c(2020, 2022)) %>%
  semi_join(odin_20_22) %>%
  group_by(income_group, year) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(income_group)) %>%
  pivot_wider(id_cols = income_group, names_from = year, values_from = median_score) %>%
  write_csv("Output/ODIN 2022 Figure 4 data.csv", na = ""))

odin_scores_alt %>%
  filter(element == "Overall score",
         data_categories == "All Categories",
         year %in% c(2020, 2022)) %>%
  semi_join(odin_20_22) %>%
  group_by(income_group, year) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(income_group)) %>%
  pivot_wider(id_cols = income_group, names_from = year, values_from = median_score) %>%
  write.xlsx("Output/ODIN 2022 Figure 4.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig4 data", row.names = FALSE)

# Figure 4 metadata
definition <- "The scores in this figure represent the median overall score for all ODIN categories for the countries belonging to each income group. The income groups are World Bank FY23 income groups."

tibble(definition, date_last_updated, responsible_agency) %>%
  write.xlsx("Output/ODIN 2022 Figure 4.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig4 metadata", append = TRUE, row.names = FALSE)

# Graph of median ODIN scores by region 2016 vs 2022 (Figure 5)
# Level change in median scores dumbbell chart
odin_scores_alt %>% 
  semi_join(odin_always) %>%
  filter(data_categories == "All Categories", element == "Overall score", year %in% c(2016, 2022)) %>%
  group_by(macro_region, region, year) %>%
  summarize(med_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(region) %>%
  mutate(diff = max(med_score - lag(med_score), na.rm = TRUE),
         rank_2022 = max(case_when(year == 2022 ~ med_score, TRUE ~ NA_real_), na.rm = TRUE),
         rank_2016 = max(case_when(year == 2016 ~ med_score, TRUE ~ NA_real_), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(yr_label = case_when(
    region == "Eastern Asia" ~ year,
    TRUE ~ NA_real_
  )) %>%
  ggplot(aes(x = med_score, y = fct_reorder(region, -rank_2016))) +
  geom_line(aes(group = region)) +
  geom_point(aes(color = as.factor(year)), size = 2) +
  geom_text(aes(label = as.factor(yr_label), color = as.factor(yr_label)), hjust = "outward") +
  facet_grid(macro_region ~ ., scales = "free_y") +
  labs(y = "", x = "Median Overall score for All Categories") +
  theme(legend.position = "off")

# Numbers for graph
odin_scores_alt %>% 
  semi_join(odin_always) %>%
  filter(data_categories == "All Categories", element == "Overall score", year %in% c(2016, 2022)) %>%
  group_by(macro_region, region, year) %>%
  summarize(med_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>% 
  pivot_wider(id_cols = c(macro_region, region), names_from = year, values_from = med_score) %>% 
  mutate(score_change = `2022` - `2016`) %>% 
  arrange(desc(score_change))

# Export data
(odin_scores_alt %>% 
  semi_join(odin_always) %>%
  filter(data_categories == "All Categories", element == "Overall score", year %in% c(2016, 2022)) %>%
  group_by(macro_region, region, year) %>%
  summarize(med_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  write_csv("Output/ODIN 2022 Figure 5 data.csv", na = ""))

odin_scores_alt %>% 
  semi_join(odin_always) %>%
  filter(data_categories == "All Categories", element == "Overall score", year %in% c(2016, 2022)) %>%
  group_by(macro_region, region, year) %>%
  summarize(med_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  write.xlsx("Output/ODIN 2022 Figure 5.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig5 data", row.names = FALSE)

# Figure 5 metadata
definition <- "The scores in this figure represent the median overall score for all ODIN categories for the countries belonging to each UN sub-region for 2016 and 2022. This uses a consistent set of 165 countries assessed in each round of ODIN."

tibble(definition, date_last_updated, responsible_agency) %>%
  write.xlsx("Output/ODIN 2022 Figure 5.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig5 metadata", append = TRUE, row.names = FALSE)


# Performance of OGDI vs non-OGDI over time (Figure 6)
ogdi_totals_historical_alt %>%
  bind_rows(non_ogdi_totals_historical_alt) %>%
  ggplot(aes(x = as.factor(year), y = total_median, fill = ogdi)) +
  geom_col(position = position_dodge()) +
  facet_wrap(element~.) +
  labs(x = "", y = "Median Score") +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

# Export data
(ogdi_totals_historical_alt %>%
  bind_rows(non_ogdi_totals_historical_alt) %>%
  arrange(element, year, ogdi) %>%
  select(element, ogdi, year, total_median) %>%
  write_csv("Output/ODIN 2022 Figure 6 data.csv", na = ""))
  
ogdi_totals_historical_alt %>%
  bind_rows(non_ogdi_totals_historical_alt) %>%
  arrange(element, year, ogdi) %>%
  select(element, ogdi, year, total_median) %>%
  write.xlsx("Output/ODIN 2022 Figure 6.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig6 data", row.names = FALSE)

# Figure 6 metadata
definition <- "The scores in this figure represent the median overall, coverage, and openness scores for data categories relevant and not-relevant to gender for a consistent set of 165 countries assessed in each ODIN round."

tibble(definition, date_last_updated, responsible_agency) %>%
  write.xlsx("Output/ODIN 2022 Figure 6.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig6 metadata", append = TRUE, row.names = FALSE)


#### Main Findings ####
# Number of countries with reduced median openness or coverage scores in 2022
odin_scores_alt %>% 
  filter(element %in% c("Overall score", "Coverage subscore", "Openness subscore"), 
         year %in% c(2020, 2022), 
         data_categories == "All Categories") %>% 
  semi_join(odin_20_22) %>%
  arrange(element, country_code, year) %>%
  pivot_wider(id_cols = c(element, country, country_code), names_from = year, values_from = score) %>%
  count(element, `2020` > `2022`)

# Median score change between 2020 and 2022 for Overall, Openness, and Coverage (Figure 7)
odin_scores_alt %>% 
  filter(element %in% c("Overall score", "Coverage subscore", "Openness subscore"), 
         year %in% c(2020, 2022), 
         data_categories == "All Categories") %>% 
  semi_join(odin_20_22) %>% 
  group_by(year, element) %>% 
  summarize(median_score = median(score, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = element, names_from = year, values_from = median_score) %>%
  mutate(score_change = `2022` - `2020`)

# Export data
(odin_scores_alt %>% 
  filter(element %in% c("Overall score", "Coverage subscore", "Openness subscore"), 
         year %in% c(2020, 2022), 
         data_categories == "All Categories") %>% 
  semi_join(odin_20_22) %>% 
  group_by(year, element) %>% 
  summarize(median_score = median(score, na.rm = TRUE)) %>% 
  ungroup() %>%
  pivot_wider(id_cols = element, names_from = year, values_from = median_score) %>%
  filter(element != "Overall score") %>%
  write_csv("Output/ODIN 2022 Figure 7 data.csv", na = ""))

odin_scores_alt %>% 
  filter(element %in% c("Overall score", "Coverage subscore", "Openness subscore"), 
         year %in% c(2020, 2022), 
         data_categories == "All Categories") %>% 
  semi_join(odin_20_22) %>% 
  group_by(year, element) %>% 
  summarize(median_score = median(score, na.rm = TRUE)) %>% 
  ungroup() %>%
  pivot_wider(id_cols = element, names_from = year, values_from = median_score) %>%
  filter(element != "Overall score") %>%
  write.xlsx("Output/ODIN 2022 Figure 7.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig7 data", row.names = FALSE)

# Figure 7 metadata
definition <- "The scores in this figure represent the median coverage, and openness scores for all data categories for a consistent set of 183 countries assessed in both ODIN 2020 and 2022 rounds."

tibble(definition, date_last_updated, responsible_agency) %>%
  write.xlsx("Output/ODIN 2022 Figure 7.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig7 metadata", append = TRUE, row.names = FALSE)

# Median coverage and openness element scores 2020 and 2022 (Figure 8)
odin_scores_alt %>%
  filter(year %in% c(2020, 2022), data_categories == "All Categories",
         !element %in% c("Overall score", "Coverage subscore", "Openness subscore")) %>%
  semi_join(odin_20_22) %>%
  group_by(macro_element, element, year) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = element, y = median_score, fill = as.factor(year), label = round(median_score, 1))) +
  geom_col(position = position_dodge()) +
  geom_text(position = position_dodge(1.1)) +
  facet_wrap(~macro_element, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(limits = c(0, 100))

# Export data
(odin_scores_alt %>%
  filter(year %in% c(2020, 2022), data_categories == "All Categories",
         !element %in% c("Overall score", "Coverage subscore", "Openness subscore")) %>%
  semi_join(odin_20_22) %>%
  group_by(macro_element, element, year) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = year, names_from = element, values_from = median_score) %>%
  select(year, `Indicator coverage and disaggregation`, `Data available last 5 years`, `Data available last 10 years`,
         `First administrative level`, `Second administrative level`, `Machine readable`, `Non proprietary`, `Download options`,
         `Metadata available`, `Terms of use`) %>%
  write_csv("Output/ODIN 2022 Figure 8 data.csv", na = ""))

odin_scores_alt %>%
  filter(year %in% c(2020, 2022), data_categories == "All Categories",
         !element %in% c("Overall score", "Coverage subscore", "Openness subscore")) %>%
  semi_join(odin_20_22) %>%
  group_by(macro_element, element, year) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = year, names_from = element, values_from = median_score) %>%
  select(year, `Indicator coverage and disaggregation`, `Data available last 5 years`, `Data available last 10 years`,
         `First administrative level`, `Second administrative level`, `Machine readable`, `Non proprietary`, `Download options`,
         `Metadata available`, `Terms of use`) %>%
  write.xlsx("Output/ODIN 2022 Figure 8.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig8 data", row.names = FALSE)

# Figure 8 metadata
definition <- "The scores in this figure represent the median coverage or openness element scores for all data categories for a consistent set of 183 countries assessed in both ODIN 2020 and 2022 rounds."

tibble(definition, date_last_updated, responsible_agency) %>%
  write.xlsx("Output/ODIN 2022 Figure 8.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig8 metadata", append = TRUE, row.names = FALSE)

# Number of countries with TOUs dropping to 0
odin_scores_alt %>%
  filter(year %in% c(2020, 2022), data_categories == "All Categories", element == "Terms of use") %>%
  semi_join(odin_20_22) %>%
  count(year, score == 0)

odin_scores_alt %>%
  filter(year %in% c(2020, 2022), data_categories == "All Categories", element == "Terms of use") %>%
  semi_join(odin_20_22) %>%
  pivot_wider(id_cols = c(country, country_code), names_from = year, values_from = score) %>%
  count(`2022` < `2020`)

odin_scores_alt %>%
  filter(year %in% c(2020, 2022), data_categories == "All Categories", element == "Terms of use") %>%
  semi_join(odin_20_22) %>%
  group_by(year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE),
            median_score = median(score, na.rm = TRUE)) %>%
  ungroup()



odin_scores_alt %>%
  filter(year %in% c(2020, 2022), data_categories == "All Categories", element == "Terms of use") %>%
  semi_join(odin_20_22) %>%
  ggplot(aes(x = score)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(as.factor(year)~.)

odin_scores_alt %>%
  filter(year %in% c(2020, 2022), data_categories == "All Categories", element == "Terms of use") %>%
  semi_join(odin_20_22) %>%
  arrange(year, score) %>%
  group_by(year) %>%
  mutate(rank = row_number()) %>%
  select(country, year, score, rank) %>%
  filter(rank > 90, rank < 94)

# Do the same countries have 0s for TOU?
odin_scores_alt %>%
  filter(year %in% c(2020, 2022), data_categories == "All Categories", element == "Terms of use") %>%
  semi_join(odin_20_22) %>%
  pivot_wider(id_cols = c(country, country_code), names_from = year, values_from = score) %>%
  count(`2022` == 0, `2020` == 0)

#### Sectoral Analysis
odin_scores_alt %>%
  semi_join(odin_20_22) %>%
  filter(year %in% c(2020, 2022), str_detect(data_categories, "subscore"), str_detect(element, "score")) %>%
  group_by(year, data_categories, element) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(data_categories, element), names_from = year, values_from = median_score) %>%
  mutate(change_score = `2022` - `2020`)

# Category median chart Figure 9
category_dumbbell_chart <- function(x){
  odin_scores_alt %>% 
    semi_join(odin_20_22) %>%
    mutate(element = case_when(
      element == "Coverage subscore" ~ "Coverage score",
      element == "Openness subscore" ~ "Openness score",
      TRUE ~ element)) %>%
    filter(element %in% c("Overall score", "Coverage score", "Openness score"), year %in% c(2020, 2022), !data_categories %in% c("All Categories", "Economic & financial statistics subscore",
                                                                                                                                 "Environment subscore", "Social statistics subscore")) %>%
    group_by(element, macro_sector, data_categories, year) %>%
    summarize(med_score = median(score, na.rm = TRUE),
              mean_score = mean(score, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(element, data_categories) %>%
    mutate(rank_2022 = max(case_when(year == 2022 ~ med_score, TRUE ~ NA_real_), na.rm = TRUE),
           rank_2020 = max(case_when(year == 2020 ~ med_score, TRUE ~ NA_real_), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(yr_label = case_when(
      data_categories == "Energy" ~ year,
      TRUE ~ NA_real_
    )) %>%
    filter(element == str_c(x, " score")) %>%
    ggplot(aes(x = fct_reorder(data_categories, -rank_2020), y = med_score)) +
    geom_line(aes(group = data_categories)) +
    geom_point(aes(color = as.factor(year)), size = 2) +
    geom_text(aes(label = as.factor(yr_label), color = as.factor(yr_label)), hjust = "outward") +
    facet_wrap(~ macro_sector, scales = "free_x") +
    scale_y_continuous(limits = c(0,100)) +
    labs(y = str_c("Median ", x, " score"), x = "") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "off")
}
category_dumbbell_chart("Overall")

# Export data
(odin_scores_alt %>% 
  semi_join(odin_20_22) %>%
  mutate(element = case_when(
    element == "Coverage subscore" ~ "Coverage score",
    element == "Openness subscore" ~ "Openness score",
    TRUE ~ element)) %>%
  filter(element %in% c("Overall score", "Coverage score", "Openness score"),
         year %in% c(2020, 2022), 
         !data_categories %in% c("All Categories", "Economic & financial statistics subscore", "Environment subscore", "Social statistics subscore")) %>%
  group_by(element, macro_sector, data_categories, year) %>%
  summarize(med_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  write_csv("Output/ODIN 2022 Figure 9 data.csv", na = ""))

odin_scores_alt %>% 
  semi_join(odin_20_22) %>%
  mutate(element = case_when(
    element == "Coverage subscore" ~ "Coverage score",
    element == "Openness subscore" ~ "Openness score",
    TRUE ~ element)) %>%
  filter(element %in% c("Overall score", "Coverage score", "Openness score"),
         year %in% c(2020, 2022), 
         !data_categories %in% c("All Categories", "Economic & financial statistics subscore", "Environment subscore", "Social statistics subscore")) %>%
  group_by(element, macro_sector, data_categories, year) %>%
  summarize(med_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  write.xlsx("Output/ODIN 2022 Figure 9.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig9 data", row.names = FALSE)

# Figure 9 metadata
definition <- "The scores in this figure represent the median overall, coverage, or openness subscores for each ODIN data category for a consistent set of 183 countries assessed in both ODIN 2020 and 2022 rounds."

tibble(definition, date_last_updated, responsible_agency) %>%
  write.xlsx("Output/ODIN 2022 Figure 9.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig9 metadata", append = TRUE, row.names = FALSE)


# Why did Reproductive Health Openness increase?
element_change_sector <- function(x, y){
  odin_scores_alt %>% 
    filter(data_categories == x, 
           year %in% c(2020, 2022),
           macro_element == str_c(y, " elements")) %>%
    semi_join(odin_20_22) %>%
    group_by(element, year) %>%
    summarize(median_score = median(score, na.rm = TRUE),
              mean_score = mean(score, na.rm = TRUE)) %>%
    ungroup()
}
element_change_sector("Reproductive health", "Openness")
element_change_sector("Government finance", "Openness")
element_change_sector("Energy", "Openness")

# FIgure 10 Element scores for Energy category
odin_scores_alt %>%
  semi_join(odin_always) %>%
  filter(data_categories == "Energy",
         !element %in% c("Overall score", "Coverage subscore", "Openness subscore")) %>%
  group_by(element, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(label = case_when(year == max(year) ~ as.character(element), TRUE ~ NA_character_)) %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = element, group = element)) +
  geom_line() +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = label), nudge_x = 0.1, direction = "y", hjust = 0, na.rm = TRUE, size = 3.5, segment.alpha = 0) +
  labs(x = "", y = "Arithmetic average score", title = "Element scores for Energy category") +
  theme(legend.position = "none") +
  scale_x_discrete(limits = c("2016", "2017", "2018", "", "2020", "", "2022", "", "", ""))

# Export data
(odin_scores_alt %>%
  semi_join(odin_always) %>%
  filter(data_categories == "Energy",
         !element %in% c("Overall score", "Coverage subscore", "Openness subscore")) %>%
  group_by(element, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = year, names_from = element, values_from = mean_score) %>%
  select(year, `Indicator coverage and disaggregation`, `Data available last 5 years`,
         `Data available last 10 years`, `Machine readable`, `Non proprietary`, `Download options`,
         `Metadata available`, `Terms of use`) %>%
  write_csv("Output/ODIN 2022 Figure 10 data.csv", na = ""))

odin_scores_alt %>%
  semi_join(odin_always) %>%
  filter(data_categories == "Energy",
         !element %in% c("Overall score", "Coverage subscore", "Openness subscore")) %>%
  group_by(element, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = year, names_from = element, values_from = mean_score) %>%
  select(year, `Indicator coverage and disaggregation`, `Data available last 5 years`,
         `Data available last 10 years`, `Machine readable`, `Non proprietary`, `Download options`,
         `Metadata available`, `Terms of use`) %>%
  write.xlsx("Output/ODIN 2022 Figure 10.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig10 data", row.names = FALSE)

# Figure 10 metadata
definition <- "The scores in this figure represent the arithmetic average of each coverage and openness element for the Energy data category for a consistent set of 165 countries assessed in each ODIN round. The elements first and second administrative level are missing since these elements are not assessed for the Energy category"

tibble(definition, date_last_updated, responsible_agency) %>%
  write.xlsx("Output/ODIN 2022 Figure 10.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig10 metadata", append = TRUE, row.names = FALSE)


# Figure 11 IMF subscriber status
odin_scores_alt %>%
  left_join(dissemination_standards, by = c("country_code" = "iso3c")) %>%
  semi_join(odin_always) %>%
  filter(element %in% c("Overall score", "Coverage subscore", "Openness subscore"), 
         data_categories == "Economic & financial statistics subscore") %>%
  mutate(dissemination_subscriber = case_when(
    is.na(dissemination_subscriber) ~ "None",
    dissemination_subscriber == "sdds" ~ "SDDS",
    dissemination_subscriber == "sdds_plus" ~ "SDDS+",
    TRUE ~ "e-GDDS"
  ),
  dissemination_subscriber = fct_relevel(dissemination_subscriber, "SDDS+", "SDDS", "e-GDDS", "None"),
  element = fct_relevel(element, "Overall score", "Coverage subscore", "Openness subscore")) %>%
  group_by(year, element, dissemination_subscriber) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(year), y = median_score, color = dissemination_subscriber, group = dissemination_subscriber, label = round(median_score, 1))) + 
  geom_line(linewidth = 1.1) + 
  geom_point() +
  geom_text(vjust = 0, nudge_y = 0.5, show.legend = FALSE, size = 3) +
  labs(x = "", y = "Median score", color = "Subscriber\nstatus", title = "ODIN scores for Economic & financial statistics") +
  scale_x_discrete(limits = c("2016", "2017", "2018", "", "2020", "", "2022")) +
  facet_wrap(~element)

# Export data
(odin_scores_alt %>%
  left_join(dissemination_standards, by = c("country_code" = "iso3c")) %>%
  semi_join(odin_always) %>%
  filter(element %in% c("Overall score", "Coverage subscore", "Openness subscore"), 
         data_categories == "Economic & financial statistics subscore") %>%
  mutate(dissemination_subscriber = case_when(
    is.na(dissemination_subscriber) ~ "None",
    dissemination_subscriber == "sdds" ~ "SDDS",
    dissemination_subscriber == "sdds_plus" ~ "SDDS+",
    TRUE ~ "e-GDDS"
  ),
  dissemination_subscriber = fct_relevel(dissemination_subscriber, "SDDS+", "SDDS", "e-GDDS", "None"),
  element = fct_relevel(element, "Overall score", "Coverage subscore", "Openness subscore")) %>%
  group_by(year, element, dissemination_subscriber) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  write_csv("Output/ODIN 2022 Figure 11 data.csv", na = ""))

odin_scores_alt %>%
  left_join(dissemination_standards, by = c("country_code" = "iso3c")) %>%
  semi_join(odin_always) %>%
  filter(element %in% c("Overall score", "Coverage subscore", "Openness subscore"), 
         data_categories == "Economic & financial statistics subscore") %>%
  mutate(dissemination_subscriber = case_when(
    is.na(dissemination_subscriber) ~ "None",
    dissemination_subscriber == "sdds" ~ "SDDS",
    dissemination_subscriber == "sdds_plus" ~ "SDDS+",
    TRUE ~ "e-GDDS"
  ),
  dissemination_subscriber = fct_relevel(dissemination_subscriber, "SDDS+", "SDDS", "e-GDDS", "None"),
  element = fct_relevel(element, "Overall score", "Coverage subscore", "Openness subscore")) %>%
  group_by(year, element, dissemination_subscriber) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  write.xlsx("Output/ODIN 2022 Figure 11.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig11 data", row.names = FALSE)

# Figure 11 metadata
definition <- "The scores in this figure represent the median overall, coverage, and openness subscores for the Economic and Financial Statistics subscore by IMF data dissemination membership status as of February 2023 for a consistent set of 165 countries assessed in each ODIN round."

tibble(definition, date_last_updated, responsible_agency) %>%
  write.xlsx("Output/ODIN 2022 Figure 11.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig11 metadata", append = TRUE, row.names = FALSE)

# Figure 12 Median Environmental scores in SIDS
odin_scores_alt %>%
  semi_join(odin_always) %>%
  filter(element == "Overall score", data_categories == "Environment subscore") %>%
  group_by(sids, year) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(year), y = median_score, group = sids, color = sids, label = round(median_score, 1))) +
  geom_line() +
  geom_text(vjust = 0, nudge_y = 0.5, show.legend = FALSE, size = 3) +
  labs(x = "", y = "Median Score", title = "Environmental scores in SIDS vs non-SIDS") +
  theme(legend.title = element_blank())

# Export data
(odin_scores_alt %>%
  semi_join(odin_always) %>%
  filter(element == "Overall score", data_categories == "Environment subscore") %>%
  group_by(sids, year) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(sids = case_when(
    sids == "no" ~ "non-SIDS",
    TRUE ~ "Small Island Developing States (SIDS)"
  )) %>%
  write_csv("Output/ODIN 2022 Figure 12 data.csv", na = ""))

odin_scores_alt %>%
  semi_join(odin_always) %>%
  filter(element == "Overall score", data_categories == "Environment subscore") %>%
  group_by(sids, year) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(sids = case_when(
    sids == "no" ~ "non-SIDS",
    TRUE ~ "Small Island Developing States (SIDS)"
  )) %>%
  write.xlsx("Output/ODIN 2022 Figure 12.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig12 data", row.names = FALSE)

# Figure 12 metadata
definition <- "The scores in this figure represent the median overall scores for the Environment Statistics subscore SIDS status as of February 2023 for a consistent set of 165 countries assessed in each ODIN round."

tibble(definition, date_last_updated, responsible_agency) %>%
  write.xlsx("Output/ODIN 2022 Figure 12.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig12 metadata", append = TRUE, row.names = FALSE)


#### Income Analysis
# Compute aggregate stats
aggregate_stats <- odin_scores_alt %>%
  filter(year == 2022, data_categories == "All Categories", element == "Overall score", !is.na(income_group)) %>%
  left_join(
    # World Bank GNI per capita
    wbstats::wb_data(c("gni_pc" = "NY.GNP.PCAP.CD"), mrnev = 1) %>%
      select(iso3c, gni_pc), by = c("country_code" = "iso3c")) %>% 
  group_by(income_group) %>%
  summarize(score = median(score, na.rm = TRUE), min_gni = min(gni_pc, na.rm = TRUE), max_gni = max(gni_pc, na.rm = TRUE)) %>% 
  ungroup()

# Figure 13 on scores against GNI pc colored by income group
odin_scores_alt %>%
  filter(year == 2022, data_categories == "All Categories", element == "Overall score", !is.na(income_group)) %>%
  left_join(
    # World Bank GNI per capita
    wbstats::wb_data(c("gni_pc" = "NY.GNP.PCAP.CD"), mrnev = 1) %>%
      select(iso3c, gni_pc), by = c("country_code" = "iso3c")) %>%
  ggplot(aes(x = gni_pc, y = score, color = income_group)) +
  geom_point() +
  scale_x_log10(labels = scales::comma) +
  # Aggregate line per income group +
  geom_segment(data = aggregate_stats %>% filter(income_group == "Low income"),
               aes(x = min_gni, y = score, xend = max_gni, yend = score), size = 1) +
  geom_segment(data = aggregate_stats %>% filter(income_group == "Lower middle income"),
               aes(x = min_gni, y = score, xend = max_gni, yend = score), size = 1) +
  geom_segment(data = aggregate_stats %>% filter(income_group == "Upper middle income"),
               aes(x = min_gni, y = score, xend = max_gni, yend = score), size = 1) +
  geom_segment(data = aggregate_stats %>% filter(income_group == "High income"),
               aes(x = min_gni, y = score, xend = max_gni, yend = score), size = 1) +
  # Draw aggregate stat lines across the graph inheriting same color
  geom_hline(aes(yintercept = score, color = income_group), data = aggregate_stats, linetype = "dashed") +
  scale_y_continuous(limits = c(0,100)) +
  labs(x = "GNI per capita (logged)", y = "ODIN Overall Score 2022", color = "WB FY2023\nIncome Groups",
       title = "Many countries overperform given their income", subtitle = "Bars = Median scores by income group")

# Export data
(odin_scores_alt %>%
  filter(year == 2022, data_categories == "All Categories", element == "Overall score", !is.na(income_group)) %>%
  left_join(
    # World Bank GNI per capita
    wbstats::wb_data(c("gni_pc" = "NY.GNP.PCAP.CD"), mrnev = 1) %>%
      select(iso3c, gni_pc), by = c("country_code" = "iso3c")) %>%
  select(year, country, country_code, data_categories, element, score, income_group, gni_pc) %>%
  group_by(income_group) %>%
  mutate(`Median scores by income group` = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(gni_logged = log(gni_pc), `gni text` = str_c("GNI per capita (US$) is ", as.character(gni_pc))) %>%
  arrange(desc(income_group), country) %>%
  write_csv("Output/ODIN 2022 Figure 13 data.csv", na = ""))

odin_scores_alt %>%
  filter(year == 2022, data_categories == "All Categories", element == "Overall score", !is.na(income_group)) %>%
  left_join(
    # World Bank GNI per capita
    wbstats::wb_data(c("gni_pc" = "NY.GNP.PCAP.CD"), mrnev = 1) %>%
      select(iso3c, gni_pc), by = c("country_code" = "iso3c")) %>%
  select(year, country, country_code, data_categories, element, score, income_group, gni_pc) %>%
  group_by(income_group) %>%
  mutate(`Median scores by income group` = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(gni_logged = log(gni_pc), `gni text` = str_c("GNI per capita (US$) is ", as.character(gni_pc))) %>%
  arrange(desc(income_group), country) %>%
  write.xlsx("Output/ODIN 2022 Figure 13.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig13 data", row.names = FALSE)

# Figure 13 metadata
definition <- "The scores in this figure represent the 2022 overall scores for all data categories for all 192 countries assessed in the ODIN 2022|23 round. GNI per capita is from World Bank (indicator NY.GNP.PCAP.CD), most recent value for each country, accessed 7 February 2023. Country points are colored by World Bank FY23 income group. Horizontal bars represent median ODIN scores by income group"

tibble(definition, date_last_updated, responsible_agency) %>%
  write.xlsx("Output/ODIN 2022 Figure 13.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig13 metadata", append = TRUE, row.names = FALSE)


# Countries scoring higher/lower than income groups
inc_group_with_median <- odin_scores_alt %>%
  filter(year == 2022, data_categories == "All Categories", element == "Overall score", !is.na(income_group)) %>%
  select(country, country_code, income_group, score) %>%
  bind_cols(odin_scores_alt %>%
              filter(year == 2022, data_categories == "All Categories", element == "Overall score", !is.na(income_group)) %>%
              group_by(income_group) %>%
              summarize(median_score = median(score, na.rm = TRUE)) %>%
              ungroup() %>%
              pivot_wider(names_from = income_group, names_prefix = "median_", values_from = median_score)
  )
inc_group_with_median %>%
  filter(income_group=="High income", score < `median_Low income`)

#### Regional Analysis
# Figure 14 Regional median scores 2020 vs 2022
region_change <- odin_scores_alt %>%
  filter(year %in% c(2020, 2022),
         element %in% c("Overall score", "Coverage subscore", "Openness subscore"),
         data_categories == "All Categories") %>%
  semi_join(odin_20_22) %>%
  group_by(macro_region, region, element, year) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(macro_region, region), names_from = c(element, year), values_from = median_score) %>%
  mutate(overall_change = `Overall score_2022` - `Overall score_2020`,
         coverage_change = `Coverage subscore_2022` - `Coverage subscore_2020`,
         openness_change = `Openness subscore_2022` - `Openness subscore_2020`) %>%
  arrange(desc(overall_change))

# Export data
(region_change %>%
  select(region, `Overall score_2020`, `Overall score_2022`, overall_change) %>%
  arrange(desc(overall_change)) %>%
  write_csv("Output/ODIN 2022 Figure 14 data.csv", na = ""))

region_change %>%
  select(region, `Overall score_2020`, `Overall score_2022`, overall_change) %>%
  arrange(desc(overall_change)) %>%
  write.xlsx("Output/ODIN 2022 Figure 14.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig14 data", row.names = FALSE)

# Figure 14 metadata
definition <- "The scores in this figure represent the change in median overall scores for all data categories by UN region for a consistent set of 183 countries assessed in both the 2020 and 2022 ODIN round."

tibble(definition, date_last_updated, responsible_agency) %>%
  write.xlsx("Output/ODIN 2022 Figure 14.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig14 metadata", append = TRUE, row.names = FALSE)

# Figure 15
# Export data
(region_change %>%
    arrange(macro_region) %>%
    select(region, coverage_change, openness_change) %>%
    write_csv("Output/ODIN 2022 Figure 15 data.csv", na = ""))

region_change %>%
  arrange(macro_region) %>%
  select(region, coverage_change, openness_change) %>%
  write.xlsx("Output/ODIN 2022 Figure 15.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig15 data", row.names = FALSE)

# Figure 15 metadata
definition <- "The scores in this figure represent the change in median coverage or openness subscores for all data categories by UN region for a consistent set of 183 countries assessed in both the 2020 and 2022 ODIN round."

tibble(definition, date_last_updated, responsible_agency) %>%
  write.xlsx("Output/ODIN 2022 Figure 15.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig15 metadata", append = TRUE, row.names = FALSE)


region_change %>%
  filter(coverage_change>0, openness_change>0)

region_change %>%
  filter(coverage_change<0, openness_change>0)

region_change %>%
  filter(coverage_change>0, openness_change<0)

region_change %>%
  filter(coverage_change<0, openness_change<0)

region_change %>%
  arrange(macro_region)

# Table 1
(odin_scores_alt %>%
  filter(year %in% c(2020, 2022), element == "Overall score", data_categories == "All Categories") %>%
  semi_join(odin_20_22) %>%
  pivot_wider(id_cols = c(country, country_code, region, macro_region), names_from = year, values_from = score) %>%
  mutate(score_change = `2022` - `2020`) %>%
  arrange(macro_region, region, desc(score_change)) %>%
  write_csv("Output/ODIN 2022 Table 1 data.csv", na = ""))

(odin_scores_alt %>%
    distinct(macro_region, region) %>%
    arrange(macro_region, region) %>%
    write_csv("Output/ODIN 2022 Table 1 region list.csv", na = ""))

# Need to modify in Excel to add country data to list

# Table 1 metadata
definition <- "The scores in this figure represent the change in overall scores for the top three improved countries (above 1 point increase) by UN region."

tibble(definition, date_last_updated, responsible_agency) %>%
  write.xlsx("Output/ODIN 2022 Table 1 metadata.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Tab1 metadata", append = TRUE, row.names = FALSE)


# Figure 16 country information
(odin_scores_alt %>%
  filter(year %in% c(2020, 2022), str_detect(element, "score"), data_categories == "All Categories",
         country %in% c("Chile", "Paraguay", "Trinidad and Tobago", "Australia", "Jordan")) %>%
  pivot_wider(id_cols = c(country, year), names_from = element, values_from = score) %>%
  arrange(country, year) %>%
  select(country, year, `Overall score`, `Coverage subscore`, `Openness subscore`) %>%
  write_csv("Output/ODIN 2022 Figure 16 data.csv", na = ""))

odin_scores_alt %>%
  filter(year %in% c(2020, 2022), str_detect(element, "score"), data_categories == "All Categories",
         country %in% c("Chile", "Paraguay", "Trinidad and Tobago", "Australia", "Jordan")) %>%
  pivot_wider(id_cols = c(country, year), names_from = element, values_from = score) %>%
  arrange(country, year) %>%
  select(country, year, `Overall score`, `Coverage subscore`, `Openness subscore`) %>%
  write.xlsx("Output/ODIN 2022 Figure 16.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig16 data", row.names = FALSE)

# Figure 16 metadata
definition <- "The scores in this figure represent the overall, coverage, and openness scores for selected countries for all ODIN data categories for 2020 and 2022."

tibble(definition, date_last_updated, responsible_agency) %>%
  write.xlsx("Output/ODIN 2022 Figure 16.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig16 metadata", append = TRUE, row.names = FALSE)


# Chile one of only 6 countries in South America to increase their ODIN score
odin_scores_alt %>%
  filter(year %in% c(2020, 2022), element == "Overall score", data_categories == "All Categories") %>%
  semi_join(odin_20_22) %>%
  pivot_wider(id_cols = c(country, country_code, region, macro_region), names_from = year, values_from = score) %>%
  mutate(score_change = `2022` - `2020`) %>%
  arrange(macro_region, region, desc(score_change)) %>%
  filter(region == "South America")

# Chile increase in sector scores
odin_scores_alt %>%
  filter(country == "Chile", year %in% c(2020, 2022), str_detect(element, "score"), str_detect(data_categories, "score")) %>%
  pivot_wider(id_cols = c(element, data_categories), names_from = year, values_from = score)

# Chile increase in element scores
odin_scores_alt %>%
  filter(country == "Chile", year %in% c(2020, 2022), !is.na(macro_element), data_categories == "All Categories") %>%
  pivot_wider(id_cols = c(element), names_from = year, values_from = score) %>%
  mutate(score_change = `2022` - `2020`) %>%
  arrange(desc(score_change))

# Paraguay rank change
odin_scores_alt %>%
  filter(region == "South America", year == 2022, element == "Overall score", data_categories == "All Categories") %>%
  arrange(desc(score))

# Paraguay increase in element scores
odin_scores_alt %>%
  filter(country == "Paraguay", year %in% c(2020, 2022), !is.na(macro_element), data_categories == "All Categories") %>%
  pivot_wider(id_cols = c(element), names_from = year, values_from = score) %>%
  mutate(score_change = `2022` - `2020`) %>%
  arrange(desc(score_change))

# Trinidad one of only four countries with increase? which highest increase and what rank?
odin_scores_alt %>%
  filter(year %in% c(2020, 2022), element == "Overall score", data_categories == "All Categories") %>%
  semi_join(odin_20_22) %>%
  pivot_wider(id_cols = c(country, country_code, region, macro_region), names_from = year, values_from = score) %>%
  mutate(score_change = `2022` - `2020`) %>%
  arrange(macro_region, region, desc(score_change)) %>%
  filter(region == "Caribbean")

odin_scores_alt %>%
  filter(year %in% c(2020, 2022), element == "Overall score", data_categories == "All Categories") %>%
  semi_join(odin_20_22) %>%
  pivot_wider(id_cols = c(country, country_code, region, macro_region), names_from = year, values_from = score) %>%
  mutate(score_change = `2022` - `2020`) %>%
  arrange(macro_region, region, desc(score_change)) %>%
  filter(region == "Caribbean") %>%
  arrange(desc(`2022`))

# Trinidad and Tobago increase in element scores
odin_scores_alt %>%
  filter(country == "Trinidad and Tobago", year %in% c(2020, 2022), !is.na(macro_element), data_categories == "All Categories") %>%
  pivot_wider(id_cols = c(element), names_from = year, values_from = score) %>%
  mutate(score_change = `2022` - `2020`) %>%
  arrange(desc(score_change))

# Australia increase in element scores
odin_scores_alt %>%
  filter(country == "Australia", year %in% c(2020, 2022), !is.na(macro_element), data_categories == "All Categories") %>%
  pivot_wider(id_cols = c(element), names_from = year, values_from = score) %>%
  mutate(score_change = `2022` - `2020`) %>%
  arrange(desc(score_change))

# Jordan rank change
odin_scores_alt %>%
  filter(region == "Western Asia", year == 2022, element == "Overall score", data_categories == "All Categories") %>%
  arrange(desc(score))

# Jordan increase in element scores
odin_scores_alt %>%
  filter(country == "Jordan", year %in% c(2020, 2022), !is.na(macro_element), data_categories == "All Categories") %>%
  pivot_wider(id_cols = c(element), names_from = year, values_from = score) %>%
  mutate(score_change = `2022` - `2020`) %>%
  arrange(desc(score_change))

# Figure 17 metadata
definition <- "This graphic shows the overlap that exists between indicators used in ODIN assessments and those under the SDG global monitoring framework, as of January 2023."

tibble(definition, date_last_updated, responsible_agency) %>%
  write.xlsx("Output/ODIN 2022 Figure 17 metadata.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig17 metadata", row.names = FALSE)


# Figure 18 OGDI
(ogdi_country_alt %>%
    full_join(non_ogdi_country_alt %>% pivot_wider(id_cols = c(country, country_code), names_from = element, values_from = non_gender_score)) %>%
    rename(ogdi_coverage = total_coverage_subscore, ogdi_openness = total_openness_subscore, ogdi_overall = total_overall_score,
           non_ogdi_coverage = `Coverage subscore`, non_ogdi_openness = `Openness subscore`, non_ogdi_overall = `Overall score`) %>%
    select(country, country_code, ogdi_overall, non_ogdi_overall) %>%
    write_csv("Output/ODIN 2022 Figure 18 data.csv", na = ""))

ogdi_country_alt %>%
  full_join(non_ogdi_country_alt %>% pivot_wider(id_cols = c(country, country_code), names_from = element, values_from = non_gender_score)) %>%
  rename(ogdi_coverage = total_coverage_subscore, ogdi_openness = total_openness_subscore, ogdi_overall = total_overall_score,
         non_ogdi_coverage = `Coverage subscore`, non_ogdi_openness = `Openness subscore`, non_ogdi_overall = `Overall score`) %>%
  select(country, country_code, ogdi_overall, non_ogdi_overall) %>%
  write.xlsx("Output/ODIN 2022 Figure 18.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig18 data", row.names = FALSE)

# Figure 18 metadata
definition <- "The scores in this figure represent 2022 scores for all 192 countries assessed in the ODIN 2022/23 round by their score for gender-relevant versus not gender-relevant data categories."

tibble(definition, date_last_updated, responsible_agency) %>%
  write.xlsx("Output/ODIN 2022 Figure 18.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig18 metadata", append = TRUE, row.names = FALSE)


# Figure 19
ogdi_totals_alt %>%
  bind_rows(non_ogdi_totals_alt) %>%
  ggplot(aes(x = element, y = total_median, fill = ogdi)) +
  geom_col(position = position_dodge()) +
  geom_point(aes(y = total_mean), position = position_dodge(width = 0.9), show.legend = FALSE) +
  labs(x = "", y = "Median score (dots represent averages)") +
  theme(legend.title = element_blank())

# Export data
(ogdi_totals_alt %>%
  bind_rows(non_ogdi_totals_alt) %>%
  write_csv("Output/ODIN 2022 Figure 19 data.csv", na = ""))

ogdi_totals_alt %>%
  bind_rows(non_ogdi_totals_alt) %>%
  write.xlsx("Output/ODIN 2022 Figure 19.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig19 data", row.names = FALSE)

# Figure 19 metadata
definition <- "The scores in this figure represent the median overall, coverage, and openness scores for gender- or non-gender-relevant data categories for all 192 countries assessed in the ODIN 2022/23 round."

tibble(definition, date_last_updated, responsible_agency) %>%
  write.xlsx("Output/ODIN 2022 Figure 19.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig19 metadata", append = TRUE, row.names = FALSE)


# Figure 20

# Performance in 2022 of gender vs non-gender data_categories by element
ogdi_2022_alt %>%
  filter(year == 2022) %>%
  group_by(country, country_code, macro_element, element, ogdi) %>%
  summarize(mean_country_element_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(macro_element, element, ogdi) %>%
  summarize(mean_world_element_score = mean(mean_country_element_score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(ogdi), !is.na(macro_element)) %>%
  group_by(ogdi) %>%
  mutate(rank = rank(mean_world_element_score),
         rank = case_when(ogdi == "non_OGDI" ~ NA_real_, TRUE ~ rank)) %>%
  ggplot(aes(x = fct_reorder(element, rank), y = mean_world_element_score, fill = ogdi)) +
  geom_col(position = position_dodge()) +
  coord_flip() +
  facet_grid(macro_element~., scales = "free_y") +
  labs(x = "Average score", y = "") +
  theme(legend.title = element_blank())
# doing this with the median actually shows that Indicator and coverage and machine-readable 
# are the most different between the two sets of categories.

# Export data
(ogdi_2022_alt %>%
  filter(year == 2022) %>%
  group_by(country, country_code, macro_element, element, ogdi) %>%
  summarize(mean_country_element_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(macro_element, element, ogdi) %>%
  summarize(mean_world_element_score = mean(mean_country_element_score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(ogdi), !is.na(macro_element)) %>%
  write_csv("Output/ODIN 2022 Figure 20 data.csv", na = ""))

ogdi_2022_alt %>%
  filter(year == 2022) %>%
  group_by(country, country_code, macro_element, element, ogdi) %>%
  summarize(mean_country_element_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(macro_element, element, ogdi) %>%
  summarize(mean_world_element_score = mean(mean_country_element_score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(ogdi), !is.na(macro_element)) %>%
  write.xlsx("Output/ODIN 2022 Figure 20.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig20 data", row.names = FALSE)

# Figure 20 metadata
definition <- "The scores in this figure represent the median coverage and openness element scores by gender- or non-gender-relevant data categories for all 192 countries assessed in the ODIN 2022/23 round."

tibble(definition, date_last_updated, responsible_agency) %>%
  write.xlsx("Output/ODIN 2022 Figure 20.xlsx", showNA = FALSE, sheetName = "ODIN 22|23 Report Fig20 metadata", append = TRUE, row.names = FALSE)




odin_scores_alt %>%
  filter(country == "Oman", year == 2022, data_categories == "Government finance")


odin_scores_alt %>%
  filter(year == 2022)

odin_mar9 <- readxl::read_excel("C:/Users/loren/Downloads/odin_2022_Mar9.xlsx") %>%
  janitor::clean_names() %>%
  filter(!is.na(region)) %>%
  mutate(data_categories = case_when(
    data_categories == "All categories" ~ "All Categories",
    data_categories == "Economic & Financial Statistics subscores" ~ "Economic & financial statistics subscore",
    data_categories == "Social Statistics subscore" ~ "Social statistics subscore",
    data_categories == "Agriculture and land use" ~ "Agriculture & Land Use",
    TRUE ~ data_categories
  ),
  first_administrative_level = as.numeric(str_remove(first_administrative_level, "-")),
  second_administrative_level = as.numeric(str_remove(second_administrative_level, "-")),
  # Fix trailing new line break in region
  region = str_remove(region, "\n"),
  country_code = case_when(
    country == "Andorra" ~ "AND",
    TRUE ~ country_code
  ),
  country = case_when(country_code == "TUR" ~ "Turkey", TRUE ~ country)) %>%
  pivot_longer(cols = indicator_coverage_and_disaggregation:overall_score, names_to = "element", values_to = "new_score")

odin_mar7 <- read_csv("C:/Users/loren/Documents/GitHub/odin_analysis/Input/ODIN_scores_2022_fix.csv") %>%
  janitor::clean_names() %>%
  filter(!is.na(region)) %>%
  rename(data_categories = elements, coverage_subscore = coverage_subscores, openness_subscore = openness_subscores, overall_score = overall_subscores) %>%
  # Bring spelling of data categories in line with previous years
  mutate(data_categories = case_when(
    data_categories == "All categories" ~ "All Categories",
    data_categories == "Economic & Financial Statistics subscores" ~ "Economic & financial statistics subscore",
    data_categories == "Social Statistics subscore" ~ "Social statistics subscore",
    data_categories == "Agriculture and land use" ~ "Agriculture & Land Use",
    TRUE ~ data_categories
  ),
  first_administrative_level = as.numeric(str_remove(first_administrative_level, "-")),
  second_administrative_level = as.numeric(str_remove(second_administrative_level, "-")),
  # Fix trailing new line break in region
  region = str_remove(region, "\n"),
  year = as.numeric(year),
  country = case_when(country_code == "TUR" ~ "Turkey", TRUE ~ country)) %>%
  pivot_longer(cols = indicator_coverage_and_disaggregation:overall_score, names_to = "element", values_to = "old_score")

combo <- odin_mar7 %>%
  full_join(odin_mar9) %>%
  # Convert elements back to sentence case for easier reading
  mutate(element = str_to_sentence(str_replace_all(element, "_", " ")))

combo %>%
  filter(new_score!=old_score)

odin_scores_alt %>%
  filter(element == "Data available last 10 years", year %in% c(2020, 2022)) %>%
  semi_join(odin_20_22) %>%
  pivot_wider(id_cols = c(country, country_code, data_categories), names_from = year, values_from = score) %>%
  filter(`2020` == 100, `2022` == 50) %>%
  select(country, country_code, data_categories, ten_year_2020 = `2020`, ten_year_2022 = `2022`) %>%
  write_csv("C:/Users/loren/Downloads/Ten year drops 2020-2022.csv", na = "")

iaeg_list <- readxl::read_excel("C:/Users/loren/Downloads/ODIN Mar 7 new data old data comparison inc country list.xlsx", sheet = 2) %>%
  select(country_code, membership)

odin_scores_alt %>%
  filter(year %in% c(2020, 2022), str_detect(element, "score"), data_categories == "All Categories") %>%
  semi_join(odin_20_22) %>%
  pivot_wider(id_cols = c(country, country_code, element), names_from = year, values_from = score) %>%
  mutate(score_change = `2022` - `2020`) %>%
  left_join(iaeg_list) %>%
  filter(element == "Overall score") %>%
  arrange(score_change) %>%
  print(n = 35)
# Grenada and Nauru as new ODIN countries can't be compared to 2020 scores

# Rwanda (-6.6) and Madagascar (-6.5) the IAEG-SDG members with the biggest score drops, followed by Oman (-5.5) as
# IAEG observer, followed by India (-5.3)


# March 15 fix
