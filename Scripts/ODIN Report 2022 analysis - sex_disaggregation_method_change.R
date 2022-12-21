#### sex-disaggregation
# What happened to coverage scores between 2020 and 2022
odin_scores %>%
  filter(year %in% c(2020, 2022), element %in% c("Overall score", "Coverage subscore", "Openness subscore"),
         data_categories == "All Categories") %>%
  group_by(year, element) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = element, group = element)) + 
  geom_line()

# Coverage overall still goes up

# What about each coverage element?
odin_scores %>%
  filter(year %in% c(2020, 2022), macro_element == "Coverage elements",
         data_categories == "All Categories") %>%
  group_by(year, element) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = element, group = element)) + 
  geom_line()

# Indicator coverage goes down, data available last five years stays pretty flat

# What does this look like if we split up data categories by whether or not they require sex-disaggregation and are newly designated?
odin_scores %>%
  mutate(cat_sex_disagg = case_when(
    data_categories %in% c("Population & vital statistics", "Health outcomes", "Reproductive health",
                           "Gender statistics", "Crime & justice",
                           "Labor") ~ "Always",
    data_categories %in% c("Education facilities",
                           "Education outcomes", "Food security & nutrition") ~ "New",
    data_categories %in% c("All Categories", "Economic & financial statistics subscore", 
                           "Environment subscore", "Social statistics subscore") ~ NA_character_,
    TRUE ~ "Not"
  ),
  cat_sex_disagg = fct_relevel(cat_sex_disagg, "Not", "Always", "New")) %>%
  filter(year %in% c(2020, 2022), macro_element == "Coverage elements",
         !is.na(cat_sex_disagg)) %>%
  group_by(year, element, data_categories, cat_sex_disagg) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(year, element, cat_sex_disagg) %>%
  summarize(disagg_status_mean_score = mean(mean_score, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(year), y = disagg_status_mean_score, color = element, group = element)) + 
  geom_line() +
  facet_wrap(~cat_sex_disagg)

# What does this look like just for coverage element "Indicator coverage and disaggregation?"
odin_scores %>%
  mutate(cat_sex_disagg = case_when(
    data_categories %in% c("Population & vital statistics", "Health outcomes", "Reproductive health",
                           "Gender statistics", "Crime & justice",
                           "Labor") ~ "Always",
    data_categories %in% c("Education facilities",
                           "Education outcomes", "Food security & nutrition") ~ "New",
    data_categories %in% c("All Categories", "Economic & financial statistics subscore", 
                           "Environment subscore", "Social statistics subscore") ~ NA_character_,
    TRUE ~ "No"
  ),
  cat_sex_disagg = fct_relevel(cat_sex_disagg, "No", "Always", "New")) %>%
  filter(year %in% c(2020, 2022), element == "Indicator coverage and disaggregation",
         !is.na(cat_sex_disagg)) %>%
  group_by(year, data_categories, cat_sex_disagg) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(year, cat_sex_disagg) %>%
  summarize(disagg_status_mean_score = mean(mean_score, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(year), y = disagg_status_mean_score, color = cat_sex_disagg, group = cat_sex_disagg)) + 
  geom_line() +
  labs(x = "", y = "Average score for Indicator Coverage and Disaggregation", color = "Sex-disaggregation\n for data category?") +
  scale_y_continuous(limits = c(0,100))

# Look at country level score changes for three new requirements for Indicator Coverage and Disaggregation
# What does this look like just for coverage element "Indicator coverage and disaggregation?"
odin_scores %>%
  filter(year %in% c(2020, 2022), element == "Indicator coverage and disaggregation",
         data_categories %in% c("Education facilities",
                                "Education outcomes", "Food security & nutrition")) %>%
  pivot_wider(id_cols = c(country_code, data_categories), names_from = year, names_prefix = "year_", values_from = score) %>%
  mutate(year_change = year_2022 - year_2020) %>%
  select(country_code, data_categories, year_2020, year_2022, year_change) %>%
  arrange(data_categories, year_change) %>%
  count(data_categories, year_change)

odin_scores %>%
  filter(year %in% c(2020, 2022), element == "Indicator coverage and disaggregation",
         data_categories %in% c("Education facilities",
                                "Education outcomes", "Food security & nutrition")) %>%
  pivot_wider(id_cols = c(country_code, data_categories), names_from = year, names_prefix = "year_", values_from = score) %>%
  mutate(year_change = year_2022 - year_2020) %>%
  select(country_code, data_categories, year_2020, year_2022, year_change) %>%
  arrange(data_categories, year_change) %>%
  group_by(country_code) %>%
  summarize(avg_change = mean(year_change, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(avg_change)
