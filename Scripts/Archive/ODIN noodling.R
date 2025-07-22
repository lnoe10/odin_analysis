library(readxl)
always_ODIN_countries <- read_excel("~/R work/Input Data/always_ODIN countries.xlsx") %>%
  mutate(country_code = countrycode::countrycode(country, "country.name", "iso3c"),
         country_code = case_when(
           country == "Kosovo" ~ "XKX",
           TRUE ~ country_code
         ))

odin_scores_plus_always_odin <- odin_scores %>%
  left_join(always_ODIN_countries) %>%
  mutate(always_odin = case_when(
    is.na(always_odin) ~ 0,
    TRUE ~ always_odin
  ))

odin_subset <- odin_scores_plus_always_odin %>%
  filter(always_odin == 1)

# Create average score difference by country
odin_subset %>%
  filter(element == "Overall score", data_categories == "All Categories") %>%
  count(year)


odin_scores %>%
  filter(element == "Overall score", data_categories == "All Categories") %>%
  group_by(country, country_code) %>%
  summarize(num_obs = n()) %>%
  ungroup() %>%
  count(num_obs)

#
# Create average score difference by country
odin_scores %>%
  # Filter for countries with overall score in every year (5)
  filter(element == "Overall score", data_categories == "All Categories") %>%
  group_by(country, country_code) %>%
  mutate(num_obs = n()) %>%
  ungroup() %>%
  # Keep only those countries with assessments every year
  filter(num_obs == 5) %>%
  # Create average score difference by country
  arrange(country_code, year) %>%
  group_by(country_code) %>%
  mutate(year_change = score - lag(score),
         avg_change = mean(year_change, na.rm = TRUE)) %>%
  select(country, year, score, year_change, avg_change)


odin_scores %>%
  filter(data_categories %in% c("Population & vital statistics", "Education facilities",
                              "Education outcomes", "Health outcomes", "Reproductive health",
                              "Food security & nutrition", "Gender statistics", "Crime & justice",
                              "Labor"), 
         year %in% c(2020, 2022),
         # Filtering for these three elements has to be an %in% filter, not ==, otherwise R will look for one cell that contains all three
         element %in% c("Overall score", "Coverage subscore", "Openness subscore")) %>%
  group_by(year, data_categories, element) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = data_categories, group = data_categories)) +
  geom_line() + 
  facet_wrap(~element)
  
odin_scores %>%
  filter(data_categories %in% c("Population & vital statistics", "Education facilities",
                                "Education outcomes", "Health outcomes", "Reproductive health",
                                "Food security & nutrition", "Gender statistics", "Crime & justice",
                                "Labor"), 
         year %in% c(2020, 2022),
         macro_element == "Coverage elements") %>%
  group_by(year, data_categories, element, sids) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(element == "Second administrative level") %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = data_categories, group = data_categories)) +
  geom_line() + 
  facet_wrap(~sids)

odin_scores %>%
  filter(data_categories %in% c("Population & vital statistics", "Education facilities",
                                "Education outcomes", "Health outcomes", "Reproductive health",
                                "Food security & nutrition", "Gender statistics", "Crime & justice",
                                "Labor"), 
         year %in% c(2020, 2022),
         element == "Indicator coverage and disaggregation") %>%
  group_by(year, data_categories) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = data_categories, names_from = year, names_prefix = "year_", values_from = mean_score) %>%
  mutate(year_change = year_2022 - year_2020) %>%
  arrange(desc(year_change))

odin_scores %>%
  # Restrict dataset to single unique rows
  filter(element == "Overall score", data_categories == "Environment subscore", sids == "yes", year %in% c(2020, 2022)) %>%
  select(country, country_code, score, year) %>%
  pivot_wider(id_cols = country:country_code, names_from = year, names_prefix = "year_", values_from = score) %>%
  filter(!is.na(year_2020), !is.na(year_2022)) %>%
    summarize(avg_2020 = mean(year_2020, na.rm = TRUE), avg_2022 = mean(year_2022, na.rm = TRUE))

colMeans(environment_overlapping_SIDS[sapply(environment_overlapping_SIDS, is.numeric)])



# 1.	2016-2022 average overall, openness and coverage scores for the 165 common countries between 2016 and 2022.

odin_always <- odin_scores %>%
  filter(element == "Overall score", data_categories == "All Categories") %>%
  group_by(country_code) %>%
  summarize(num_obs = n()) %>%
  ungroup() %>%
  filter(num_obs == 5) %>%
  mutate(consistent_sample = "Yes") %>%
  select(country_code, consistent_sample)

(odin_scores %>%
  semi_join(odin_always) %>%
  filter(element %in% c("Overall score", "Coverage subscore", "Openness subscore"), data_categories == "All Categories") %>%
  group_by(year, element) %>%
  summarize(med_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_col = year, names_from = element, values_from = med_score) %>%
  write_csv("C:/Users/loren/Downloads/ODIN scores consistent countries 2016-2022.csv", na = ""))

# 2.	Average overall regional scores for 2020 and 2022.

odin_both2020_2022 <- odin_scores %>%
  filter(element == "Overall score", data_categories == "All Categories", year %in% c(2020, 2022)) %>%
  group_by(country_code) %>%
  summarize(num_obs = n()) %>%
  ungroup() %>%
  filter(num_obs == 2) %>%
  mutate(consistent_sample = "Yes") %>%
  select(country_code, consistent_sample)

(odin_scores %>%
  semi_join(odin_both2020_2022) %>%
  filter(element == "Overall score", data_categories == "All Categories", year %in% c(2020, 2022)) %>%
  group_by(year, region) %>%
  summarize(med_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = region, names_from = year, values_from = med_score, names_prefix = "year_") %>%
  mutate(change = year_2022 - year_2020) %>%
  write_csv("C:/Users/loren/Downloads/ODIN region change consistent countries 2020-2022.csv", na = ""))

# Function for graphing category rank
data_category_rank <- function(a){
  odin_scores %>% 
    filter(element == a, year == 2022, 
           !data_categories %in% c("All Categories", 
                                   "Economic & financial statistics subscore", 
                                   "Environment subscore", "Social statistics subscore")) %>%
    group_by(data_categories) %>%
    summarize(mean_score = mean(score, na.rm = TRUE)) %>%
    ungroup() %>%
    ggplot(aes(x = fct_reorder(data_categories, mean_score), y = mean_score)) +
    geom_col() +
    labs(x = "", y = str_c("Average ", a)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
}

data_category_rank("Coverage subscore")

# Biggest improved categories
data_change20_22 <- function(a){
  odin_scores %>%
    semi_join(odin_20_22) %>%
    filter(element == a, year %in% c(2020, 2022), 
           !data_categories %in% c("All Categories",
                                   "Economic & financial statistics subscore",
                                   "Environment subscore", "Social statistics subscore")) %>%
    group_by(data_categories, year) %>%
    summarize(mean_score = mean(score, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(data_categories, year) %>%
    group_by(data_categories) %>%
    mutate(change_20_22 = mean_score - lag(mean_score)) %>%
    ungroup() %>%
    filter(year == 2022) %>%
    ggplot(aes(x = fct_reorder(data_categories, change_20_22), y = change_20_22)) +
    geom_col() +
    labs(x = "", y = str_c("Average ", a)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
}
data_change20_22("Openness subscore")

# GDB vs overall ODIN
odin_scores %>%
  filter(year == 2022, data_categories == "All Categories", element == "Overall score") %>%
  full_join(read_csv("C:/Users/loren/Downloads/gdb_overall.csv") %>%
              select(country_code = iso3, gdb_score = score)) %>%
  summarize(correl = cor(score, gdb_score, use = "pairwise.complete.obs"))

# Table Change in median coverage and openness scores in major data categories, 2020 and 2022
odin_scores %>%
  filter(year %in% c(2020, 2022), element %in% c("Overall score", "Coverage subscore", "Openness subscore"),
         str_detect(data_categories, "score")) %>%
  semi_join(odin_20_22) %>%
  group_by(year, element, data_categories) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(element, data_categories, year) %>%
  group_by(element, data_categories) %>%
  mutate(change_20_22 = median_score - lag(median_score)) %>%
  ungroup()


# Level change in median scores dumbbell chart
odin_scores %>% 
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
  labs(y = "", x = "Median Overall score for All Categories") +
  theme(legend.position = "off")
ggsave("C:/Users/loren/Downloads/region dumbbell chart alternate.png", dpi = 400)

# Level change in median scores dumbbell chart
odin_scores %>% 
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
ggsave("C:/Users/loren/Downloads/region dumbbell chart facet.png", dpi = 400)


odin_scores %>%
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
ggsave("C:/Users/loren/Downloads/Env scores SIDS.png", dpi = 400)


element_graph <- function(x, y){
  odin_scores %>%
    filter(macro_element == str_c(x, " elements"), data_categories == y,
           year %in% c(2020, 2022)) %>%
    mutate(element = case_when(
      x == "Coverage" ~ fct_relevel(element, "Indicator coverage and disaggregation", "Data available last 5 years", "Data available last 10 years",
                                              "First administrative level", "Second administrative level"),
      x == "Openness" ~ fct_relevel(element, "Machine readable", "Non proprietary", "Metadata available", "Download options", "Terms of use"),
      TRUE ~ element)) %>%
    semi_join(odin_20_22) %>%
    group_by(element, year) %>%
    summarize(median_score = median(score, na.rm = TRUE)) %>%
    ungroup() %>%
    ggplot(aes(x = as.factor(year), y = median_score, group = element, color = element)) +
    geom_line() +
    labs(x = "", y = "Median Score", title = y)
}

element_graph("Coverage", "Food security & nutrition")

# What's moved data_category differences between 2020 and 2022 in terms of element shifts?
odin_scores %>%
  semi_join(odin_20_22) %>%
  filter(year %in% c(2020, 2022)) %>%
  group_by(year, macro_element, element, macro_sector, data_categories) %>%
  summarize(median_score = median(score, na.rm = FALSE),
            mean_score = mean(score, na.rm = FALSE)) %>%
  ungroup() %>%
  filter(macro_sector == "Economic and financial statistics", (macro_element == "Openness elements" | element == "Openness subscore")) %>%
  pivot_wider(id_cols = c(element, data_categories), names_from = year, values_from = c(median_score, mean_score)) %>%
  print(n = 42) %>%
  filter(data_categories == "Government finance")

# Export for color map
odin_scores %>%
  filter(year == 2022, element == "Overall score", data_categories == "All Categories") %>%
  mutate(map_color = case_when(
    score < 20 ~ "#D95F5E",
    score >= 20 & score < 40 ~ "#F5AC4C",
    score >= 40 & score < 60 ~ "#FFEE70",
    score >= 60 & score < 80 ~ "#B0D94C",
    TRUE ~ "#91B342"
  )) %>%
  mutate(iso2c = countrycode::countrycode(country_code, "iso3c", "iso2c"),
         iso2c = case_when(country_code == "XKX" ~ "XK", TRUE ~ iso2c)) %>%
  select(country, iso2c, country_code, region, score, map_color) %>%
  write_csv("C:/Users/loren/Downloads/scores for map ODIN 2022.csv", na = "")


# Category median chart
category_dumbbell_chart <- function(x){
  odin_scores %>% 
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
ggsave("C:/Users/loren/Downloads/category dumbbell chart facet overall.png", dpi = 400)


odin_scores %>%
  semi_join(odin_20_22) %>%
  filter(data_categories %in% c("Economic & financial statistics subscore", 
                                   "Environment subscore", "Social statistics subscore"), 
         element %in% c("Overall score", "Coverage subscore", "Openness subscore"),
         year %in% c(2020, 2022)) %>%
  group_by(data_categories, element, year) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup()
