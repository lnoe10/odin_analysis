library(tidyverse)

#### Introduction ####

# Median score change between 2020 and 2022 for Overall, Openness, and Coverage
odin_scores %>% 
  filter(element %in% c("Overall score", "Coverage subscore", "Openness subscore"), 
         year %in% c(2020, 2022), 
         data_categories == "All Categories") %>% 
  semi_join(odin_20_22) %>% 
  group_by(year, element) %>% 
  summarize(median_score = median(score, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = element, names_from = year, values_from = median_score) %>%
  mutate(score_change = `2022` - `2020`)

# Change in overall score over time since 2016
odin_scores %>% 
  filter(element %in% c("Overall score"), 
         data_categories == "All Categories") %>% 
  semi_join(odin_always) %>% 
  group_by(year) %>% 
  summarize(median_score = median(score, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(change = median_score - lag(median_score))
# Result for sample of countries always vs just last two years the same.

# Number of countries with reduced median scores in 2022
odin_scores %>% 
  filter(element %in% c("Overall score"), 
         year %in% c(2020, 2022), 
         data_categories == "All Categories") %>% 
  semi_join(odin_20_22) %>%
  arrange(country_code, year) %>%
  pivot_wider(id_cols = c(country, country_code), names_from = year, values_from = score) %>%
  count(`2020` > `2022`)

# Range of 2022 scores
odin_scores %>%
  filter(year == 2022, element == "Overall score",
         data_categories == "All Categories") %>%
  summarize(min_score = min(score, na.rm = TRUE),
            max_score = max(score, na.rm = TRUE))

#### Results at a Glance ####

# Number of countries that have been added or dropped
odin_scores %>%
  filter(element %in% c("Overall score"), 
         year %in% c(2020, 2022), 
         data_categories == "All Categories") %>%
  pivot_wider(id_cols = c(country, country_code), names_from = year, values_from = score) %>%
  filter(is.na(`2020`)|is.na(`2022`))

# New countries in 2022
odin_scores %>%
  filter(element %in% c("Overall score"), 
         data_categories == "All Categories") %>%
  group_by(country, country_code) %>%
  mutate(num_obs = n()) %>%
  filter(year == 2022, num_obs == 1)

# Difference in median openness score due to change in openness methodology (Figure 2)
combined %>%
  semi_join(odin_20_22) %>%
  filter(element == "Overall score", 
         data_categories %in% c("All Categories", "Social statistics subscore", "Environment subscore", "Economic & financial statistics subscore")) %>%
  group_by(data_categories) %>%
  summarize(total_openness_old_method = median(new_score, na.rm = TRUE),
            total_openness_new_method = median(old_score, na.rm = TRUE)) %>%
  mutate(change = total_openness_new_method - total_openness_old_method)

# Graph of main three scores since 2016 (Figure 2)
odin_scores %>%
  filter(element %in% c("Overall score", "Coverage subscore", "Openness subscore"), 
         data_categories == "All Categories") %>%
  semi_join(odin_always) %>%
  group_by(element, year) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = median_score, group = element, color = element, label = median_score)) +
  geom_line() +
  geom_text()

# Graph of main three scores since 2016 (Figure 3)
odin_scores %>%
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

# Graph of median ODIN scores by income group for 2020 - 2022 (Figure 4)
odin_scores %>%
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

# Graph of median ODIN scores by region 2016 vs 2022 (Figure 5)
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

# Performance of OGDI vs non-OGDI over time (Figure 6)
ogdi_totals_historical %>%
  bind_rows(non_ogdi_totals_historical) %>%
  ggplot(aes(x = as.factor(year), y = total_median, fill = ogdi)) +
  geom_col(position = position_dodge()) +
  facet_wrap(element~.) +
  labs(x = "", y = "Median Score") +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

#### Main Findings ####
# Number of countries with reduced median openness or coverage scores in 2022
odin_scores %>% 
  filter(element %in% c("Overall score", "Coverage subscore", "Openness subscore"), 
         year %in% c(2020, 2022), 
         data_categories == "All Categories") %>% 
  semi_join(odin_20_22) %>%
  arrange(element, country_code, year) %>%
  pivot_wider(id_cols = c(element, country, country_code), names_from = year, values_from = score) %>%
  count(element, `2020` > `2022`)

# Median score change between 2020 and 2022 for Overall, Openness, and Coverage (Figure 7)
odin_scores %>% 
  filter(element %in% c("Overall score", "Coverage subscore", "Openness subscore"), 
         year %in% c(2020, 2022), 
         data_categories == "All Categories") %>% 
  semi_join(odin_20_22) %>% 
  group_by(year, element) %>% 
  summarize(median_score = median(score, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = element, names_from = year, values_from = median_score) %>%
  mutate(score_change = `2022` - `2020`)

# Median coverage and openness element scores 2020 and 2022 (Figure 8)
odin_scores %>%
  filter(year %in% c(2020, 2022), data_categories == "All Categories",
         !element %in% c("Overall score", "Coverage subscore", "Openness subscore")) %>%
  semi_join(odin_20_22) %>%
  group_by(macro_element, element, year) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup()

# Number of countries with TOUs dropping to 0
odin_scores %>%
  filter(year %in% c(2020, 2022), data_categories == "All Categories", element == "Terms of use") %>%
  semi_join(odin_20_22) %>%
  count(year, score == 0)

odin_scores %>%
  filter(year %in% c(2020, 2022), data_categories == "All Categories", element == "Terms of use") %>%
  semi_join(odin_20_22) %>%
  group_by(year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE),
            median_score = median(score, na.rm = TRUE)) %>%
  ungroup()

odin_scores %>%
  filter(year %in% c(2020, 2022), data_categories == "All Categories", element == "Terms of use") %>%
  semi_join(odin_20_22) %>%
  ggplot(aes(x = score)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(as.factor(year)~.)

odin_scores %>%
  filter(year %in% c(2020, 2022), data_categories == "All Categories", element == "Terms of use") %>%
  semi_join(odin_20_22) %>%
  arrange(year, score) %>%
  group_by(year) %>%
  mutate(rank = row_number()) %>%
  select(country, year, score, rank) %>%
  filter(rank > 90, rank < 94)

# Do the same countries have 0s for TOU?
odin_scores %>%
  filter(year %in% c(2020, 2022), data_categories == "All Categories", element == "Terms of use") %>%
  semi_join(odin_20_22) %>%
  pivot_wider(id_cols = c(country, country_code), names_from = year, values_from = score) %>%
  count(`2022` == 0, `2020` == 0)

#### Sectoral Analysis
odin_scores %>%
  semi_join(odin_20_22) %>%
  filter(year %in% c(2020, 2022), str_detect(data_categories, "subscore"), str_detect(element, "score")) %>%
  group_by(year, data_categories, element) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(data_categories, element), names_from = year, values_from = median_score) %>%
  mutate(change_score = `2022` - `2020`)

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
category_dumbbell_chart("Openness")

# Why did Reproductive Health Openness increase?
element_change_sector <- function(x, y){
  odin_scores %>% 
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

# FIgure 10 Element scores for Energy category
odin_scores %>%
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

# Figure 11 IMF subscriber status
odin_scores %>%
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

# Figure 12 Median Environmental scores in SIDS
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

#### Income Analysis
# Figure 13 on scores against GNI pc colored by income group
odin_scores %>%
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

#### Regional Analysis
# Figure 14 Regional median scores 2020 vs 2022
region_change <- odin_scores %>%
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
