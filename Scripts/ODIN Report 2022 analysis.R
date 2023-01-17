#### SETUP ####

library(tidyverse)

setwd("C:/Users/loren/Documents/GitHub/odin_analysis")

# Read in and clean data
odin_scores <- read_csv("Input/ODIN_scores_2022.csv") %>%
  # convert all variable names to snakecase
  janitor::clean_names() %>%
  # Take out missing observations at bottom of the table, it's just metadata on file generation
  filter(!is.na(region)) %>%
  # rename variables for appending previous years
  rename(data_categories = elements, coverage_subscore = coverage_subscores, openness_subscore = openness_subscores, overall_score = overall_subscores) %>%
  # Bring spelling of data categories in line with previous years
  mutate(data_categories = case_when(
    data_categories == "All categories" ~ "All Categories",
    data_categories == "Economic and Financial Statistics subscores" ~ "Economic & financial statistics subscore",
    data_categories == "Social Statistics subscores" ~ "Social statistics subscore",
    TRUE ~ data_categories
  ),
  # Fix trailing new line break in region
  region = str_remove(region, "\n")) %>%
  # Add 2020 scores
  bind_rows(read_csv("Input/ODIN_scores_2020.csv") %>%
              # convert all variable names to snakecase
              janitor::clean_names() %>%
              # Take out missing observations at bottom of the table, it's just metadata on copyright
              filter(!is.na(year), str_count(year) <= 4) %>%
              # Take out character that means not applicable in second administrative level and convert to numeric
              mutate(second_administrative_level = as.numeric(str_remove(second_administrative_level, "-")))) %>%
  # Add 2018 scores
  bind_rows(read_csv("Input/ODIN_scores_2018.csv") %>%
              # convert all variable names to snakecase
              janitor::clean_names() %>%
              # Take out missing observations at bottom of the table, it's just metadata on copyright
              filter(!is.na(year), str_count(year) <= 4) %>%
              # Take out character that means not applicable in second administrative level and convert to numeric
              mutate(second_administrative_level = as.numeric(str_remove(second_administrative_level, "-")))) %>%
  # Add 2017 scores
  bind_rows(read_csv("Input/ODIN_scores_2017.csv") %>%
              # convert all variable names to snakecase
              janitor::clean_names() %>%
              # Take out missing observations at bottom of the table, it's just metadata on copyright
              filter(!is.na(year), str_count(year) <= 4)) %>%
  # Add 2016 scores
  bind_rows(read_csv("Input/ODIN_scores_2016.csv") %>%
              # convert all variable names to snakecase
              janitor::clean_names() %>%
              # Take out missing observations at bottom of the table, it's just metadata on copyright
              filter(!is.na(year), str_count(year) <= 4)) %>%
  # Clean variables and data categories to be able to compare scores across years
  mutate(
    # Remove white space and new lines at the end of data_categories variable. Basically get rid of trailing white space
    data_categories = str_remove_all(data_categories, "\\n|\\s$|\\r"),
    # Switch names for older data categories to new (2020)  data category names
    data_categories = case_when(
           data_categories == "Energy use" ~ "Energy",
           data_categories == "Land use" ~ "Agriculture & Land Use",
           TRUE ~ data_categories
         ),
    # Convert year variable to numeric
    year = as.numeric(year)) %>%
  # Convert to long
  pivot_longer(indicator_coverage_and_disaggregation:overall_score, names_to = "element", values_to = "score") %>%
  # Clean and add generate variables of interest
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
      data_categories %in% c("Agriculture & Land Use", "Land use", "Resource use", "Energy", "Energy use", "Pollution", "Built environment") ~ "Environmental statistics",
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
           ),
    # Fix country code for Andorra, which is still in ADO, the old code. Fix before merging in other data sources
    country_code = case_when(
      country == "Andorra" ~ "AND",
      TRUE ~ country_code
    ),
    country = case_when(
      country_code == "COD" ~ "Congo, Dem. Rep.",
      country_code == "COG" ~ "Congo, Rep.",
      country_code == "HKG" ~ "Hong Kong SAR, China",
      country_code == "IRN" ~ "Iran, Islamic Rep.",
      country_code == "KOR" ~ "Korea, Rep.",
      country_code == "MAC" ~ "Macao SAR, China",
      country_code == "FSM" ~ "Micronesia, Fed. Sts.",
      TRUE ~ country
    )) %>%
  # Merge in World Bank income groups and regions
  # From https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups
  # File "current classification by income in XLSX format"
  left_join(read_csv("Input/wb_countries_fy23.csv") %>%
              janitor::clean_names() %>%
              filter(!is.na(region)) %>%
              mutate(income_group = fct_relevel(income_group, "Low income", "Lower middle income", "Upper middle income", "High income")) %>%
              select(code, wb_region = region, income_group, lending_category), by = c("country_code" = "code")) %>%
  # Merge in UNSD Country Groups for SIDS, LDC, and LLDC
  # From https://unstats.un.org/unsd/methodology/m49/overview/
  left_join(read_csv("Input/unsd_country_groups.csv") %>% 
              janitor::clean_names() %>% 
              select(country_code = iso_alpha3_code, ldc = least_developed_countries_ldc, lldc = land_locked_developing_countries_lldc, sids = small_island_developing_states_sids)) %>%
  # Make x and NA to yes and no for three UNSD country groupings
  mutate(across(ldc:sids, ~case_when(.x == "x" ~ "yes", TRUE ~ "no")),
  # Add small/large country designation
         country_size_status = case_when(
           country_code %in% c("AND", "AIA", "ATG", "ABW", "BHR", "BRB", "DMA", "GRD", "HKG", "KIR", "LIE", "MAC", "MDV", "MLT", "MHL", "FSM", "MCO", "NRU", 	"PLW", 
                               "STP", "SMR", "SYC", "SGP", "KNA", "LCA", "VCT", "TON", "TUV") ~ "Small country",
           TRUE ~ "Large country"
         ),
  score = case_when(
    (element == "Second administrative level" & country_size_status == "Small country") ~ NA_real_,
    (element == "First administrative level" & country_size_status == "Small country" & data_categories %in% c("National accounts", "Price indexes", "Government finance", "Money & banking", 
                                                                                                            "International trade", "Balance of payments", "Resource use", "Energy", "Pollution", "Food security & nutrition")) ~ NA_real_,
    (element == "Second administrative level" & country_size_status == "Large country" & data_categories %in% c("National accounts", "Price indexes", "Government finance", "Money & banking", 
                                                                                                              "International trade", "Balance of payments", "Resource use", "Energy", "Pollution")) ~ NA_real_,
    (element == "First administrative level" & country_size_status == "Large country" & data_categories %in% c("Money & banking", "International trade", "Balance of payments", "Energy")) ~ NA_real_,
    TRUE ~ score
  ))
  

#### IMF Dissemination standards subscriber countries
# From https://dsbb.imf.org/sdds/country
# and https://dsbb.imf.org/sdds-plus/country
# and https://dsbb.imf.org/e-gdds/country
dissemination_standards <- read_csv("Input/imf_dissemination_standards_2022.csv") %>%
  mutate(iso3c = countrycode::countrycode(country, "country.name", "iso3c"),
         iso3c = case_when(
           str_detect(country, "Kosovo") & is.na(iso3c) ~ "XKX",
           str_detect(country, "Macao") & is.na(iso3c) ~ "MAC",
           TRUE ~ iso3c
         )) %>%
  pivot_longer(cols = sdds_plus:sdds, names_to = "dissemination_subscriber", values_to = "membership") %>% 
  filter(!is.na(membership)) %>%
  select(iso3c, dissemination_subscriber)

#### EXPLORATORY DATA ANALYSIS ####
# Start with shorter general discussion of scores by categories for 
# 2022 and trends since 2016. Are certain coverage or openness lacking 
# in certain categories more than others? Can do something similar to 
# previous year’s report. (Lorenz) 

#### Replicate Figure 2 from ODIN 2020/2021 Executive Summary ####
odin_scores %>% 
  filter(data_categories == "All Categories", 
         element %in% c("Overall score", "Coverage subscore", "Openness subscore")) %>% 
  group_by(element, year) %>% 
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup()

# Average over time
odin_scores %>% 
  filter(data_categories == "All Categories", 
         element %in% c("Overall score", "Coverage subscore", "Openness subscore")) %>% 
  group_by(element, year) %>% 
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = mean_score, color = element, label = round(mean_score, 1))) +
  geom_line() +
  geom_text(vjust = 0, nudge_y = 0.5, show.legend = FALSE) +
  labs(x ="", y = "Average Score for All Categories") + 
  scale_y_continuous(limits = c(35,60)) +
  theme(legend.title = element_blank())
ggsave("Graphs/Average overall score 2016-2022.png", dpi = 400)

#### Replicate Figure 3 from ODIN 2020/2021 Report ####
# ODIN scores by income group, 2016-2022
odin_scores %>% 
  filter(data_categories == "All Categories", element == "Overall score", !is.na(income_group)) %>%
  group_by(income_group, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = mean_score, color = income_group)) +
  geom_line() +
  labs(x ="", y = "Average Overall Score for All Categories") + 
  theme(legend.title = element_blank())
ggsave("Graphs/Average overall score by income goup 2016-2022.png", dpi = 400)

#### Replicate Figure 4 from ODIN 2020/2021 Report ####
# Change in average ODIN scores by region, 2016-2022 (%) 
odin_scores %>% 
  filter(data_categories == "All Categories", element == "Overall score", year %in% c(2016, 2022)) %>%
  group_by(region, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = region, names_from = year, names_prefix = "year", values_from = mean_score) %>%
  mutate(avg_change = (year2022/year2016 - 1)*100) %>%
  ggplot(aes(x = fct_reorder(region, avg_change), y = avg_change)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "", y = "Pct change in Average Overall Score for All Categories, 2016-2022")
ggsave("Graphs/Pct change in Avg Overall score by region 2016-2022.png", dpi = 400)

#### Table 1 from ODIN 2020/2021 Report ####
# Change in element scores, 2016-2022
odin_scores %>%
  # Compute average across all years by element
  filter(data_categories == "All Categories", !element %in% c("Coverage subscore", "Openness subscore", "Overall score")) %>%
  group_by(macro_element, element) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(odin_scores %>%
              # Compute percent change between 2016 and 2022
              filter(data_categories == "All Categories", !element %in% c("Coverage subscore", "Openness subscore", "Overall score"), year %in% c(2016, 2022)) %>%
              group_by(macro_element, element, year) %>%
              summarize(mean_score = mean(score, na.rm = TRUE)) %>%
              ungroup() %>%
              pivot_wider(id_cols = c(macro_element, element), names_from = year, names_prefix = "year", values_from = mean_score) %>%
              mutate(avg_change = (year2022/year2016 - 1)*100) %>%
              select(macro_element, element, avg_change))

#### Change over time of element scores
odin_scores %>%
  # Compute average across all years by element
  filter(data_categories == "All Categories", !element %in% c("Coverage subscore", "Openness subscore", "Overall score")) %>%
  group_by(macro_element, element, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(macro_element == "Coverage elements") %>%
  ggplot(aes(x = year, y = mean_score, color = element)) +
  geom_line() +
  scale_y_continuous(limits = c(0,100)) +
  labs(x = "", y = "Average score for all categories", title = "Coverage elements 2016-2022")

odin_scores %>%
  # Compute average across all years by element
  filter(data_categories == "All Categories", !element %in% c("Coverage subscore", "Openness subscore", "Overall score")) %>%
  group_by(macro_element, element, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(macro_element == "Openness elements") %>%
  ggplot(aes(x = year, y = mean_score, color = element)) +
  geom_line() +
  scale_y_continuous(limits = c(0,100)) +
  labs(x = "", y = "Average score for all categories", title = "Openness elements 2016-2022")


#### Table 3 Median ODIN Scores by income group, 2022 ####
odin_scores %>%
  filter(year == 2022, data_categories == "All Categories", element %in% c("Overall score", "Coverage subscore", "Openness subscore"), !is.na(income_group)) %>%
  group_by(income_group, element) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = income_group, names_from = element, values_from = median_score) %>%
  bind_rows(odin_scores %>%
              filter(year == 2022, data_categories == "All Categories", element %in% c("Overall score", "Coverage subscore", "Openness subscore"), income_group %in% c("High income", "Low income")) %>%
              group_by(income_group, element) %>%
              summarize(median_score = median(score, na.rm = TRUE)) %>%
              ungroup() %>%
              pivot_wider(id_cols = element, names_from = income_group, values_from = median_score) %>%
              mutate(diff_low_hi = `High income` - `Low income`) %>%
              select(element, diff_low_hi) %>%
              pivot_wider(names_from = element, values_from = diff_low_hi) %>%
              mutate(income_group = "Difference between high- and low-income", .before = `Coverage subscore`)
  )

#### Figures 8 and 9 from ODIN 2020/21 report ####
odin_scores %>%
  filter(year == 2022, data_categories == "All Categories", !element %in% c("Overall score", "Coverage subscore", "Openness subscore"), !is.na(income_group)) %>%
  group_by(macro_element, element, income_group) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(macro_element == "Openness elements") %>%
  ggplot(aes(x = income_group, y = median_score, group = as.factor(element), color = as.factor(element))) +
  geom_line() +
  scale_y_continuous(limits = c(0,100)) +
  labs(x = "", y = "Median Score across All Categories", title = "Openness Elements by income group") +
  theme(legend.title = element_blank())
ggsave("Graphs/Openness elements by income group 2022.png", dpi = 400)

odin_scores %>%
  filter(year == 2022, data_categories == "All Categories", !element %in% c("Overall score", "Coverage subscore", "Openness subscore"), !is.na(income_group)) %>%
  group_by(macro_element, element, income_group) %>%
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(macro_element == "Coverage elements") %>%
  ggplot(aes(x = income_group, y = median_score, group = as.factor(element), color = as.factor(element))) +
  geom_line() +
  scale_y_continuous(limits = c(0,100)) +
  labs(title = "Coverage Elements by income group") +
  labs(x = "", y = "Median Score across All Categories", title = "Coverage Elements by income group") +
  theme(legend.title = element_blank())
ggsave("Graphs/Coverage elements by income group 2022.png", dpi = 400)

#### ODIN scores by income group 2020
odin_scores %>%
  filter(year == 2022, data_categories == "All Categories", element == "Overall score", !is.na(income_group)) %>%
  left_join(
    # World Bank GNI per capita
    wbstats::wb_data(c("gni_pc" = "NY.GNP.PCAP.CD"), mrnev = 1) %>%
      select(iso3c, gni_pc), by = c("country_code" = "iso3c")) %>%
  ggplot(aes(x = gni_pc, y = score, color = income_group)) +
  geom_point() +
  scale_x_log10(labels = scales::comma) +
  stat_smooth(method="lm", formula=y~1, se=FALSE) +
  scale_y_continuous(limits = c(0,100)) +
  labs(x = "GNI per capita (logged)", y = "ODIN Overall Score 2022", color = "WB FY2023\nIncome Groups")
ggsave("Graphs/Country ODIN scores against GNI pc.png", dpi = 400)

# Correlation between GNI pc and scores
data4reg <- odin_scores %>%
  filter(year == 2022, data_categories == "All Categories", element %in% c("Overall score", "Coverage subscore", "Openness subscore")) %>%
  left_join(
    # World Bank GNI per capita
    wbstats::wb_data(c("gni_pc" = "NY.GNP.PCAP.CD"), mrnev = 1) %>%
      select(iso3c, gni_pc), by = c("country_code" = "iso3c")) %>%
  pivot_wider(id_cols = c(country, country_code, region, region_code, wb_region, income_group, lending_category, ldc, lldc, sids, gni_pc), names_from = element, values_from = score)
  

lm_model <- linear_reg() %>%
  set_engine('lm') %>%
  set_mode('regression')

lm_model

lm_fit <- lm_model %>%
  fit(`Overall score` ~ log10(gni_pc), data = data4reg)

# Plot all three main scores against gni_pc
data4reg %>%
  ggplot() +
  geom_point(aes(x = gni_pc, y = `Overall score`)) +
  geom_smooth(method = "lm", aes(x = gni_pc, y = `Overall score`, color = "Overall score")) +
  geom_smooth(method = "lm", aes(x = gni_pc, y = `Coverage subscore`, color = "Coverage subscore")) +
  geom_smooth(method = "lm", aes(x = gni_pc, y = `Openness subscore`, color = "Openness subscore")) +
  scale_x_log10(labels = scales::comma) +
  scale_y_continuous(limits = c(0,100)) +
  labs(x = "GNI per capita (logged)", y = "ODIN Overall Score 2022")

# Test tidy correlations
# using https://www.tidymodels.org/learn/statistics/tidy-analysis/
odin_scores %>%
  filter(year == 2022, data_categories == "All Categories", element %in% c("Overall score", "Coverage subscore", "Openness subscore")) %>%
  left_join(
    # World Bank GNI per capita
    wbstats::wb_data(c("gni_pc" = "NY.GNP.PCAP.CD"), mrnev = 1) %>%
      select(iso3c, gni_pc), by = c("country_code" = "iso3c")) %>%
  filter(!is.na(score), !is.na(gni_pc)) %>%
  group_by(element) %>%
  summarize(correlation = cor(score, log10(gni_pc), use = "pairwise.complete.obs"))

data4reg <- odin_scores %>%
  filter(year == 2022, data_categories == "All Categories", element %in% c("Overall score", "Coverage subscore", "Openness subscore")) %>%
  left_join(
    # World Bank GNI per capita
    wbstats::wb_data(c("gni_pc" = "NY.GNP.PCAP.CD"), mrnev = 1) %>%
      select(iso3c, gni_pc), by = c("country_code" = "iso3c")) %>%
  filter(!is.na(score), !is.na(gni_pc)) %>%
  mutate(log_gni_pc = log10(gni_pc))

ct <- cor.test(data4reg$score, data4reg$log_gni_pc)
tidy(ct)

nested <-
  data4reg %>%
  select(element, score, log_gni_pc) %>%
  nest(data = c(score, log_gni_pc))

nested %>% 
  mutate(test = map(data, ~ cor.test(.x$score, .x$log_gni_pc)), # S3 list-col
         tidied = map(test, tidy)
         ) %>% 
  unnest(cols = tidied) %>%
  select(-c(data, test))

# Multiple regressions
data4reg %>%
  select(element, score, log_gni_pc) %>%
  nest(data = c(-element)) %>%
  mutate(
    fit = map(data, ~ lm(score ~ log_gni_pc, data = .x)),
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied) %>% 
  select(-c(data, fit))

#### FIGURE 11: Coverage by category, 2018 vs 2020 ####
# Make sorting order based on average coverage scores in 2018
sel_order <- 
  odin_scores %>% 
  # Restrict to Coverage subscore and 2018
  filter(element == "Coverage subscore", year == 2018) %>% 
  # Make average by data category
  group_by(data_categories) %>%
  summarize(value = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  # Sort by best coverage in 2018
  arrange(desc(value)) %>% 
  # Create label that inherits ordering
  mutate(labels = factor(data_categories))

# Create graph
odin_scores %>%
  # Filter for only coverage subscore, filter out subscores that don't
  # exist in both years or that are main scores.
  filter(element == "Coverage subscore",
         !data_categories %in% c("Food security & nutrition", "Economic & financial statistics subscore",
                                 "All Categories", "Environment subscore", "Social statistics subscore"),
         year %in% c(2018, 2020)) %>%
  # Make average by data category and year
  group_by(data_categories, year) %>%
  summarize(mean_availability = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  # create label variable that inherits ordering from sorting order df
  mutate(labels = factor(data_categories, levels = sel_order$labels, ordered = TRUE)) %>%
  # Create ggplot, using new ordered label variable as x axis,
  # splitting it by year (fill)
  ggplot(aes(x = labels, y = mean_availability, fill = as.factor(year))) +
  # Calling column geom with dodge position so grouped bar will be side by side
  geom_col(position = "dodge") +
  # Rotate x axis labels, remove legend title, position legend inside graph
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank(), legend.position = c(0.9, 0.8)) +
  # Labels
  labs(x = "", y = "Average score", title = "Coverage, ODIN 2018 and 2020") +
  # Extend y axis from 0 to 100
  scale_y_continuous(limits = c(0, 100))

# Create graph with facets for macro-sectors
odin_scores %>%
  # Filter for only coverage subscore, filter out subscores that don't
  # exist in both years or that are main scores.
  filter(element == "Coverage subscore",
         !data_categories %in% c("Economic & financial statistics subscore",
                                 "All Categories", "Environment subscore", "Social statistics subscore"),
         year %in% c(2018, 2020)) %>%
  # Make average by data category and year
  group_by(macro_sector, data_categories, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  # Fill in row for Food security and nutrition so bar shows up for 2018. Value will be 0 so it doesn't display
  add_row(macro_sector = "Social statistics", data_categories = "Food security & nutrition",
          year = 2018, mean_score = 0) %>%
  # create label variable that inherits ordering from sorting order df
  mutate(labels = factor(data_categories, levels = sel_order$labels, ordered = TRUE),
         # Replace missing labels factor with name of food security & nutrition so label displays on plot
         labels = fct_explicit_na(labels, "Food security & nutrition")) %>%
  # Create ggplot, using new ordered label variable as x axis,
  # splitting it by year (fill)
  ggplot(aes(x = labels, y = mean_score, fill = as.factor(year))) +
  # Calling column geom with dodge position so grouped bar will be side by side
  geom_col(position = "dodge") +
  # Rotate x axis labels, remove legend title, position legend inside graph
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank(), legend.position = c(0.95, 0.75)) +
  # Labels
  labs(x = "", y = "Average score", title = "Coverage, ODIN 2018 and 2020") +
  # Extend y axis from 0 to 100
  scale_y_continuous(limits = c(0, 100)) + 
  facet_grid(cols = vars(macro_sector), scales = "free_x", space = "free_x")
ggsave("Output/Coverage sectors 2018 v 2020.png", dpi = 400)
  
#### FIGURE 11: Openness by category, 2018 vs 2020 ####
# Make sorting order based on average openness scores in 2018
sel_order <- 
  odin_scores %>% 
  # Restrict to Openness subscore and 2018
  filter(element == "Openness subscore", year == 2018) %>% 
  # Make average by data category
  group_by(data_categories) %>%
  summarize(value = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  # Sort by best coverage in 2018
  arrange(desc(value)) %>% 
  # Create label that inherits ordering
  mutate(labels = factor(data_categories))

# Create graph
odin_scores %>%
  # Filter for only openness subscore, filter out subscores that don't
  # exist in both years or that are main scores.
  filter(element == "Openness subscore",
         !data_categories %in% c("Food security & nutrition", "Economic & financial statistics subscore",
                                 "All Categories", "Environment subscore", "Social statistics subscore"),
         year %in% c(2018, 2020)) %>%
  # Make average by data category and year
  group_by(data_categories, year) %>%
  summarize(mean_availability = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  # create label variable that inherits ordering from sorting order df
  mutate(labels = factor(data_categories, levels = sel_order$labels, ordered = TRUE)) %>%
  # Create ggplot, using new ordered label variable as x axis,
  # splitting it by year (fill)
  ggplot(aes(x = labels, y = mean_availability, fill = as.factor(year))) +
  # Calling column geom with dodge position so grouped bar will be side by side
  geom_col(position = "dodge") +
  # Rotate x axis labels, remove legend title, position legend inside graph
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank(), legend.position = c(0.9, 0.8)) +
  # Labels
  labs(x = "", y = "Average score", title = "Openness, ODIN 2018 and 2020") +
  # Extend y axis from 0 to 100
  scale_y_continuous(limits = c(0, 100))

# Create graph with facets for macro-sectors
odin_scores %>%
  # Filter for only coverage subscore, filter out subscores that don't
  # exist in both years or that are main scores.
  filter(element == "Openness subscore",
         !data_categories %in% c("Economic & financial statistics subscore",
                                 "All Categories", "Environment subscore", "Social statistics subscore"),
         year %in% c(2018, 2020)) %>%
  # Make average by data category and year
  group_by(macro_sector, data_categories, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  # Fill in row for Food security and nutrition so bar shows up for 2018. Value will be 0 so it doesn't display
  add_row(macro_sector = "Social statistics", data_categories = "Food security & nutrition",
          year = 2018, mean_score = 0) %>%
  # create label variable that inherits ordering from sorting order df
  mutate(labels = factor(data_categories, levels = sel_order$labels, ordered = TRUE),
         # Replace missing labels factor with name of food security & nutrition so label displays on plot
         labels = fct_explicit_na(labels, "Food security & nutrition")) %>%
  # Create ggplot, using new ordered label variable as x axis,
  # splitting it by year (fill)
  ggplot(aes(x = labels, y = mean_score, fill = as.factor(year))) +
  # Calling column geom with dodge position so grouped bar will be side by side
  geom_col(position = "dodge") +
  # Rotate x axis labels, remove legend title, position legend inside graph
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank(), legend.position = c(0.95, 0.75)) +
  # Labels
  labs(x = "", y = "Average score", title = "Openness, ODIN 2018 and 2020") +
  # Extend y axis from 0 to 100
  scale_y_continuous(limits = c(0, 100)) + 
  facet_grid(cols = vars(macro_sector), scales = "free_x", space = "free_x")
ggsave("Output/Openness sectors 2018 v 2020.png", dpi = 400)

#### FIGURE 12: Ranking change of data category 2018 to 2020 ####
# Adapting style here
# https://www.statology.org/bump-chart-in-r-using-ggplot2/

# Custom theme from link above
my_theme <- function() {
  
  # Colors
  color.background = "white"
  color.text = "#22211d"
  
  # Begin construction of chart
  theme_bw(base_size=15) +
    
    # Format background colors
    theme(panel.background = element_rect(fill=color.background,
                                          color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background,
                                          color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
    theme(strip.background = element_rect(fill=color.background,
                                          color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks       = element_blank()) +
    
    # Format the legend
    theme(legend.position = "none") +
    
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=12, face = "bold")) +
    theme(axis.title.x     = element_text(size=10, color="black", face = "bold")) +
    theme(axis.title.y     = element_text(size=10, color="black", face = "bold",
                                          vjust=1.25)) +
    theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5,
                                          color = color.text)) +
    theme(axis.text.y      = element_text(size=10, color = color.text)) +
    theme(strip.text       = element_text(face = "bold")) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

# Create chart
rank_data <- odin_scores %>%
  # Filter for only overall subscore, filter out subscores that are main scores.
  filter(element == "Overall score",
         !data_categories %in% c("Economic & financial statistics subscore",
                                 "All Categories", "Environment subscore", "Social statistics subscore"),
         year %in% c(2018, 2020)) %>%
  # Make average by data category and year
  group_by(macro_sector, data_categories, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(year, macro_sector) %>%
  arrange(year, desc(mean_score), data_categories) %>% 
  mutate(rank = row_number()) %>%
  ungroup()
# Chart for Environmental Statistics
rank_data %>%
  filter(macro_sector == "Environmental statistics") %>%
  ggplot(aes(x = as.factor(year), y = rank, group = data_categories)) +
  geom_line(aes(color = data_categories, alpha = 1), size = 2) +
  geom_point(aes(color = data_categories, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:22) + 
  scale_x_discrete() +
  theme(legend.position = 'none') +
  geom_text(data = rank_data %>% filter(year == 2018, macro_sector == "Environmental statistics"),
            aes(label = data_categories) , hjust = 1, nudge_x = -0.05,
            fontface = "bold", color = "#888888", size = 3) +
  geom_text(data = rank_data %>% filter(year == 2020, macro_sector == "Environmental statistics"),
            aes(label = data_categories) , hjust = 0, nudge_x = 0.05,
            fontface = "bold", color = "#888888", size = 3) +
  labs(x = 'Year', y = 'Rank', title = 'Overall Score ranking of Environmental Data Categories') +
  my_theme()
ggsave("Output/Rank change Environmental statistics.png", dpi = 400)
# Chart for Social Statistics
rank_data %>%
  filter(macro_sector == "Social statistics") %>%
  ggplot(aes(x = as.factor(year), y = rank, group = data_categories)) +
  geom_line(aes(color = data_categories, alpha = 1), size = 2) +
  geom_point(aes(color = data_categories, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:22) + 
  scale_x_discrete() +
  theme(legend.position = 'none') +
  geom_text(data = rank_data %>% filter(year == 2018, macro_sector == "Social statistics"),
            aes(label = data_categories) , hjust = 1, nudge_x = -0.05,
            fontface = "bold", color = "#888888", size = 3) +
  geom_text(data = rank_data %>% filter(year == 2020, macro_sector == "Social statistics"),
            aes(label = data_categories) , hjust = 0, nudge_x = 0.05,
            fontface = "bold", color = "#888888", size = 3) +
  labs(x = 'Year', y = 'Rank', title = 'Overall Score ranking of Social Data Categories') +
  my_theme()
ggsave("Output/Rank change Social statistics.png", dpi = 400)
# Chart for Economic Statistics
rank_data %>%
  filter(macro_sector == "Economic and financial statistics") %>%
  ggplot(aes(x = as.factor(year), y = rank, group = data_categories)) +
  geom_line(aes(color = data_categories, alpha = 1), size = 2) +
  geom_point(aes(color = data_categories, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:22) + 
  scale_x_discrete() +
  theme(legend.position = 'none') +
  geom_text(data = rank_data %>% filter(year == 2018, macro_sector == "Economic and financial statistics"),
            aes(label = data_categories) , hjust = 1, nudge_x = -0.05,
            fontface = "bold", color = "#888888", size = 3) +
  geom_text(data = rank_data %>% filter(year == 2020, macro_sector == "Economic and financial statistics"),
            aes(label = data_categories) , hjust = 0, nudge_x = 0.05,
            fontface = "bold", color = "#888888", size = 3) +
  labs(x = 'Year', y = 'Rank', title = 'Overall Score ranking of Economic and Financial Data Categories') +
  my_theme()
ggsave("Output/Rank change Economic and financial statistics.png", dpi = 400)

#### Replicate Figure 6 on % that published data and corresponding coverage score ####
odin_scores %>%
  filter(year == 2020,
         !data_categories %in% c("Food security & nutrition", "Economic & financial statistics subscore",
                                 "All Categories", "Environment subscore", "Social statistics subscore"),
         element == "Indicator coverage and disaggregation") %>%
  mutate(score = case_when(
    score > 0 ~ 1,
    TRUE ~ score
  )) %>%
  group_by(data_categories) %>%
  summarize(pct_available = mean(score, na.rm = TRUE)*100) %>%
  ungroup() %>%
  left_join(
    odin_scores %>%
      filter(year == 2020,
             !data_categories %in% c("Food security & nutrition", "Economic & financial statistics subscore",
                                     "All Categories", "Environment subscore", "Social statistics subscore"),
             element %in% c("Coverage subscore", "Indicator coverage and disaggregation")) %>%
      select(country_code, data_categories, element, score) %>%
      pivot_wider(id_cols = c(country_code, data_categories), names_from = element, values_from = score) %>%
      filter(`Indicator coverage and disaggregation` > 0) %>%
      group_by(data_categories) %>%
      summarize(mean_coverage = mean(`Coverage subscore`, na.rm = TRUE)) %>%
      ungroup()
  ) %>%
  ggplot(aes(x = reorder(data_categories, -pct_available))) + 
  geom_col(aes(y = pct_available, fill = "Countries with\npublished data (%)")) +
  geom_point(aes(y = mean_coverage, color = "Average coverage for\ncountries with data (index)")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank(), legend.position = "bottom",
        legend.margin=margin(t = -1.5, unit='cm')) +
  # Labels
  labs(x = "", y = "", title = "Countries with published data do not necessarily have great coverage") +
  # the labels must match what you specified above
  scale_fill_manual(name = "", values = c("Countries with\npublished data (%)" = "grey")) +
  scale_color_manual(name = "", values = c("Average coverage for\ncountries with data (index)" = "black"))

#### FIGURE 10: Graph of how overall scores of category subscores have increased ####
odin_scores %>%
  filter(data_categories %in% c("Economic & financial statistics subscore",
                                "Environment subscore", "Social statistics subscore"),
         element == "Overall score") %>%
  group_by(data_categories, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = data_categories, group = data_categories)) +
  geom_line() +
  geom_point() +
  labs(x = "", y = "Average Overall score", color = "") +
  scale_x_discrete(limits = c("2016", "2017", "2018", "", "2020", "", "2022"))
ggsave("Graphs/Change of rank of sectors over time.png", dpi = 400)

#### Graph of how Overall scores of data categories within macro-sectors have changed ####
odin_scores %>%
  filter(!data_categories %in% c("Economic & financial statistics subscore",
                                "Environment subscore", "Social statistics subscore", "All Categories"),
         element == "Overall score") %>%
  group_by(macro_sector, data_categories, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = data_categories, group = data_categories)) +
  geom_line() +
  geom_point() +
  facet_wrap(~macro_sector, scales = "free") +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(15, 85)) +
  scale_x_discrete(limits = c("2016", "2017", "2018", "", "2020", "", "2022")) +
  labs(x = "", y = "Average overall score")

# Economic and financial statistics
odin_scores %>%
  filter(!data_categories %in% c("Economic & financial statistics subscore",
                                 "Environment subscore", "Social statistics subscore", "All Categories"),
         element == "Overall score") %>%
  group_by(macro_sector, data_categories, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(macro_sector == "Economic and financial statistics") %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = data_categories, group = data_categories)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(15, 85)) +
  scale_x_discrete(limits = c("2016", "2017", "2018", "", "2020", "", "2022")) +
  labs(x = "", y = "Average overall score", title = "Economic and financial statistics")
ggsave("Graphs/Economic and financial sub-categories 2016-2022.png",dpi = 400)

# Environmental Statistics
odin_scores %>%
  filter(!data_categories %in% c("Economic & financial statistics subscore",
                                 "Environment subscore", "Social statistics subscore", "All Categories"),
         element == "Overall score") %>%
  group_by(macro_sector, data_categories, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(macro_sector == "Environmental statistics") %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = data_categories, group = data_categories)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(15, 85)) +
  scale_x_discrete(limits = c("2016", "2017", "2018", "", "2020", "", "2022")) +
  labs(x = "", y = "Average overall score", title = "Environmental statistics")
ggsave("Graphs/Environmental sub-categories 2016-2022.png",dpi = 400)

#### Graph of how Coverage scores of data categories within macro-sectors have changed ####
odin_scores %>%
  filter(!data_categories %in% c("Economic & financial statistics subscore",
                                 "Environment subscore", "Social statistics subscore", "All Categories"),
         element == "Coverage subscore") %>%
  group_by(macro_sector, data_categories, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = data_categories, group = data_categories)) +
  geom_line() +
  geom_point() +
  facet_wrap(~macro_sector, scales = "free") +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(15, 85)) +
  scale_x_discrete(limits = c("2016", "2017", "2018", "", "2020", "", "2022")) +
  labs(x = "", y = "Average coverage score")

# Economic and financial statistics
odin_scores %>%
  filter(!data_categories %in% c("Economic & financial statistics subscore",
                                 "Environment subscore", "Social statistics subscore", "All Categories"),
         element == "Coverage subscore") %>%
  group_by(macro_sector, data_categories, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(macro_sector == "Economic and financial statistics") %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = data_categories, group = data_categories)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(15, 85)) +
  scale_x_discrete(limits = c("2016", "2017", "2018", "", "2020", "", "2022")) +
  labs(x = "", y = "Average coverage score", title = "Economic and financial statistics")

# Economic and financial statistics
odin_scores %>%
  filter(!data_categories %in% c("Economic & financial statistics subscore",
                                 "Environment subscore", "Social statistics subscore", "All Categories"),
         element == "Coverage subscore") %>%
  group_by(macro_sector, data_categories, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(macro_sector == "Environmental statistics") %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = data_categories, group = data_categories)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(15, 85)) +
  scale_x_discrete(limits = c("2016", "2017", "2018", "", "2020", "", "2022")) +
  labs(x = "", y = "Average coverage score", title = "Environmental statistics")

#### Graph of how Openness scores of data categories within macro-sectors have changed ####
odin_scores %>%
  filter(!data_categories %in% c("Economic & financial statistics subscore",
                                 "Environment subscore", "Social statistics subscore", "All Categories"),
         element == "Openness subscore") %>%
  group_by(macro_sector, data_categories, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = data_categories, group = data_categories)) +
  geom_line() +
  geom_point() +
  facet_wrap(~macro_sector, scales = "free") +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(15, 85)) +
  scale_x_discrete(limits = c("2016", "2017", "2018", "", "2020", "", "2022")) +
  labs(x = "", y = "Average openness score")

# Economic and financial statistics
odin_scores %>%
  filter(!data_categories %in% c("Economic & financial statistics subscore",
                                 "Environment subscore", "Social statistics subscore", "All Categories"),
         element == "Openness subscore") %>%
  group_by(macro_sector, data_categories, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(macro_sector == "Economic and financial statistics") %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = data_categories, group = data_categories)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(15, 85)) +
  scale_x_discrete(limits = c("2016", "2017", "2018", "", "2020", "", "2022")) +
  labs(x = "", y = "Average openness score", title = "Economic and financial statistics")

# Environmental
odin_scores %>%
  filter(!data_categories %in% c("Economic & financial statistics subscore",
                                 "Environment subscore", "Social statistics subscore", "All Categories"),
         element == "Openness subscore") %>%
  group_by(macro_sector, data_categories, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(macro_sector == "Environmental statistics") %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = data_categories, group = data_categories)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(15, 85)) +
  scale_x_discrete(limits = c("2016", "2017", "2018", "", "2020", "", "2022")) +
  labs(x = "", y = "Average openness score", title = "Environmental statistics")

#### Change in Element scores within Energy data category ####
# Investigate which elements behind energy have most rapidly increased
# As energy has increased across overall, coverage, and openness
odin_scores %>%
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
  labs(x = "", y = "Average score") +
  theme(legend.position = "none") +
  scale_x_discrete(limits = c("2016", "2017", "2018", "", "2020", "", ""))

#### HEALTH ####
# Health section- Focus on scores from the 3 health categories, 
# identify trends or interesting findings from 2020. Bring in Global 
# Health 50/50 COVID data or other COVID related data (Lorenz) 

#### Overall Trends ####
odin_scores %>%
  filter(data_categories %in% c("Health facilities", "Health outcomes", "Population & vital statistics", "Reproductive health"),
         element %in% c("Overall score", "Coverage subscore", "Openness subscore")) %>%
  group_by(data_categories, element, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = data_categories, group = data_categories)) +
  geom_line(size = 1.2) + 
  geom_point(data = odin_scores %>%
               filter(data_categories %in% c("Food security & nutrition"),
                      element %in% c("Overall score", "Coverage subscore", "Openness subscore")) %>%
               group_by(data_categories, element, year) %>%
               summarize(mean_score = mean(score, na.rm = TRUE)) %>%
               ungroup()) +
  facet_wrap(~element) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(x = "", y = "Average score")

#### FIGURE 13: Overall State in 2020 ####
odin_scores %>%
  filter(data_categories %in% c("Health facilities", "Health outcomes", "Population & vital statistics", "Reproductive health", "Food security & nutrition"),
         element %in% c("Coverage subscore", "Openness subscore"), year == 2020) %>%
  group_by(data_categories, element, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  # Order coverage and openess properly so they show up on graph
  mutate(element = fct_relevel(element, "Openness subscore", "Coverage subscore")) %>%
  ggplot(aes(x = reorder(data_categories, mean_score), y = mean_score, fill = element, color = element)) +
  geom_col(position = "dodge", width = 0.5) +
  # geom_text(aes(label = text_label), position = position_dodge(0.9), hjust = 0, size = 3.5) +
  coord_flip() +
  # Load library(extrafont) to use Open Sans font
  theme(text = element_text(family = "Open Sans"), 
        # Make legend without title, change position
        legend.position = "bottom", legend.title = element_blank(),
        # Add some padding to right of plot to make 100 show up
        plot.margin = margin(5,10,5,5),
        # Create custom theme to remove grey background, add x axis line
        # back in, add major grid lines and make tick marks same color
        panel.background = element_rect(fill = "white"),
        axis.line.y = element_line(color = "black"),
        panel.grid.major.x = element_line(color = "grey"),
        axis.ticks.x = element_line(color = "grey")) +
  # Blank x and y axis labels and no title
  labs(x = "", y = "") +
  # Y axis scale with custom limits, breaks and moving it snug to x axis
  scale_y_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100), expand = c(0,0)) +
  # Custom ODIN report colors
  scale_fill_manual(values = c("#06b2eb", "#fa8e00")) +
  scale_color_manual(values = c("#06b2eb", "#fa8e00"))
ggsave("Output/Figure 13 - The state of health-related indicators in 2020.png", dpi = 400)

#### Examine coverage and openness elements by health categories ####
# Health Facilities
odin_scores %>%
  filter(data_categories %in% c("Health facilities", "Health outcomes", "Population & vital statistics", "Reproductive health"),
         macro_element %in% c("Coverage elements", "Openness elements")) %>%
  group_by(data_categories, macro_element, element, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(data_categories == "Health facilities") %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = element, group = element)) +
  geom_line(size = 1.2) + 
  facet_wrap(~macro_element) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 5)) +
  guides(shape = guide_legend(override.aes = list(size = 1))) +
  labs(x = "", y = "Average score", title = "Health facilities")

# Health Outcomes
odin_scores %>%
  filter(data_categories %in% c("Health facilities", "Health outcomes", "Population & vital statistics", "Reproductive health"),
         macro_element %in% c("Coverage elements", "Openness elements")) %>%
  group_by(data_categories, macro_element, element, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(data_categories == "Health outcomes") %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = element, group = element)) +
  geom_line(size = 1.2) + 
  facet_wrap(~macro_element) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 5)) +
  guides(shape = guide_legend(override.aes = list(size = 1))) +
  labs(x = "", y = "Average score", title = "Health outcomes")

# Population & vital statistics
odin_scores %>%
  filter(data_categories %in% c("Health facilities", "Health outcomes", "Population & vital statistics", "Reproductive health"),
         macro_element %in% c("Coverage elements", "Openness elements")) %>%
  group_by(data_categories, macro_element, element, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(data_categories == "Population & vital statistics") %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = element, group = element)) +
  geom_line(size = 1.2) + 
  facet_wrap(~macro_element) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 5)) +
  guides(shape = guide_legend(override.aes = list(size = 1))) +
  labs(x = "", y = "Average score", title = "Population & vital statistics")

# Reproductive Health
odin_scores %>%
  filter(data_categories %in% c("Health facilities", "Health outcomes", "Population & vital statistics", "Reproductive health"),
         macro_element %in% c("Coverage elements", "Openness elements")) %>%
  group_by(data_categories, macro_element, element, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(data_categories == "Reproductive health") %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = element, group = element)) +
  geom_line(size = 1.2) + 
  facet_wrap(~macro_element) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 5)) +
  guides(shape = guide_legend(override.aes = list(size = 1))) +
  labs(x = "", y = "Average score", title = "Population & vital statistics")

# Food Security & Nutrition
odin_scores %>%
  filter(data_categories == "Food security & nutrition",
         macro_element %in% c("Coverage elements", "Openness elements")) %>%
  group_by(data_categories, macro_element, element, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = element, y = mean_score)) +
  geom_col() +
  facet_wrap(~macro_element, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  # theme(legend.position = "bottom", legend.title = element_blank(),
  #       legend.text = element_text(size = 5)) +
  # guides(shape = guide_legend(override.aes = list(size = 1))) +
  labs(x = "", y = "Average score", title = "Food security & nutrition")


#### Health category change over time by region ####
odin_scores %>%
  filter(element %in% c("Overall score", "Coverage subscore", "Openness subscore"),
         data_categories %in% c("Health facilities", "Health outcomes", "Population & vital statistics", "Reproductive health")) %>%
  group_by(macro_region, region, element, data_categories, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(element == "Overall score", data_categories == "Population & vital statistics") %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = region, group = region)) +
  geom_line() + 
  facet_wrap(~macro_region) +
  labs(x = "", y = "Average overall score", title = "Population & vital statistics")

#### FIGURE 14: Examine country reporting of COVID-19 variables against ODIN health variables####
# in 2020

# Count of countries reporting each COVID-19 variable (note: total, not sex-disaggregated)
owid_data_availability %>% 
  summarize(across(case_data:excess_data, sum)) %>%
  as.data.frame()

# Join COVID-19 data availability to ODIN data and create averages
# by whether or not countries report COVID-19 data.
odin_health_covid <-
  odin_scores %>%
  # Filter for just 2020 scores, only Overall, Coverage, and Openness scores
  # And only for COVID-19 health-related ODIN categories
  filter(year == 2020, element %in% c("Overall score", "Coverage subscore", "Openness subscore"),
         data_categories %in% c("Health facilities", "Health outcomes", "Population & vital statistics")) %>%
  # Merge in Data availability, this will merge 1 to many, so filtered
  # dataset will expand. Will filter again in next step
  left_join(owid_data_availability, by = c("country_code" = "iso3c")) %>%
  # Replace all missing instances of data_available with 0, since if they're not
  # in OWID for anything, they likely don't have data/don't report it to major institutions
  mutate(data_available = case_when(
    is.na(data_available) ~ 0,
    TRUE ~ data_available
  )) %>%
  # Filter For just matching instances between data categories and the
  # covid variables that the data category would capture information for
  # Health outcomes has the following two indicators: Immunization rate
  # and Disease prevalence, which matches with Cases of COVID-19, Tests, and Vaccinations
  # Health Facilities has the following indicators: number of health facilities
  # number of beds or data on healthcare staff, and health expenditures, which
  # match COVId-19 hospital and ICU data. Finally, Population & vital statistics
  # has population counts, birth rates, and death rates, the latter of which
  # matches number of deaths from COVID-19 and the ability to calculate
  # Excess deaths.
  # Note this drops 8 countries that do not overlap between ODIN and OWID:
  # Andorra, Anguilla, Greenland, Hong Kong, Macao, Palau, Tonga, and Turkmenistan.
  filter((data_categories == "Health outcomes" & covid_variable %in% c("Cases", "Total Tests", "Total vaccinations")) |
         (data_categories == "Health facilities" & covid_variable %in% c("Hospital Admissions", "Hospital Patients", "ICU Admissions", "ICU Patients")) |
         (data_categories == "Population & vital statistics" & covid_variable %in% c("Deaths", "Excess Deaths")))

# Create bar graph of difference in scores, also create SD to create error bars
# [optional] for those countries reporting OWID data.
odin_health_covid %>%
  filter(element == "Overall score", covid_variable != "Cases") %>%
  group_by(data_categories, covid_variable, data_available) %>%
  summarize(mean_score = mean(score, na.rm = TRUE), sd = sd(score, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(label = case_when(
    data_available == 0 ~ "No",
    TRUE ~ "Yes"
  )) %>%
  ggplot(aes(x = covid_variable, y = mean_score, fill = as.factor(label))) +
  geom_col(position = "dodge", width = 0.8) +
  #geom_errorbar(aes(ymin = mean_score - sd, ymax = mean_score + sd),
  #             width = .2,
  #             position = position_dodge(0.9)) +
  facet_grid(cols = vars(data_categories), scales = "free_x", space = "free_x") +
  labs(x = "", y = "Average score", fill = "Data\nAvailable?") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0,100))
ggsave("Output/COVID-19 variable data availability and ODIN scores.png", dpi = 400)

# Test of conditional density plot to show distribution of scores across
# whether or not data is available
odin_health_covid %>% 
  filter(element == "Overall score", covid_variable == "Excess Deaths") %>% 
  ggplot(aes(x = score, fill = as.factor(data_available))) + 
  geom_density(position = "fill", alpha = 0.5)

# Test of beanplot
odin_health_covid %>% 
  filter(element == "Overall score", covid_variable == "Total Tests") %>% 
  ggplot(aes(x = as.factor(data_available), y = score)) + 
  geom_violin() +
  geom_jitter()

#### ECONOMIC ####
# Economic section- Focus on gaps in economic and financial statistics. 
# What categories are lacking? Are there certain coverage or openness 
# elements that are lower across categories? Do SDDS, SDDS+ countries 
# score higher? (Lorenz) 

#### State of Economic & Financial Statistics ####

odin_scores %>%
  filter(element %in% c("Overall score", "Coverage subscore", "Openness subscore"), 
         data_categories == "Economic & financial statistics subscore") %>%
  group_by(year, element) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(element = fct_relevel(element, "Overall score", "Coverage subscore", "Openness subscore")) %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = element, group = element)) +
  geom_line() +
  labs(x = "", y = "Average score", color = "")

#### Components of Economic & financial statistics over time ####
odin_scores %>%
  filter(element %in% c("Overall score", "Coverage subscore", "Openness subscore"), 
         macro_sector == "Economic and financial statistics") %>%
  group_by(year, data_categories, element) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = data_categories, group = data_categories)) +
  geom_line() +
  facet_wrap(~element)

#### FIGURE 15: Overall scores of Economic & financial statistics components over time ####
odin_scores %>%
  filter(element %in% c("Overall score"), 
         macro_sector == "Economic and financial statistics") %>%
  group_by(year, data_categories) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(label = case_when(year == max(year) ~ as.character(data_categories), TRUE ~ NA_character_)) %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = data_categories, group = data_categories)) +
  geom_line() +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = label), nudge_x = 0.1, direction = "y", hjust = 0, na.rm = TRUE, size = 3.5, segment.color = "grey") +
  labs(x = "", y = "Average score") +
  theme(legend.position = "none") +
  scale_x_discrete(limits = c("2016", "2017", "2018", "", "2020", ""))
ggsave("Output/Economic subsectors over time.png", dpi = 400)

# What open data elements for economic and financial statistics have performed best? ####
odin_scores %>%
  filter(data_categories == "Economic & financial statistics subscore",
         macro_element %in% c("Coverage elements", "Openness elements")) %>%
  group_by(macro_element, element, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = element, group = element)) +
  geom_line(size = 1.2) + 
  facet_wrap(~macro_element) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 5)) +
  guides(shape = guide_legend(override.aes = list(size = 1))) +
  labs(x = "", y = "Average score", title = "Economic & financial statistics open data elements")

# How have each categories within Economic & financial statistics fared on each open data element
# What open data elements for economic and financial statistics have performed best? ####
econ_fisc <- odin_scores %>%
  filter(macro_sector == "Economic and financial statistics") %>%
  count(data_categories) %>%
  pull(data_categories)

for (i in 1:length(econ_fisc)){
  odin_scores %>%
    filter(data_categories == econ_fisc[i],
           macro_element %in% c("Coverage elements", "Openness elements")) %>%
    group_by(macro_element, element, year) %>%
    summarize(mean_score = mean(score, na.rm = TRUE)) %>%
    ungroup() %>%
    ggplot(aes(x = as.factor(year), y = mean_score, color = element, group = element)) +
    geom_line(size = 1.2) + 
    facet_wrap(~macro_element) +
    theme(legend.position = "bottom", legend.title = element_blank(),
          legend.text = element_text(size = 5)) +
    guides(shape = guide_legend(override.aes = list(size = 1))) +
    labs(x = "", y = "Average score", title = econ_fisc[i])
  ggsave(str_c('C:/Users/lnoe/Documents/R/Graphs/', econ_fisc[i], '.png'))
}

#### FIGURE 16: DO SDDS countries score higher? ####
odin_scores %>%
  left_join(dissemination_standards, by = c("country_code" = "iso3c")) %>%
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
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = dissemination_subscriber, group = dissemination_subscriber)) + 
  geom_line(size = 1.1) + 
  geom_point() +
  labs(x = "", y = "Average score", color = "Subscriber\nstatus", title = "ODIN scores for Economic & financial statistics") +
  scale_x_discrete(limits = c("2016", "2017", "2018", "", "2020")) +
  facet_wrap(~element)
ggsave("Output/IMF Dissemination standards countries and their scores.png", dpi = 400)
# Note that None now includes an odd grouping of countries
# Andorra, Anguilla Cuba, Greenland, Liechtenstein, New Zealand, Somalia
# South Sudan, Taiwan, Turkmenistan
# so scores for places like New Zealand and Taiwan will increase the scores
# Whereas others for South Sudan will decrease them. Overall, None
# category is lower.
# Interesting movement since 2016 for e-GDDS (which was only formalized in 2015)
# so maybe case of e-GDDS countries complying more and more with e-GDDS
# over time
# e-GDDS countries have just about superseded None countries on openness and
# and have rapidly improved on coverage.

#### Open Gender Data Index ####
# Try to replicate OGDI from ODIN 2020 report.

# 1. Average ten data categories with sex-disaggregation or data relevance
# Crime and Justice, Education Outcomes, Food Security & Nutrition, Gender Statistics,
# Health Outcomes, Labor, Population & Vital Statistics, Reproductive Health, Built Environment, Poverty & Income

odin_scores %>%
  mutate(ogdi = case_when(data_categories %in% c("Crime & justice", "Education facilities", "Education outcomes", "Food security & nutrition",
                                                 "Gender statistics", "Health outcomes", "Labor", "Population & vital statistics",
                                                 "Reproductive health", "Built environment", "Poverty & income") ~ "OGDI",
                          data_categories %in% c("All Categories", "Economic & financial statistics subscore", "Environment subscore", "Social statistics subscore") ~ NA_character_,
                          TRUE ~ "non_OGDI")) %>%
  filter(element == "Overall score", !is.na(ogdi)) %>%
  # Average by country by year by OGDI status
  group_by(year, ogdi, country_code) %>%
  summarize(score_by_ogdi = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  # Average by year
  group_by(year, ogdi) %>%
  summarize(overall_ogdi = mean(score_by_ogdi, na.rm = TRUE)) %>%
  ungroup()

# Performance in 2022 of data categories
odin_scores %>%
  mutate(ogdi = case_when(data_categories %in% c("Crime & justice", "Education facilities", "Education outcomes", "Food security & nutrition",
                                                 "Gender statistics", "Health outcomes", "Labor", "Population & vital statistics",
                                                 "Reproductive health", "Built environment", "Poverty & income") ~ "OGDI",
                          data_categories %in% c("All Categories", "Economic & financial statistics subscore", "Environment subscore", "Social statistics subscore") ~ NA_character_,
                          TRUE ~ "non_OGDI")) %>%
  filter(element == "Overall score", ogdi == "OGDI") %>%
  # Average by country by year by OGDI status
  group_by(year, data_categories) %>%
  summarize(score_by_datacat = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(year == 2022)

## OPTIONAL IMPORT COVID-19 DATA
##### COVID-19 data availability from OWID
## Download Our World in Data dataset
#owid_raw <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv", guess_max = 20000)
#
## Create dataset that marks 1 for a variable if a country has ever had 
## data for this variable (most variables are continuous, daily observations)
## and 0 for countries that don't have this information.
#
#owid_data_availability <-
#  # Cases
#  owid_raw %>%
#  filter(!is.na(total_cases), !location %in% c("International", "World")) %>%
#  distinct(location) %>%
#  mutate(case_data = 1) %>%
#  # Deaths
#  left_join(owid_raw %>%
#              filter(!is.na(total_deaths), !location %in% c("International", "World")) %>%
#              distinct(location) %>%
#              mutate(deaths_data = 1)) %>%
#  # ICU Patients
#  left_join(owid_raw %>%
#              filter(!is.na(icu_patients), !location %in% c("International", "World")) %>%
#              distinct(location) %>%
#              mutate(icu_data = 1)) %>%
#  # Hospital Patients
#  left_join(owid_raw %>%
#              filter(!is.na(hosp_patients), !location %in% c("International", "World")) %>%
#              distinct(location) %>%
#              mutate(hosp_data = 1)) %>%
#  # ICU Admissions
#  left_join(owid_raw %>%
#              filter(!is.na(weekly_icu_admissions), !location %in% c("International", "World")) %>%
#              distinct(location) %>%
#              mutate(icu_admit_data = 1)) %>%
#  # Hospital Admissions
#  left_join(owid_raw %>%
#              filter(!is.na(weekly_hosp_admissions), !location %in% c("International", "World")) %>%
#              distinct(location) %>%
#              mutate(hosp_admit_data = 1)) %>%
#  # Tests
#  left_join(owid_raw %>%
#              filter(!is.na(total_tests), !location %in% c("International", "World")) %>%
#              distinct(location) %>%
#              mutate(test_data = 1)) %>%
#  # Vaccinations
#  left_join(owid_raw %>%
#              filter(!is.na(total_vaccinations), !location %in% c("International", "World")) %>%
#              distinct(location) %>%
#              mutate(vacc_data = 1)) %>%
#  # Replace missing counts with 0s and add iso codes
#  mutate(across(case_data:vacc_data, ~ case_when(is.na(.x) ~ 0, TRUE ~ .x)),
#         iso3c = countrycode::countrycode(location, "country.name", "iso3c"),
#         iso3c = case_when(
#           location == "Kosovo" ~ "XKX",
#           location == "Micronesia (country)" ~ "FSM",
#           location == "Timor" ~ "TLS",
#           TRUE ~ iso3c
#         ),
#         # Create binary indicator for whether excess deaths are available
#         # OWID uses https://www.mortality.org/
#         excess_data = case_when(
#           iso3c %in% c("AUT", "AUS", "BEL", "BGR", "CHL", "CAN", "HRV", "CZE", "DNK", "GBR", "EST",
#                        "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "ISR", "ITA", "LVA", "LTU", "LUX",
#                        "NLD", "NZL", "NOR", "POL", "PRT", "KOR", "RUS", "SVN", "SVK", "ESP", "SWE",
#                        "CHE", "TWN", "USA") ~ 1,
#           TRUE ~ 0
#         )) %>%
#  select(iso3c, case_data:excess_data) %>%
#  pivot_longer(case_data:excess_data, names_to = "covid_variable", values_to = "data_available") %>%
#  mutate(covid_variable = case_when(
#    covid_variable == "case_data" ~ "Cases",
#    covid_variable == "deaths_data" ~ "Deaths",
#    covid_variable == "excess_data" ~ "Excess Deaths",
#    covid_variable == "hosp_admit_data" ~ "Hospital Admissions",
#    covid_variable == "hosp_data" ~ "Hospital Patients",
#    covid_variable == "icu_admit_data" ~ "ICU Admissions",
#    covid_variable == "icu_data" ~ "ICU Patients",
#    covid_variable == "test_data" ~ "Total Tests",
#    TRUE ~ "Total vaccinations"
#  ))
#