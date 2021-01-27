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
  mutate(second_administrative_level = as.numeric(str_remove(second_administrative_level, "-"))) %>%
  # Add 2017 scores
  bind_rows(read_csv("Input/ODIN_scores_2017.csv") %>%
              # convert all variable names to snakecase
              janitor::clean_names() %>%
              # Take out missing observations at bottom of the table, it's just metadata on copyright
              filter(!is.na(year))) %>%
  # Add 2016 scores
  bind_rows(read_csv("Input/ODIN_scores_2016.csv") %>%
              # convert all variable names to snakecase
              janitor::clean_names() %>%
              # Take out missing observations at bottom of the table, it's just metadata on copyright
              filter(!is.na(year))) %>%
  # Clean variables and data categories to be able to compare 2020 to 2018
  mutate(data_categories = str_remove_all(data_categories, "\\n$|\\s$"),
         data_categories = case_when(
           data_categories == "Energy use" ~ "Energy",
           data_categories == "Land use" ~ "Agriculture & Land Use",
           TRUE ~ data_categories
         )) %>%
  # Convert to long
  pivot_longer(indicator_coverage_and_disaggregation:overall_score, names_to = "element", values_to = "score") %>%
  # Convert elements back to sentence case for easier reading
  mutate(element = str_to_sentence(str_replace_all(element, "_", " ")),
         # Create macro sector group for use in graphing/analysis
         macro_sector = case_when(
           data_categories %in% c("Population & vital statistics", "Education facilities",
                                  "Education outcomes", "Health facilities", "Health outcomes",
                                  "Reproductive health", "Food security & nutrition", "Gender statistics",
                                  "Crime & justice", "Poverty & income")  ~ "Social statistics",
           data_categories %in% c("National accounts", "Labor", "Price indexes", "Government finance",
                                  "Money & banking", "International trade", "Balance of payments") ~ "Economic and financial statistics",
           data_categories %in% c("Agriculture & Land Use", "Resource use", "Energy", "Pollution", "Built environment") ~ "Environmental statistics",
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
         ))

#### GENERAL ANALYSIS ####
# Start with shorter general discussion of scores by categories for 
# 2020 and trends since 2016. Are certain coverage or openness lacking 
# in certain categories more than others? Can do something similar to 
# previous year’s report. (Lorenz) 

#### Replicate 2020 and 2018 part of Figure 2 from ODIN 2020/2021 Executive Summary ####
odin_scores %>% 
  filter(data_categories == "All Categories", 
         element %in% c("Overall score", "Coverage subscore", "Openness subscore")) %>% 
  group_by(element, year) %>% 
  summarize(median_score = median(score, na.rm = TRUE)) %>%
  ungroup()

#### Coverage by category, 2018 vs 2020 ####
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
         !data_categories %in% c("Food security & nutrition", "Economic & financial statistics subscore",
                                 "All Categories", "Environment subscore", "Social statistics subscore"),
         year %in% c(2018, 2020)) %>%
  # Make average by data category and year
  group_by(macro_sector, data_categories, year) %>%
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
        legend.title = element_blank(), legend.position = c(0.95, 0.75)) +
  # Labels
  labs(x = "", y = "Average score", title = "Coverage, ODIN 2018 and 2020") +
  # Extend y axis from 0 to 100
  scale_y_continuous(limits = c(0, 100)) + 
  facet_wrap(~macro_sector, scales = "free")
  
#### Openness by category, 2018 vs 2020 ####
# Make sorting order based on average coverage scores in 2018
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
  # Filter for only coverage subscore, filter out subscores that don't
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
         !data_categories %in% c("Food security & nutrition", "Economic & financial statistics subscore",
                                 "All Categories", "Environment subscore", "Social statistics subscore"),
         year %in% c(2018, 2020)) %>%
  # Make average by data category and year
  group_by(macro_sector, data_categories, year) %>%
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
        legend.title = element_blank(), legend.position = c(0.95, 0.75)) +
  # Labels
  labs(x = "", y = "Average score", title = "Openness, ODIN 2018 and 2020") +
  # Extend y axis from 0 to 100
  scale_y_continuous(limits = c(0, 100)) + 
  facet_wrap(~macro_sector, scales = "free")

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

#### Graph of how coverage of category subscores have increased ####
odin_scores %>%
  filter(data_categories %in% c("Economic & financial statistics subscore",
                                "Environment subscore", "Social statistics subscore", "All Categories"),
         element == "Overall score") %>%
  group_by(data_categories, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = data_categories, group = data_categories)) +
  geom_line()

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
  facet_wrap(~macro_sector, scales = "free") +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(15, 85)) +
  labs(x = "", y = "Average overall score")

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
  facet_wrap(~macro_sector, scales = "free") +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(15, 85)) +
  labs(x = "", y = "Average coverage score")

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
  facet_wrap(~macro_sector, scales = "free") +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(15, 85)) +
  labs(x = "", y = "Average openness score")

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
  ggrepel::geom_text_repel(aes(label = label), nudge_x = 0.1, direction = "y", hjust = 0, na.rm = TRUE, size = 3.5, segment.alpha = 0) +
  labs(x = "", y = "Average score") +
  theme(legend.position = "none") +
  scale_x_discrete(limits = c("2016", "2017", "2018", "2020", "2021", "2022"))

#### HEALTH ####
# Health section- Focus on scores from the 3 health categories, 
# identify trends or interesting findings from 2020. Bring in Global 
# Health 50/50 COVID data or other COVID related data (Lorenz) 

#### Overall Trends ####
odin_scores %>%
  filter(data_categories %in% c("Health facilities", "Health outcomes", "Population & vital statistics"),
         element %in% c("Overall score", "Coverage subscore", "Openness subscore")) %>%
  group_by(data_categories, element, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = data_categories, group = data_categories)) +
  geom_line(size = 1.2) + 
  facet_wrap(~element) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(x = "", y = "Average score")

#### Examine coverage and openness elements by health categories ####
# Health Facilities
odin_scores %>%
  filter(data_categories %in% c("Health facilities", "Health outcomes", "Population & vital statistics"),
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
  filter(data_categories %in% c("Health facilities", "Health outcomes", "Population & vital statistics"),
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
  filter(data_categories %in% c("Health facilities", "Health outcomes", "Population & vital statistics"),
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

#### Health category change over time by region ####
odin_scores %>%
  filter(element %in% c("Overall score", "Coverage subscore", "Openness subscore"),
         data_categories %in% c("Health facilities", "Health outcomes", "Population & vital statistics")) %>%
  group_by(macro_region, region, element, data_categories, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(element == "Overall score", data_categories == "Population & vital statistics") %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = region, group = region)) +
  geom_line() + 
  facet_wrap(~macro_region) +
  labs(x = "", y = "Average overall score", title = "Population & vital statistics")

#### ECONOMIC ####
# Economic section- Focus on gaps in economic and financial statistics. 
# What categories are lacking? Are there certain coverage or openness 
# elements that are lower across categories? Do SDDS, SDDS+ countries 
# score higher? (Lorenz) 