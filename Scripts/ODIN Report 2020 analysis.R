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

#### COVID-19 data availability from OWID
# Download Our World in Data dataset
owid_raw <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv", guess_max = 20000)

# Create dataset that marks 1 for a variable if a country has ever had 
# data for this variable (most variables are continuous, daily observations)
# and 0 for countries that don't have this information.

owid_data_availability <-
  # Cases
  owid_raw %>%
  filter(!is.na(total_cases), !location %in% c("International", "World")) %>%
  distinct(location) %>%
  mutate(case_data = 1) %>%
  # Deaths
  left_join(owid_raw %>%
              filter(!is.na(total_deaths), !location %in% c("International", "World")) %>%
              distinct(location) %>%
              mutate(deaths_data = 1)) %>%
  # ICU Patients
  left_join(owid_raw %>%
              filter(!is.na(icu_patients), !location %in% c("International", "World")) %>%
              distinct(location) %>%
              mutate(icu_data = 1)) %>%
  # Hospital Patients
  left_join(owid_raw %>%
              filter(!is.na(hosp_patients), !location %in% c("International", "World")) %>%
              distinct(location) %>%
              mutate(hosp_data = 1)) %>%
  # ICU Admissions
  left_join(owid_raw %>%
              filter(!is.na(weekly_icu_admissions), !location %in% c("International", "World")) %>%
              distinct(location) %>%
              mutate(icu_admit_data = 1)) %>%
  # Hospital Admissions
  left_join(owid_raw %>%
              filter(!is.na(weekly_hosp_admissions), !location %in% c("International", "World")) %>%
              distinct(location) %>%
              mutate(hosp_admit_data = 1)) %>%
  # Tests
  left_join(owid_raw %>%
              filter(!is.na(total_tests), !location %in% c("International", "World")) %>%
              distinct(location) %>%
              mutate(test_data = 1)) %>%
  # Vaccinations
  left_join(owid_raw %>%
              filter(!is.na(total_vaccinations), !location %in% c("International", "World")) %>%
              distinct(location) %>%
              mutate(vacc_data = 1)) %>%
  # Replace missing counts with 0s and add iso codes
  mutate(across(case_data:vacc_data, ~ case_when(is.na(.x) ~ 0, TRUE ~ .x)),
         iso3c = countrycode::countrycode(location, "country.name", "iso3c"),
         iso3c = case_when(
           location == "Kosovo" ~ "XKX",
           location == "Micronesia (country)" ~ "FSM",
           location == "Timor" ~ "TLS",
           TRUE ~ iso3c
         ),
         # Create binary indicator for whether excess deaths are available
         # OWID uses https://www.mortality.org/
         excess_data = case_when(
           iso3c %in% c("AUT", "AUS", "BEL", "BGR", "CHL", "CAN", "HRV", "CZE", "DNK", "GBR", "EST",
                        "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "ISR", "ITA", "LVA", "LTU", "LUX",
                        "NLD", "NZL", "NOR", "POL", "PRT", "KOR", "RUS", "SVN", "SVK", "ESP", "SWE",
                        "CHE", "TWN", "USA") ~ 1,
           TRUE ~ 0
         )) %>%
  select(iso3c, case_data:excess_data) %>%
  pivot_longer(case_data:excess_data, names_to = "covid_variable", values_to = "data_available") %>%
  mutate(covid_variable = case_when(
    covid_variable == "case_data" ~ "Cases",
    covid_variable == "deaths_data" ~ "Deaths",
    covid_variable == "excess_data" ~ "Excess Deaths",
    covid_variable == "hosp_admit_data" ~ "Hospital Admissions",
    covid_variable == "hosp_data" ~ "Hospital Patients",
    covid_variable == "icu_admit_data" ~ "ICU Admissions",
    covid_variable == "icu_data" ~ "ICU Patients",
    covid_variable == "test_data" ~ "Total Tests",
    TRUE ~ "Total vaccinations"
  ))

#### IMF Dissemination standards subscriber countries
# From https://dsbb.imf.org/sdds/country
# and https://dsbb.imf.org/sdds-plus/country
# and https://dsbb.imf.org/e-gdds/country
dissemination_standards <- read_csv("Input/imf_dissemination_standards.csv") %>%
  mutate(iso3c = countrycode::countrycode(country, "country.name", "iso3c"),
         iso3c = case_when(
           str_detect(country, "Kosovo") & is.na(iso3c) ~ "XKX",
           str_detect(country, "Macao") & is.na(iso3c) ~ "MAC",
           TRUE ~ iso3c
         )) %>%
  pivot_longer(cols = e_gdds:sdds_plus, names_to = "dissemination_subscriber", values_to = "membership") %>% 
  filter(!is.na(membership)) %>%
  select(iso3c, dissemination_subscriber)

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
  
#### Openness by category, 2018 vs 2020 ####
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
                                "Environment subscore", "Social statistics subscore"),
         element == "Overall score") %>%
  group_by(data_categories, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(year), y = mean_score, color = data_categories, group = data_categories)) +
  geom_line() +
  geom_point() +
  labs(x = "", y = "Average score", color = "") +
  scale_x_discrete(limits = c("2016", "2017", "2018", "", "2020"))
ggsave("Output/Change of rank of sectors over time.png", dpi = 400)

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
  scale_x_discrete(limits = c("2016", "2017", "2018", "", "2020")) +
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
  geom_point() +
  facet_wrap(~macro_sector, scales = "free") +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(15, 85)) +
  scale_x_discrete(limits = c("2016", "2017", "2018", "", "2020")) +
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
  geom_point() +
  facet_wrap(~macro_sector, scales = "free") +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(15, 85)) +
  scale_x_discrete(limits = c("2016", "2017", "2018", "", "2020")) +
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

#### Overall State in 2020 ####
odin_scores %>%
  filter(data_categories %in% c("Health facilities", "Health outcomes", "Population & vital statistics", "Reproductive health", "Food security & nutrition"),
         element %in% c("Coverage subscore", "Openness subscore"), year == 2020) %>%
  group_by(data_categories, element, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(text_label = case_when(data_categories == "Population & vital statistics" ~ element, TRUE ~ NA_character_),
         element = fct_relevel(element, "Openness subscore", "Coverage subscore")) %>%
  ggplot(aes(x = reorder(data_categories, mean_score), y = mean_score, fill = element, color = element)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = text_label), position = position_dodge(0.9), hjust = 0, size = 3.5) +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "", y = "Average score", title = "Health-related indicators 2020") +
  scale_y_continuous(limits = c(0,100))
ggsave("Output/The state of health-related indicators in 2020.png", dpi = 400)

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

#### Examine country reporting of COVID-19 variables against ODIN health variables####
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

#### Overall scores of Economic & financial statistics components over time ####
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

#### DO SDDS countries score higher? ####
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