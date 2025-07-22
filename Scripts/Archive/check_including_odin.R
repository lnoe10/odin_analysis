library(tidyverse)
CRS_2020_data <- read_delim("C:/Users/loren/Downloads/CRS 2020 data/CRS 2020 data.txt", 
                            delim = "|", escape_double = FALSE, trim_ws = TRUE, guess_max = 100000)

odin_scores %>% 
  filter(year == 2022, element %in% c("Overall score", "Coverage subscore", "Openness subscore")) %>% 
  group_by(macro_sector, data_categories, element) %>% 
  summarize(mean_score = mean(score, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(element == "Coverage subscore", !is.na(macro_sector)) %>% 
  ggplot(aes(x = fct_reorder(data_categories, mean_score), y = mean_score, fill = macro_sector)) + 
  geom_col() + 
  coord_flip() + 
  scale_y_continuous(limits = c(0,100))

odin_2022_analysis %>%
  # Restrict dataset to single unique rows
  filter(element = "Overall Score", data_categories == "Environment subscore") %>%
  # Tally how many values you have by a certain column, in this case year
  count(year)

# Total count by year
odin_scores %>%
  # Restrict dataset to single unique rows
  filter(element == "Overall score", data_categories == "Environment subscore") %>%
  # Tally how many values you have by a certain column, in this case year and SIDS
  count(year, sids) %>%
  print(n = 25)

# Overlap
odin_scores %>%
  # Restrict dataset to single unique rows
  filter(element == "Overall score", data_categories == "Environment subscore", sids == "yes", year>=2020) %>%
  # Tally how many observations you have by year for SIDS countries
  count(country_code, name ="num_years") %>%
  # Now that the unit of observation is the country, we can tally the number of countries who have 1 or two observations
  count(num_years)

odin_scores %>%
  # Restrict dataset to single unique rows
  filter(element == "Overall score", data_categories == "Environment subscore", sids == "yes", year>=2020) %>%
  select(country, country_code, score, year) %>%
  pivot_wider(id_cols = country:country_code, names_from = year, names_prefix = "year_", values_from = score) %>%
  filter(!is.na(year_2022), !is.na(year_2020)) %>%
  summarize(avg_2020 = mean(year_2020, na.rm = TRUE), avg_2022 = mean(year_2022, na.rm = TRUE))
  
  

odin_scores %>%
  # Restrict dataset to single unique rows for only SIDS countries in 2016-2022
  filter(element == "Overall score", 
         data_categories == "Environment subscore", year %in% c(2016, 2018, 2020, 2022)) %>%
  group_by(sids, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup()   %>%
  ggplot(aes(x = year, 
             y = mean_score,
             color = sids)) +
  geom_line() +
  scale_x_continuous(breaks = c(2016, 2018, 2020, 2022))+
  scale_y_continuous(limits=c(0, 100)) +
  labs(title = "Average overall score for environmental categories across overlapping SIDS countries", x ="", y = "") + 
  theme(legend.title = element_blank())
print(environment_subscore_overlappingSIDS)
  

un_countries <- read_csv("C:/Users/loren/Downloads/un_countries_mess.csv") %>%
  filter(str_length(un_country) > 1, !str_detect(un_country, "^Date of|^Back to top")) %>%
  mutate(country_code = countrycode::countrycode(un_country, "country.name", "iso3c"),
         country_code = case_when(
           str_detect(un_country, "iye") ~ "TUR",
           TRUE ~ country_code
         )) %>%
  filter(!is.na(country_code))

odin_scores_report %>%
  distinct(country_code, .keep_all = TRUE) %>%
  full_join(un_countries) %>%
  filter(is.na(country)) %>%
  select(un_country)


ogdi_scores <- read_csv("C:/Users/loren/Documents/Work/OGDI Score Testing_062123.csv") %>%
  janitor::clean_names() %>%
  select(year:coverage_subscores) %>%
  mutate(coverage_subscores = as.numeric(str_remove(coverage_subscores, "%")))

# Get rid of multiple scores
ogdi_reduced <- ogdi_scores %>%
  distinct(country, category, indicator, .keep_all = TRUE) %>%
  group_by(country, category) %>%
  summarize(cat_mean = mean(coverage_subscores, na.rm = TRUE)) %>%
  ungroup()

ogdi_reduced %>%
  group_by(country) %>%
  summarize(ogdi_coverage = mean(cat_mean, na.rm = TRUE)) %>%
  ungroup()

ogdi_scores %>%
  distinct(country, category, indicator, .keep_all = TRUE) %>%
  group_by(country, category) %>%
  mutate(cat_mean = mean(coverage_subscores, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(country) %>%
  mutate(ogdi_coverage = mean(cat_mean, na.rm = TRUE)) %>%
  ungroup()

ogdi_scores %>%
  distinct(country, category, indicator, .keep_all = TRUE) %>%
  group_by(country) %>%
  summarize(ogdi_coverage_unweight = mean(coverage_subscores, na.rm = TRUE)) %>%
  ungroup() %>%
  full_join(ogdi_reduced %>%
              group_by(country) %>%
              summarize(ogdi_coverage_cat = mean(cat_mean, na.rm = TRUE)) %>%
              ungroup()) %>%
  write_csv("C:/Users/loren/Documents/Work/OGDI coverage scores comp.csv", na = "")

sample_surveys <- tibble()
for (i in c(6969, 8611, 8395, 6534, 9870, 8637, 8508, 7052, 10026, 7537, 7613, 10011, 7055,
           7520, 11329, 7433, 7596, 11326, 7579, 7170, 7616, 7585, 11247, 10649)){
  survey_df <- as.data.frame(fromJSON(content(GET(str_c("https://catalog.ihsn.org/index.php/api/catalog/", i, "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc)
  sample_surveys <- sample_surveys %>%
    bind_rows(survey_df)
}

odin_map_old %>%
  filter(year == 2022, element == "Overall score", data_categories == "All Categories") %>%
  select(country_code, old_score = score) %>%
  full_join(odin_scores_report %>% 
              filter(year == 2022, element == "Overall score", data_categories == "All Categories") %>%
              select(country, country_code, new_score = score)) %>%
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


odin_aug8 <- read_csv("C:/Users/loren/Downloads/odin_2022_Aug8.csv", na = "") %>%
  # convert all variable names to snakecase
  janitor::clean_names() %>%
  # Take out missing observations at bottom of the table, it's just metadata on file generation
  filter(!is.na(region_code)) %>%
  # Rename variables to bring in line with older data
  # rename(data_categories = elements, coverage_subscore = coverage_subscores, openness_subscore = openness_subscores, overall_score = overall_subscores) %>%
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
  country = case_when(country_code == "TUR" ~ "Turkey", TRUE ~ country)) %>%
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
  rename(score_aug = score)

odin_mar7 <- read_csv("C:/Users/loren/Downloads/ODIN_scores_2022_7Mar.csv", na = "") %>%
  # convert all variable names to snakecase
  janitor::clean_names() %>%
  # Take out missing observations at bottom of the table, it's just metadata on file generation
  filter(!is.na(region_code)) %>%
  # Rename variables to bring in line with older data
  # rename(data_categories = elements, coverage_subscore = coverage_subscores, openness_subscore = openness_subscores, overall_score = overall_subscores) %>%
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
  country = case_when(country_code == "TUR" ~ "Turkey", TRUE ~ country)) %>%
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
  rename(score_mar = score)

combined <- odin_aug8 %>%
  full_join(odin_mar7) %>%
  group_by(data_categories, element) %>%
  mutate(global_rank_mar = rank(-score_mar, ties.method = "min"),
         global_rank_aug = rank(-score_aug, ties.method = "min")) %>%
  ungroup() %>%
  group_by(data_categories, element, region) %>%
  mutate(region_rank_mar = rank(-score_mar, ties.method = "min"),
         region_rank_aug = rank(-score_aug, ties.method = "min")) %>%
  ungroup() %>%
  mutate(global_rank_diff = global_rank_mar - global_rank_aug,
         region_rank_diff = region_rank_mar - region_rank_aug)

odin_aug9 <- read_csv("C:/Users/loren/Downloads/odin_2022_Aug9.csv", na = "") %>%
  # convert all variable names to snakecase
  janitor::clean_names() %>%
  # Take out missing observations at bottom of the table, it's just metadata on file generation
  filter(!is.na(region_code)) %>%
  # Rename variables to bring in line with older data
  # rename(data_categories = elements, coverage_subscore = coverage_subscores, openness_subscore = openness_subscores, overall_score = overall_subscores) %>%
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
  country = case_when(country_code == "TUR" ~ "Turkey", TRUE ~ country)) %>%
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
  rename(score_9aug = score)
