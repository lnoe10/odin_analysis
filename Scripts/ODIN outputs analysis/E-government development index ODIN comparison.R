library(tidyverse)

# https://publicadministration.un.org/egovkb/en-us/Data-Center
#  # Read in E-government development index indicators 2022
egov_all <- read_csv("C:/Users/loren/Downloads/EGOV_DATA_2022.csv") %>%
  pivot_longer(`E-Government Rank`:`Telecommunication Infrastructure Index`, names_to = "Egov Indicator", values_to = "Egov Score") %>%
  janitor::clean_names() %>%
  mutate(iso3c = countrycode::countrycode(country_name, "country.name", "iso3c"),
         iso3c = case_when(
           str_detect(country_name, "iye") ~ "TUR",
           TRUE ~ iso3c
         )) %>%
  filter(egov_indicator != "E-Government Rank") %>%
  # Read in E-government development index Open Government Data Index pilot study 2022
  # From 2022 Annex Table A.13
  bind_rows(read_csv("C:/Users/loren/Downloads/EGOV_OGDI.csv") %>%
              mutate(country_name = str_trim(str_remove(country, "Low|Very High|Middle|High")),
                     iso3c = countrycode::countrycode(country_name, "country.name", "iso3c"),
                     iso3c = case_when(
                       str_detect(country_name, "iye") ~ "TUR",
                       TRUE ~ iso3c
                     ),
                     egov_indicator = "Open Government Data Index",
                     survey_year = 2022) %>%
              select(survey_year, country_name, iso3c, egov_indicator, egov_score = ogdi))

odin_scores_report %>%
  filter(year == 2022, data_categories == "All Categories", str_detect(element, "score")) %>%
  left_join(egov_all %>% select(-c(country_name, survey_year)), by = c("country_code" = "iso3c"), multiple = "all") %>%
  select(country, country_code, element, score, egov_indicator, egov_score) %>%
  group_by(element, egov_indicator) %>%
  summarize(cor = cor(score, egov_score, use = "pairwise.complete.obs"))


# 187 countries shared