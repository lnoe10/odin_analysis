library(tidyverse)

odin_2020 <- read_csv("C:/Users/loren/Documents/odin_scores_2020.csv") %>%
  mutate(across(`Population & vital statistics`:`Overall score`, as.numeric)) %>%
  pivot_longer(`Population & vital statistics`:`Overall score`, names_to = "data_categories", values_to = "score") %>%
  select(country = Country, iso3c = `Country Code`, region = Region, region_code = `Region code`, elements = `Data categories`, data_categories, score) %>%
  filter(!is.na(elements)) %>%
  mutate(elements = str_remove(elements, "\n"),
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
           elements %in% c("Indicator coverage and disaggregation", "Data available last 5 years",
                          "Data available last 10 years", "First administrative level", "Second administrative level") ~ "Coverage elements",
           elements %in% c("Non proprietary", "Metadata available", "Machine readable", "Download options", "Terms of use") ~ "Openness elements",
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

# Average score by data category
odin_2020 %>%
  filter(elements %in% c("Coverage subscore", "Openness subscore", "All Categories"), !is.na(macro_sector)) %>%
  group_by(elements, macro_sector, data_categories) %>%
  summarize(mean_cat_score = mean(score, na.rm = TRUE)) %>%
  arrange(elements, desc(mean_cat_score)) %>%
  print(n = 66)

# Average score by data category subscore
odin_2020 %>%
  filter(elements %in% c("Coverage subscore", "Openness subscore", "All Categories"), is.na(macro_sector)) %>%
  group_by(elements, data_categories) %>%
  summarize(mean_cat_score = mean(score, na.rm = TRUE)) %>%
  arrange(elements, desc(mean_cat_score)) %>%
  print(n = 12)

# To get energy data across time, import all ODIN from https://github.com/lnoe10/odin_analysis and do the following
odin_scores %>%
  filter(data_categories %in% c("Economic & financial statistics subscore",
                                "Environment subscore", "Social statistics subscore"),
         element == "Openness subscore") %>%
  group_by(data_categories, year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>% 
  pivot_wider(names_from = year, values_from = mean_score) %>% 
  writexl::write_xlsx("C:/Users/loren/Downloads/ODIN 2020 Openness scores.xlsx")