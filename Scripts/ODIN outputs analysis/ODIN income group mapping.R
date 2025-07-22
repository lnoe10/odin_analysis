library(tidyverse)
library(tidymodels)
library(wbstats)

# ODIN scores
# Read in and clean data
odin_scores <- read_csv("C:/Users/loren/Documents/GitHub/odin_analysis/Input/ODIN_scores_2020.csv") %>%
  # convert all variable names to snakecase
  janitor::clean_names() %>%
  # Take out missing observations at bottom of the table, it's just metadata on copyright
  filter(!is.na(year), str_count(year) <= 4) %>%
  # Add 2018 scores
  bind_rows(read_csv("C:/Users/loren/Documents/GitHub/odin_analysis/Input/ODIN_scores_2018.csv") %>%
              # convert all variable names to snakecase
              janitor::clean_names() %>%
              # Take out missing observations at bottom of the table, it's just metadata on copyright
              filter(!is.na(year), str_count(year) <= 4)) %>%
  mutate(second_administrative_level = as.numeric(str_remove(second_administrative_level, "-"))) %>%
  # Add 2017 scores
  bind_rows(read_csv("C:/Users/loren/Documents/GitHub/odin_analysis/Input/ODIN_scores_2017.csv") %>%
              # convert all variable names to snakecase
              janitor::clean_names() %>%
              # Take out missing observations at bottom of the table, it's just metadata on copyright
              filter(!is.na(year), str_count(year) <= 4)) %>%
  # Add 2016 scores
  bind_rows(read_csv("C:/Users/loren/Documents/GitHub/odin_analysis/Input/ODIN_scores_2016.csv") %>%
              # convert all variable names to snakecase
              janitor::clean_names() %>%
              # Take out missing observations at bottom of the table, it's just metadata on copyright
              filter(!is.na(year), str_count(year) <= 4)) %>%
  # Clean variables and data categories to be able to compare 2020 to 2018
  mutate(data_categories = str_remove_all(data_categories, "\\n$|\\s$"),
         data_categories = case_when(
           data_categories == "Energy use" ~ "Energy",
           data_categories == "Land use" ~ "Agriculture & Land Use",
           TRUE ~ data_categories
         ),
         year = as.numeric(year)) %>%
  # Convert to long
  pivot_longer(indicator_coverage_and_disaggregation:overall_score, names_to = "element", values_to = "score") %>%
  # Convert elements back to sentence case for easier reading
  mutate(element = str_to_sentence(str_replace_all(element, "_", " ")),
         data_categories = str_remove(data_categories, "\r"),
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
         ))

# World Bank income groups
# Import list of regions and income groups from World Bank FY2022 classifications
# https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups
wb_countries <- read_csv("C:/Users/loren/Downloads/wb_countries_fy22.csv") %>%
  janitor::clean_names() %>%
  filter(!is.na(region)) %>%
  # Venezuela is not classified in FY2022. Manually assign to last year's income classification "UM"
  mutate(income_group = case_when(
    str_detect(economy, "Venezuela") ~ "Upper middle income",
    TRUE ~ income_group
  ),
  income_group = fct_relevel(income_group, "Low income", "Lower middle income", "Upper middle income", "High income"),
  code = case_when(
    economy == "Andorra" ~ "ADO",
    TRUE ~ code
  ))

# World Bank GNI per capita
gni <- wb_data(c("gni_pc" = "NY.GNP.PCAP.CD"), mrnev = 1)

# Merge all together
df <- odin_scores %>%
  filter(year == 2020) %>%
  left_join(wb_countries %>% select(code, region, income_group, lending_category), by = c("country_code" = "code")) %>%
  left_join(gni %>% select(iso3c, year = date, gni_pc), by = c("country_code" = "iso3c"))

### Map correlation ####

# Overall score
df %>%
  filter(data_categories == "All Categories", element == "Overall score", !is.na(income_group)) %>%
  ggplot(aes(x = gni_pc, y = score, color = income_group)) +
  geom_point() +
  scale_x_log10(labels = comma) +
  stat_smooth(method="lm", formula=y~1, se=FALSE) +
  scale_y_continuous(limits = c(0,100)) +
  labs(x = "GNI per capita (logged)", y = "ODIN Overall Score", color = "WB FY2022\nIncome Groups")

# Coverage score
df %>%
  filter(data_categories == "All Categories", element == "Coverage subscore", !is.na(income_group)) %>%
  ggplot(aes(x = gni_pc, y = score, color = income_group)) +
  geom_point() +
  scale_x_log10(labels = comma) +
  stat_smooth(method="lm", formula=y~1, se=FALSE) +
  scale_y_continuous(limits = c(0,100)) +
  labs(x = "GNI per capita (logged)", y = "ODIN Coverage Score", color = "WB FY2022\nIncome Groups")

# Openness score
df %>%
  filter(data_categories == "All Categories", element == "Openness subscore", !is.na(income_group)) %>%
  ggplot(aes(x = gni_pc, y = score, color = income_group)) +
  geom_point() +
  scale_x_log10(labels = comma) +
  stat_smooth(method="lm", formula=y~1, se=FALSE) +
  scale_y_continuous(limits = c(0,100)) +
  labs(x = "GNI per capita (logged)", y = "ODIN Openness Score", color = "WB FY2022\nIncome Groups")

# Boxplots for overall scores
df %>%
  filter(data_categories == "All Categories", element == "Overall score", !is.na(income_group)) %>%
  ggplot(aes(income_group, score)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2) +
  scale_y_continuous(limits = c(0, 100))

# Correlation for indicator coverage and disaggregation
df %>%
  filter(data_categories == "All Categories", element == "Indicator coverage and disaggregation", !is.na(income_group)) %>%
  ggplot(aes(x = gni_pc, y = score, color = income_group)) +
  geom_point() +
  scale_x_log10() +
  stat_smooth(method="lm", formula=y~1, se=FALSE)

# Boxplots for indicator coverage and disaggregation
df %>%
  filter(data_categories == "All Categories", element == "Indicator coverage and disaggregation", !is.na(income_group)) %>%
  ggplot(aes(income_group, score)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2) +
  scale_y_continuous(limits = c(0, 100))

#### SPI correlation ####
spi_info <- wb_data(
  indicator = c("spi_overall" = "IQ.SPI.OVRL",
                "spi_1" = "IQ.SPI.PIL1",
                "spi_2" = "IQ.SPI.PIL2",
                "spi_3" = "IQ.SPI.PIL3",
                "spi_4" = "IQ.SPI.PIL4",
                "spi_5" = "IQ.SPI.PIL5"), start_date = 2016, end_date = 2019
) %>%
  mutate(iso3c = case_when(country == "Andorra" ~ "ADO",
                           TRUE ~ iso3c))

df_full <- odin_scores %>%
  left_join(wb_countries %>% select(code, region, income_group, lending_category), by = c("country_code" = "code"))

df %>%
  filter(data_categories == "All Categories", element %in% c("Overall score", "Coverage subscore", "Openness subscore")) %>%
  pivot_wider(id_cols = c(country_code, income_group, region.y), names_from = element, values_from = score) %>%
  rename(odin_overall = "Overall score", odin_coverage = "Coverage subscore", odin_openness = "Openness subscore") %>%
  full_join(spi_info %>% select(iso3c, country, spi_overall:spi_5), by = c("country_code" = "iso3c")) %>%
  select(odin_coverage:odin_overall, spi_overall:spi_5) %>%
  corrr::correlate() %>%
  corrr::fashion()

# Correlate overall with all years
# Correlate by income group
# Import ODIN elements

# Does SPI 3 improperly weigh lower income countries as having good values because they have more development data
