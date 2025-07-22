library(tidyverse)

odin_2022_countries <- read_csv("C:/Users/loren/Documents/odin_2022.csv") %>%
  mutate(iso3c = countrycode::countrycode(country, "country.name", "iso3c"),
         iso3c = case_when(
           country == "Kosovo" ~ "XKX",
           TRUE ~ iso3c),
         year = 2022L) %>%
  select(-country)

odin_2020_countries <- read_csv("C:/Users/loren/Documents/GitHub/odin_analysis/Input/ODIN_scores_2020.csv") %>%
  # convert all variable names to snakecase
  janitor::clean_names() %>%
  # Take out missing observations at bottom of the table, it's just metadata on copyright
  filter(!is.na(year), str_count(year) <= 4) %>%
  distinct(country_code, year) %>%
  mutate(year = as.numeric(year),
         country_code = case_when(
           country_code == "ADO" ~ "AND",
           TRUE ~ country_code
         ))

odin_countries <- odin_2022_countries %>%
  bind_rows(odin_2020_countries %>% rename(iso3c = country_code))

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
  # https://thedocs.worldbank.org/en/doc/bb52765f38156924d682486726f422d4-0090082021/original/FCSList-FY22.pdf
  fragile_states = case_when(
    code %in% c("AFG", "SOM", "SYR", "YEM", "ARM", "AZE", "BFA", "BDI", "CMR", "CAF", "TCD", "COD", "ETH", "HTI", "IRQ", "LBY",
                "MLI", "MOZ", "MMR", "NER", "NGA", "SSD", "COG", "ERI", "GNB", "XKX", "LBN", "PNG", "SDN", "VEN", "PSE", "ZWE",
                "COM", "KIR", "MHL", "FSM", "SLB", "TLS", "TUV") ~ "Fragile",
    TRUE ~ "Not fragile"
  ))

# For LDC list
m49 <- read_csv("C:/Users/loren/Documents/unsd_classifications.csv") %>%
  janitor::clean_names() %>%
  select(iso3c = iso_alpha3_code, ldc = least_developed_countries_ldc) %>%
  mutate(ldc = case_when(
    ldc == "x" ~ "Least Developed Country",
    TRUE ~ "Not LDC"
  ))

# SIDS list https://www.un.org/ohrlls/content/list-sids
sids <- read_csv("C:/Users/loren/Documents/sids_list.csv") %>%
  janitor::clean_names() %>%
  mutate(country = str_remove(country, "^[0-9]*\\. "),
         iso3c = countrycode::countrycode(country, "country.name", "iso3c"),
         sids_country = "Small Island Developing States") %>%
  select(-country)

all_markers <- odin_countries %>%
  left_join(wb_countries, by = c("iso3c" = "code")) %>%
  left_join(m49) %>%
  left_join(sids) %>%
  mutate(sids_country = case_when(
    is.na(sids_country) ~ "Not SIDS",
    TRUE ~ sids_country
  ))

# countries by lending category
all_markers %>%
  count(year, lending_category)

# countries by income group
all_markers %>%
  count(year, income_group)

# countries by Fragile States
all_markers %>%
  count(year, fragile_states)

# countries by Least Developed Country status
all_markers %>%
  count(year, ldc)

# countries by SIDS
all_markers %>%
  count(year, sids_country)