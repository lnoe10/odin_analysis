library(tidyverse)

odin_2020 <- read_csv("C:/Users/loren/Documents/GitHub/odin_analysis/Input/ODIN_scores_2020.csv") %>%
  # convert all variable names to snakecase
  janitor::clean_names() %>%
  # Take out missing observations at bottom of the table, it's just metadata on copyright
  filter(!is.na(year), str_count(year) <= 4)

odin_2020_countries <- odin_2020 %>%
  distinct(country_code) %>%
  mutate(odin_country = TRUE)

# UN Member countries
un_member_countries <- read_csv("C:/Users/loren/Downloads/un_member_countries.csv") %>%
  mutate(country_code = countrycode::countrycode(country, "country.name", "iso3c"))

all_countries <- read_csv("C:/Users/loren/Downloads/un_country_list.csv") %>%
  janitor::clean_names() %>%
  mutate(m49 = TRUE) %>%
  select(all_country = country_or_area, country_code = iso_alpha3_code, m49)

all_countries %>%
  full_join(un_member_countries) %>%
  mutate(country_code = case_when(country_code == "AND" ~ "ADO", TRUE ~ country_code)) %>%
  full_join(odin_2020_countries)
# Extra countries are Kosovo, Hong Kong, Taiwan, Macao

# Tabulate UN member countries versus ODIN countries
all_countries %>%
  full_join(un_member_countries) %>%
  mutate(country_code = case_when(country_code == "AND" ~ "ADO", TRUE ~ country_code)) %>%
  full_join(odin_2020_countries) %>%
  count(odin_country, un_member)

# All countries that are not UN members but are ODIN countries
all_countries %>%
  full_join(un_member_countries) %>%
  mutate(country_code = case_when(country_code == "AND" ~ "ADO", TRUE ~ country_code)) %>%
  full_join(odin_2020_countries) %>%
  filter(odin_country == TRUE, is.na(un_member))

# 180 ODIN countries are UN member countries, 7 aren't, of which four (Kosovo, Hong Kong, Macao, and Taiwan) are not
# listed by UN, and three (Anguilla, Greenland, State of Palestine) are listed in UN, but aren't UN members

all_countries %>%
  full_join(un_member_countries) %>%
  mutate(country_code = case_when(country_code == "AND" ~ "ADO", TRUE ~ country_code)) %>%
  full_join(odin_2020_countries) %>%
  filter(is.na(odin_country), un_member == TRUE) %>%
  arrange(all_country)

# 1 Barbados                              BRB          TRUE  Barbados     TRUE      NA          
# 2 Central African Republic              CAF          TRUE  Central Afr~ TRUE      NA          
# 3 Comoros                               COM          TRUE  Comoros      TRUE      NA          
# 4 Democratic People's Republic of Korea PRK          TRUE  Democratic ~ TRUE      NA          
#  5 Eritrea                               ERI          TRUE  Eritrea      TRUE      NA          
#  6 Grenada                               GRD          TRUE  Grenada      TRUE      NA          
#  7 Kiribati                              KIR          TRUE  Kiribati     TRUE      NA          
#  8 Monaco                                MCO          TRUE  Monaco       TRUE      NA          
#  9 Nauru                                 NRU          TRUE  Nauru        TRUE      NA          
# 10 Sudan                                 SDN          TRUE  Sudan        TRUE      NA          
# 11 Tuvalu                                TUV          TRUE  Tuvalu       TRUE      NA          
# 12 Vanuatu                               VUT          TRUE  Vanuatu      TRUE      NA          
# 13 Venezuela (Bolivarian Republic of)    VEN          TRUE  Venezuela, ~ TRUE      NA          