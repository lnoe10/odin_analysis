library(tidyverse)

setwd("C:/Users/loren/Documents/R work")

ratings <- readxl::read_excel("Input Data/SP_ratings_April_2025.xlsx", sheet = 1, skip = 4) |>
  janitor::clean_names() |>
  mutate(iso3c = countrycode::countrycode(country_code, "iso2c", "iso3c"),
         sov_rating_major = case_when(
           str_count(lt_fc_rating, "A") == 3 ~ "AAA",
           str_count(lt_fc_rating, "A") == 2 ~ "AA",
           str_count(lt_fc_rating, "A") == 1 ~ "A",
           str_count(lt_fc_rating, "B") == 3 ~ "BBB",
           str_count(lt_fc_rating, "B") == 2 ~ "BB",
           str_count(lt_fc_rating, "B") == 1 ~ "B",
           str_detect(lt_fc_rating, "C") ~ "CCC",
           TRUE ~ "SD"
         )) |>
  full_join(readxl::read_excel("Output Data/ODIN scores 2016-2024 April 2.xlsx") |> 
              filter(year == 2024, data_category == "Economic & financial statistics subscore", element == "Openness subscore") |>
              select(iso3c = country_code, score)) |>
  mutate(
    lt_fc_rating = case_when(
      is.na(lt_fc_rating) ~ "None",
      TRUE ~ lt_fc_rating
    ),
    sov_rating_major = case_when(
      is.na(sov_rating_major) ~ "None",
      TRUE ~ sov_rating_major
    ),
    lt_fc_rating = as.factor(lt_fc_rating),
    sov_rating_major = as.factor(sov_rating_major),
    sov_rating_major = fct_relevel(sov_rating_major, "None", "SD", "CCC", "B", "BB", "BBB", "A", "AA", "AAA"))
  

ratings |> 
  ggplot(aes(x = sov_rating_major)) + 
  geom_histogram(stat = "count")

ratings |>
  group_by(sov_rating_major) |>
  summarize(avg_score = mean(score, na.rm = TRUE)) |>
  ungroup() |>
  ggplot(aes(x = sov_rating_major, y = avg_score)) +
  geom_point()

ratings |>
  ggplot(aes(x = sov_rating_major, y = score)) +
  geom_boxplot()

avg_scores <- ratings |>
  group_by(sov_rating_major) |>
  summarize(score = mean(score, na.rm = TRUE)) |>
  ungroup()

ratings |>
  ggplot(aes(x = sov_rating_major, y = score, color = sov_rating_major)) +
  geom_jitter() +
  geom_point(data = avg_scores, aes(x = sov_rating_major, y = score), color = "black", alpha = 0.5, size = 2) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "S&P Sovereign Credit Rating", y = "ODIN 2024/2025 Economic & Financial Statistics Coverage score") +
  theme_bw() +
  theme(legend.position = "off")
ggsave("Graphs/SP Coverage econ scores jitter 2024.png", dpi = 400)

ratings |>
  ggplot(aes(x = sov_rating_major, y = score, color = sov_rating_major)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "S&P Sovereign Credit Rating", y = "ODIN 2024/2025 Economic & Financial Statistics Coverage score") +
  theme_bw() +
  theme(legend.position = "off", axis.title.y = element_text(size = 8))
ggsave("Graphs/SP Coverage econ scores boxplot 2024.png", dpi = 400)

ratings |>
  group_by(sov_rating_major) |>
  summarize(avg_score = mean(score, na.rm = TRUE), median_score = median(score, na.rm = TRUE)) |>
  ungroup()