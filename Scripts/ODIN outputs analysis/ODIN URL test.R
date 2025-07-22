library(httr)
library(tidyverse)

setwd("C:/Users/loren/Documents/R work/")

nso_df <- readxl::read_excel("Input Data/ODIN 2024-25 Countries_test.xlsx", sheet = 3, skip = 2) |>
  janitor::clean_names()

nso_list <- nso_df |>
  pull(nso_website)

tryCatch(GET("http://nsia.gov.af")$status_code, error=function(e){})

nso_status <- data.frame(country = as.character(), nso_address = as.character(), site_status = as.character())

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = length(nso_list), # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

Sys.time()
for (i in 1:length(nso_list)){
  website_status <- tryCatch(GET(nso_list[i])$status_code, error=function(e){})
  if (is.null(website_status) == T){
    nso_status <- nso_status |>
      add_row(country = nso_df$country[i], nso_address = nso_list[i], site_status = NA_character_)
  } else{
    nso_status <- nso_status |>
      add_row(country = nso_df$country[i], nso_address = nso_list[i], site_status = as.character(website_status))
  }
  Sys.sleep(sample(1:5, 1))
  setTxtProgressBar(pb, i)
}
Sys.time()