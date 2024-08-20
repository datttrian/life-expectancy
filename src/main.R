library(tidyverse)
library(rvest)
library(glue)

webpage <- "https://www.mortality.org/" %>%
  read_html %>%
  html_nodes(xpath = "/html/body/div[1]/div/div[3]/ul/li")

countries <- webpage %>%
  html_text(trim = TRUE)

codes <- webpage %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  substring(23)

get_life_table <- function(country) {
  data <- codes[which(countries == country)] %>%
    map(~ {
      map_df(c("female", "male"), function(gender) {
        path_prefix <-
          ifelse(gender == "female", "fltper_1x1", "mltper_1x1")
        file_path <- glue("{path_prefix}/{.}.{path_prefix}.txt")
        read_table(file_path, skip = 2) %>%
          mutate(Country = country, Gender = gender)
      })
    }) %>%
    bind_rows
}

life_tables <- map(countries, get_life_table) %>%
  bind_rows %>%
  mutate(Age = as.numeric(Age)) %>%
  filter_all(all_vars(!is.na(.)))

life_tables %>% glimpse
