# Life Expectancy

## Get Life Table

``` r
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
```

    ## Warning: 4440 parsing failures.
    ##  row col               expected actual                            file
    ## 8104  mx no trailing characters      . 'fltper_1x1/BEL.fltper_1x1.txt'
    ## 8104  qx no trailing characters      . 'fltper_1x1/BEL.fltper_1x1.txt'
    ## 8104  ax no trailing characters      . 'fltper_1x1/BEL.fltper_1x1.txt'
    ## 8104  lx no trailing characters      . 'fltper_1x1/BEL.fltper_1x1.txt'
    ## 8104  dx no trailing characters      . 'fltper_1x1/BEL.fltper_1x1.txt'
    ## .... ... ...................... ...... ...............................
    ## See problems(...) for more details.

    ## Warning: 4440 parsing failures.
    ##  row col               expected actual                            file
    ## 8104  mx no trailing characters      . 'mltper_1x1/BEL.mltper_1x1.txt'
    ## 8104  qx no trailing characters      . 'mltper_1x1/BEL.mltper_1x1.txt'
    ## 8104  ax no trailing characters      . 'mltper_1x1/BEL.mltper_1x1.txt'
    ## 8104  lx no trailing characters      . 'mltper_1x1/BEL.mltper_1x1.txt'
    ## 8104  dx no trailing characters      . 'mltper_1x1/BEL.mltper_1x1.txt'
    ## .... ... ...................... ...... ...............................
    ## See problems(...) for more details.

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `Age = as.numeric(Age)`.
    ## Caused by warning:
    ## ! NAs introduced by coercion

``` r
life_tables %>%
  glimpse
```

    ## Rows: 828,960
    ## Columns: 12
    ## $ Year    [3m[38;5;246m<dbl>[39m[23m 1921, 1921, 1921, 1921, 1921, 1921, 1921, 1921, 1921, 1921, 19…
    ## $ Age     [3m[38;5;246m<dbl>[39m[23m 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, …
    ## $ mx      [3m[38;5;246m<dbl>[39m[23m 0.05999, 0.01206, 0.00578, 0.00289, 0.00325, 0.00252, 0.00248,…
    ## $ qx      [3m[38;5;246m<dbl>[39m[23m 0.05750, 0.01199, 0.00576, 0.00288, 0.00325, 0.00251, 0.00248,…
    ## $ ax      [3m[38;5;246m<dbl>[39m[23m 0.28, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.…
    ## $ lx      [3m[38;5;246m<dbl>[39m[23m 100000, 94250, 93120, 92583, 92316, 92016, 91785, 91557, 91391…
    ## $ dx      [3m[38;5;246m<dbl>[39m[23m 5750, 1130, 537, 267, 300, 231, 228, 166, 126, 125, 114, 105, …
    ## $ Lx      [3m[38;5;246m<dbl>[39m[23m 95857, 93685, 92851, 92450, 92166, 91900, 91671, 91474, 91328,…
    ## $ Tx      [3m[38;5;246m<dbl>[39m[23m 6317561, 6221704, 6128020, 6035168, 5942719, 5850553, 5758652,…
    ## $ ex      [3m[38;5;246m<dbl>[39m[23m 63.18, 66.01, 65.81, 65.19, 64.37, 63.58, 62.74, 61.90, 61.01,…
    ## $ Country [3m[38;5;246m<chr>[39m[23m "Australia", "Australia", "Australia", "Australia", "Australia…
    ## $ Gender  [3m[38;5;246m<chr>[39m[23m "female", "female", "female", "female", "female", "female", "f…
