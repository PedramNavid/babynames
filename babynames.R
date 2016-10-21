library(jsonlite)
library(dplyr)
library(ggplot2)
library(stringr)

DATA_SET_ID_F  = "5149f141-b653-4e91-86f9-14e7f33e7830"
DATA_SET_ID_M = "b0eeaeb2-bf54-4de6-8a0f-d2fda674284b"

API_KEY = readLines("apikey.txt", warn = F)

URL_M = paste0("http://api.namara.io/v0/data_sets/", 
             DATA_SET_ID_M, 
             "/data/en-4?api_key=",
             API_KEY)

URL_F = paste0("http://api.namara.io/v0/data_sets/", 
               DATA_SET_ID_F, 
               "/data/en-4?api_key=",
               API_KEY)

# Get the data from Namara, 
get_api_data <- function(URL, max = 1e06, verbose = TRUE) {
  cat("\nGrabbing data from Namara: ")
  df <- data.frame()
  for (i in seq(0,250*1e06, by = 250)) {
    offset_url = paste0(URL, "&offset=", i)
    tmp <- fromJSON(readLines(offset_url, warn = F))
    if (verbose & i %% 1000 == 0) cat(".")
    if (length(tmp) == 0) break
    df <- rbind(df, tmp)
  }
  df
}

male_names <- get_api_data(URL_M)
female_names <- get_api_data(URL_F)


# Build a function to get year / frequency / risk

add_freq <- function(df) {
  df %>% 
    group_by(year) %>% 
    mutate(percents = frequency / sum(frequency)) %>% 
    arrange(name, year) %>% 
    group_by(name) %>% 
    mutate(rel_risk = percents / lag(percents, order_by = year),
           rel_loss = 1 - rel_risk)
}

add_feature <- function(.data, .dots = lazyeval::lazy_dots(...)) {
  x <- filter(.data, .dots) %>% 
    group_by(year, 
  
}

mn <- add_freq(male_names) %>% 
  mutate(has_y = str_detect(name, "Y"),
         ends_1 = str_sub(name, -1, -1),
         ends_2 = str_sub(name, -2, -1),
         ends_3 = str_sub(name, -3, -1))

ggplot(data=filter(mn, ends_3 == "IAN"), aes(year, frequency, color = '-IAN')) +
  stat_summary(fun.y = sum, geom = "line") +
  stat_summary(data=filter(mn, has_y), fun.y = sum, geom = "line", aes(color = 'Y')) +
  stat_summary(data=filter(mn, ends_3 == "SON"), fun.y = sum, geom = "line", aes(color = '-SON'))
  
  

ggplot(data = mn, aes(year, frequency)) +
  stat_summary(data=filter(mn, ends_1 == "N"), fun.y = sum, geom = "line", aes(color = 'N'))

