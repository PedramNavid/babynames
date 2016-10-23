library(jsonlite)
library(dplyr)
library(ggplot2)
library(stringr)
library(ggthemes)

# Globals ---- 

API_KEY = readLines("apikey.txt", warn = F) # Namara.io API key
DATA_SET_ID_F  = "5149f141-b653-4e91-86f9-14e7f33e7830"
DATA_SET_ID_M = "b0eeaeb2-bf54-4de6-8a0f-d2fda674284b"

URL_M = paste0("http://api.namara.io/v0/data_sets/", 
             DATA_SET_ID_M, "/data/en-4?api_key=", API_KEY)

URL_F = paste0("http://api.namara.io/v0/data_sets/", 
               DATA_SET_ID_F, "/data/en-4?api_key=", API_KEY)

# Cache dataset
RDS = "rds/baby_names.rds" 

# Functions ----
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

# Import data ----

if(!file.exists(RDS)) {
  male_names <- get_api_data(URL_M)
  female_names <- get_api_data(URL_F)
  save(male_names, female_names, file = RDS)
} else load(RDS)



# Build a function to get year / frequency / risk

add_freq <- function(df) {
  df %>% 
    group_by(year) %>% 
    mutate(percents = frequency / sum(frequency)) %>% 
    arrange(name, year) %>% 
    group_by(name) %>% 
    mutate(rel_risk = percents / lag(percents, order_by = year),
           rel_loss = 1 - round(rel_risk,2) * 100) %>% 
    mutate(ends_1 = str_sub(name, -1, -1),
           ends_2 = str_sub(name, -2, -1),
           ends_3 = str_sub(name, -3, -1))
}

mn <- add_freq(male_names)
fn <- add_freq(female_names)
mn$sex = "male"
fn$sex = "female"
an <- rbind(mn, fn)

# Create table of ending letters
last_letter <- an %>% 
  group_by(sex, year, ends_1) %>% 
  summarise(freq = sum(frequency), n = n()) %>% 
  group_by(sex, year) %>% 
  mutate(percents = freq / sum(freq))

color = case_when(
  last_letter$ends_1 == "E" ~ "E",
  last_letter$ends_1 == "A" ~ "A",
  last_letter$ends_1 == "N" ~ "N",
  TRUE ~ "Other")

last_letter$color = color

# Plots
theme_set(theme_few(base_size = 11))

ggplot(last_letter, aes(year, percents, color = color, group = ends_1)) + 
  geom_line() +
  facet_wrap(~ sex) +
  scale_color_manual(values = c("#F8766D", "#7CAE00", "#00BFC4", "#BBBBBB")) +
  scale_y_continuous(labels=percent) +
  labs(y = "Occurence of Name", color = "Last Letter in Name", title = "Last Letter Trends for Males and Female Ontario-Born Babies") +
  theme(legend.position = "bottom", 
        panel.grid = element_line(),
        panel.grid.major.y = element_line(color = 'grey70'),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())


