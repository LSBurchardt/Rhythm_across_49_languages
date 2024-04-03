library(dplyr)
library(tidyverse)
library(stringr)
library(readr)

# Read CSV files from DoReCo 1.3
csv_dir = "..."
setwd("...")
wd_csv_files <- list.files(path = csv_dir, pattern = "\\_wd.csv$", full.names = TRUE, recursive = TRUE)
wd_csv_data <- map_df(wd_csv_files, readr::read_csv)

# Calculate synthesis index based on mean number of morphs per word type
synthesis_data <- wd_csv_data %>%
  select(lang, core_extended, wd, mb, mb_ID, gl) %>%
  filter(!is.na(gl)) %>%
  filter(core_extended == "core") %>%
  filter(wd != "<p:>") %>%
  filter(!str_starts(wd, '<<')) %>%
  distinct(lang, wd, mb, .keep_all = T) %>%
  mutate(morph_count = str_count(mb_ID, "m")) %>%
  group_by(lang) %>%
  mutate(synthesis = round(mean(morph_count), 3)) %>%
  summarise(synthesis = unique(synthesis), .groups = 'drop') %>%
  ungroup()

# Write to a new CSV file
write_csv(synthesis_data, "DoReCo_1_3_core_synthesis.csv")