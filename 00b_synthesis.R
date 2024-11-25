library(purrr)
library(dplyr)
library(stringr)
library(readr)

### This script calculates synthesis scores for all core DoReCo datasets with morphological annotations ###
### Synthesis scores are defined as the mean number of morphs (roots, affixes, clitics) per unique word form ###
### Disfluencies, pauses, and units adjacent to word-internal pauses are excluded from the calculations ###

# Read word-level CSV files from DoReCo 2.0
csv_dir = "..."
setwd("...")
wd_csv_files <- list.files(path = csv_dir, pattern = "\\_wd.csv$", full.names = TRUE, recursive = TRUE)
wd_csv_data <- map_df(wd_csv_files, readr::read_csv)

# Calculate synthesis index based on mean number of morphs per word type
synthesis_data <- wd_csv_data %>%
  select(lang, core_extended, wd, mb, mb_ID, gl) %>%
  filter(!is.na(gl)) %>%
  filter(core_extended == "core") %>%
  mutate(next_to_wip = ifelse(
    lead(wd) == "<<wip>>" | lag(wd) == "<<wip>>", "yes", "no")) %>%
  filter(next_to_wip == "no") %>%
  filter(wd != "<p:>") %>%
  filter(!str_starts(wd, '<<')) %>%
  distinct(lang, wd, mb, .keep_all = T) %>%
  mutate(morph_count = str_count(mb_ID, "m")) %>%
  group_by(lang) %>%
  mutate(synthesis = round(mean(morph_count), 3)) %>%
  summarise(synthesis = unique(synthesis), .groups = 'drop')

# Write to a new CSV file
write_csv(synthesis_data, "DoReCo_2_0_core_synthesis.csv")
