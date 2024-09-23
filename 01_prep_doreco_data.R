library(tidyverse)
library(dplyr)

### This script reads the word-level core datasets from DoReCo 1.3 and reformats them such that one row corresponds to one inter-onset-interval (IOI) ###
### It also merges the metadata extracted in 00a_metadata.R and the synthesis scores created in 00b_synthesis.R, as well as the manually coded info on tone languages in DoReCo_1_3_tone.csv ###

# Read word-level CSV files from DoReCo 1.3
csv_dir = "..."
setwd("...")
wd_csv_files <- list.files(path = csv_dir, pattern = "\\_wd.csv$", full.names = TRUE, recursive = TRUE)
wd_csv_data <- map_df(wd_csv_files, readr::read_csv)

# Set corpus-wide unique speaker codes
wd_csv_data$speaker <- paste(wd_csv_data$lang,"_",wd_csv_data$speaker,sep="")

# Calculate duration of individual words and pauses
wd_csv_data <- wd_csv_data %>% mutate(duration = round(end - start, 3))

# Identify IO units, calculate duration, check for labels, assign corpus-wide running integer
# Automatically discards non-core datasets as only core datasets contain ph values
io_data <- wd_csv_data %>%
  mutate(across(.cols = everything(), ~ifelse(. == "<<wip>>", "<p:>", .))) %>%
  group_by(file, speaker) %>%
  mutate(is_sequence_start = (ph != "<p:>" & lag(ph, default = first(ph)) == "<p:>") | row_number() == 1,
         io_unit = cumsum(is_sequence_start)) %>%
  group_by(file, speaker, io_unit) %>%
  filter(!(ph == "<p:>" & io_unit == 1)) %>%
  summarise(start_time = first(start),
            end_time = last(end),
            pause_duration = last(duration),
            io_duration = round(sum(duration), 3),
            only_labels = !(any(!str_starts(wd[wd != "<p:>"], '<<'))),
            .groups = 'drop') %>%
  group_by(file, speaker) %>%
  filter(io_unit != max(io_unit)) %>%
  ungroup() %>%
  mutate(io_unit = row_number())

# Read file-level metadata CSV, set corpus-wide unique speaker codes, and merge dataframes
metadata <- read.csv("DoReCo_1_3_core_metadata_merged.csv")
metadata$spk_code_a <- ifelse(
  is.na(metadata$spk_code_a),
  NA,
  paste0(metadata$lang, "_", metadata$spk_code_a)
)
metadata$spk_code_b <- ifelse(
  is.na(metadata$spk_code_b),
  NA,
  paste0(metadata$lang, "_", metadata$spk_code_b)
)
io_merged <- merge(io_data, metadata, by="file")

# Remove problematic datapoints
io_clean <- io_merged %>%
  # Exclude conversational and stimulus-based genres
  filter(!(genre %in% c("conversation", "stimulus retelling"))) %>%
  # Another method to identify conversational data
  filter(is.na(spk_code_b)) %>%
  # Exclude IPUs consisting solely of disfluencies, code-switching or other labelled content
  filter(only_labels == FALSE)

# Merge synthesis data
synth_data <- read.csv("DoReCo_1_3_core_synthesis.csv")
io_synth <- merge(io_clean, synth_data, by="lang", all.x = TRUE)

# Merge tone data
tone_data <- read.csv("DoReCo_1_3_tone.csv")
io_tone <- merge(io_synth, tone_data, by="lang", all.x = TRUE)

# Cleaning up
io_final <- io_tone %>%
  rename(glottocode = lang,
         speaker_sex = spk_sex_a,
         speaker_age = spk_age_a) %>%
  arrange(io_unit) %>%
  select(file,speaker,io_unit,start_time,end_time,pause_duration,io_duration,genre,glottocode,speaker_age,speaker_sex,synthesis,tone)

# Write to a new CSV file
write_csv(io_final, "DoReCo_1_3_IO_20240923.csv")
