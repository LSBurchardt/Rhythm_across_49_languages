library(dplyr)
library(tidyverse)
library(stringr)
library(readr)

# Read CSV files from DoReCo 1.3
csv_dir = ".../csvs"
setwd("...")
wd_csv_files <- list.files(path = csv_dir, pattern = "\\_wd.csv$", full.names = TRUE, recursive = TRUE)
wd_csv_data <- map_df(wd_csv_files, readr::read_csv)

# Corpus-wide unique speaker codes
wd_csv_data$speaker <- paste(wd_csv_data$lang,"_",wd_csv_data$speaker,sep="")

# Calculate duration of individual words
wd_csv_data <- wd_csv_data %>% mutate(duration = round(end - start, 3))

# Identify IO units, calculate duration, check for labels, assign corpus-wide running integer
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

# Read speaker-level metadata CSV and merge dataframes
metadata <- read.csv("csvs/metadata_merged_1_3.csv", na.strings = "na")
metadata$file <- paste("doreco_",metadata$lang,"_",metadata$filename,sep="")
io_merged <- merge(io_data,metadata,by="file")

# Remove non-core speakers, i.e. data without proper time alignment for words and pauses
io_clean <- subset(io_merged, (speaker %in% c(spk_code_a, spk_code_b)))

# Remove conversational and stimulus-based data
io_clean <- subset(io_clean, !(genre %in% c("conversation", "stimulus retelling")))
io_clean <- subset(io_clean, is.na(spk_code_b))

# Cleaning up
io_clean <- subset(io_clean, select = -c(filename,rec_date,rec_date_c,genre_stim,extended,gloss,transl,sound_quality,background_noise,word_tokens,spk_code_a,spk_age_c_a,spk_code_b,spk_age_b,spk_age_c_b,spk_sex_b))
lookup <- c(speaker_sex = "spk_sex_a", speaker_age = "spk_age_a")
io_final <- rename(io_clean, all_of(lookup))  

# Write to a new CSV file
write_csv(io_final, "DoReCo_1_3_IO.csv")
