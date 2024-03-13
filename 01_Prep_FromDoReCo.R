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

# Read metadata CSV and merge dataframes
metadata <- read_csv("csvs/metadata_merged_1_3.csv")
metadata$file <- paste("doreco_",metadata$glottocode,"_",metadata$filename,sep="")
df_merged <- merge(wd_csv_data,metadata,by="file")

# Remove non-core speakers, i.e. data without proper time alignment for words and pauses
df_clean <- subset(df_merged, (speaker %in% c(spk_code_a, spk_code_b)))

# Remove conversational and stimulus-based data
df_clean <- subset(df_clean, !(genre %in% c("conversation", "stimulus retelling")))

# Identify IO units, calculate duration, check for labels, assign corpus-wide running integer
io_data <- df_clean %>%
  group_by(lang, file, speaker) %>%
  mutate(is_sequence_start = (ph != "<p:>" & lag(ph, default = first(ph)) == "<p:>") | row_number() == 1,
         io_unit = cumsum(is_sequence_start)) %>%
  group_by(lang, file, speaker, io_unit) %>%
  filter(!(ph == "<p:>" & io_unit == 1)) %>%
  summarise(start_time = first(start),
            end_time = last(end),
            duration = sum(duration),
            only_labels = !(any(!str_starts(wd[wd != "<p:>"], '<<'))),
            .groups = 'drop') %>%
  group_by(lang, file, speaker) %>%
  filter(io_unit != max(io_unit)) %>%
  ungroup() %>%
  mutate(io_unit = row_number())  
  
# Write to a new CSV file
write_csv(io_data, "DoReCo_1_3_IO.csv") # This creates a new CSV file with the processed data
