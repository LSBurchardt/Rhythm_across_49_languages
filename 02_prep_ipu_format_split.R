# Rhythm Analysis of DoReCo languages
# Involved: Susanne Fuchs, Ludger Paschen, Lara S. Burchardt
# R Codes from: Lara S. Burchardt 

# Script  4 of 6

# data preparation: data input includes ~100.000 iois from 49 languages and
# multiple speakers per language
# we separate the data into uninterrupted instances of speech from one speaker, per language
# we extract onsets and offsets of events

###############################################################################

# 00: load packages ----

if (!require(install.load)) {
  install.packages("install.load")
}

library(install.load)

install_load("tidyverse", "stringr")

# 01a: functions ----


# 01b: load data ----


data_ioi <- read_delim("DoReCo_2_0_IO_20240928.csv", delim = ",")

# 02: preparations ----

# 02a: identifier----

languages <- unique(data_ioi$glottocode)

data_ioi$element <- "a"

# change file name so, that it is not separated with _ but with - to make string splitting easier in post processing
# (in post processing we want to split by _ but we don't want the file name to be split)

data_ioi$file <- str_replace_all(data_ioi$file, "_", "-")

# we apply the same transformation to the Speaker name

data_ioi$speaker <- str_replace_all(data_ioi$speaker, "_", "-")

# add column "speech_duration" 

data_ioi$sprach_dauer <-data_ioi$io_duration-data_ioi$pause_duration


# 03: file separation & saving ----

# per file and speaker we need to check for long silent pauses, when a pause is in 99th Quantile (longest 1%), we split
# the file there, we name them part 1, part 2, part 3, ...to include this column as a grouping column in the group_walk
# function below, to automatically save them as separate files

# calculating cut_off
pause_cutoff <- quantile(data_ioi$pause_duration, .99)

# defining function for splitting due to silent breaks

assign_part <- function(df) {
  part_counter <- 1
  parts <- c()
  
  # Initialize the first io_unit
  previous_io_unit <- df$io_unit[1]
  
  for (i in seq_along(df$pause_duration)) {
    pause <- df$pause_duration[i]
    io_unit <- df$io_unit[i]
    
    # Condition for pause duration exceeding cutoff
    if (pause > pause_cutoff) {
      part_counter <- part_counter + 1
    }
    
    # Condition for io_unit jumps larger than 1 (indicating filtering and therefor none consecutive intervals)
    if (i > 1 && io_unit - previous_io_unit > 1) {
      part_counter <- part_counter + 1
    }
    
    # Updating parts vector
    parts <- c(parts, paste0("part", part_counter))
    
    # Updating previous io_unit
    previous_io_unit <- io_unit
  }
  
  # Add the parts column to the dataframe
  df$part <- parts
  return(df)
}

#adding column "part" indicating sequences seperated by silent breaks

data_ioi <- data_ioi %>%
  group_by(file, speaker) %>%
  do(assign_part(.)) %>%
  ungroup()


# we filter out the rows with too long silent breaks, to actually exclude those intervals from the analysis
data_ioi <- data_ioi %>% 
  filter(pause_duration < pause_cutoff)

# save prepared data table in full, to access meta data after rhythm analysis and to analyse io durations, pause durations etc. across dataset

write.table(data_ioi, file = "unsplit_ipu_data_for_rhythm_analysis_including_meta_data_run_Oct24.csv", sep = ",", col.names = TRUE, row.names = FALSE)

# saving individual csv files to be used in RANTO app

for (x in 1:length(languages)){
  test <-  data_ioi %>% 
    filter(glottocode == languages[x]) %>%
    mutate(file = as.factor(file),
           speaker = as.factor(speaker)) %>% 
    group_by(file, speaker, part) %>%
    select(start_time, end_time, element, glottocode, genre, speaker, speaker_sex, speaker_age, file, part) %>% 
    #select(onset, offset, element, lang, Genre, speaker, Speaker_Sex, Speaker_Age, file) %>% 
    group_walk(
      ~ {
        if (nrow(.x) >= 4) {  # Check if subset has at least 3 rows
          write.table(.x, 
                      paste0("language_data/ipu_language_", 
                             languages[x], "_file_", .y$file, 
                             "_speaker_", .y$speaker ,"_", .y$part, ".csv"), 
                      sep = ";", 
                      row.names = FALSE, 
                      dec = ".")
        }
      }, 
      .keep = TRUE)
}

