library(dplyr)

### This script combines and reformats metadata retrieved from DoReCo 2.0 ###
### This allows the metadata to be easily merged with the core DoReCo data in 01_prep_doreco_data.R ###

# Read individual metadata csv files from DoReCo 2.0
setwd("...")
directory_path <- "..."
file_paths <- list.files(path = directory_path, pattern = "*.csv", full.names = TRUE)

# Names of columns containing info on multiple speakers
columns_to_split <- c("spk_code", "spk_age", "spk_age_c", "spk_sex")

# Define function to split a column based on "/" separator
split_column <- function(df, column_name) {
  # Create new column names
  a_col <- paste0(column_name, "_a")
  b_col <- paste0(column_name, "_b")
  
  # Split the column based on "/"
  df <- df %>%
    mutate(!!a_col := ifelse(grepl("/", !!sym(column_name)),
                             sub("/.*", "", !!sym(column_name)),
                             !!sym(column_name)),
           !!b_col := ifelse(grepl("/", !!sym(column_name)),
                             sub(".*/", "", !!sym(column_name)),
                             NA))
  
  # Cleaning
  df <- df %>% select(-!!sym(column_name))
  
  return(df)
}

# Function to extract glottocode
extract_glottocode <- function(file_path) {
  # Extract file name from full file path
  file_name <- basename(file_path)
  
  # Split the file name by "_" and return the middle part
  parts <- unlist(strsplit(file_name, "_"))
  if (length(parts) >= 3) {
    return(parts[2])
  } else {
    return(NA)
  }
}

# Function to read and process multiple CSVs
process_csv_files <- function(file_paths, columns_to_split) {
  # Initialize an empty list to store processed data frames
  dfs <- list()
  
  # Loop over each file, process it, and store the result in the list
  for (file_path in file_paths) {
    # Read the CSV as all character columns
    df <- read.csv(file_path, stringsAsFactors = FALSE, colClasses = "character")
    
    # Extract the glottocode from the file name and add as a new column
    glottocode <- extract_glottocode(file_path)
    df$lang <- glottocode
    
    # Split specified columns
    for (col in columns_to_split) {
      df <- split_column(df, col)
    }
    
    # Add the processed data frame to the list
    dfs[[length(dfs) + 1]] <- df
  }
  
  # Combine all data frames into one using bind_rows
  combined_df <- bind_rows(dfs)
  
  return(combined_df)
}

# Main step: Batch processing
processed_df <- process_csv_files(file_paths, columns_to_split)

# Cleaning: remove non-core files and rename column
final_df <- processed_df %>%
  filter(extended == "no") %>%
  mutate(file = paste0("doreco_", lang, "_", name)) %>%
  select(-name,-id)

# Save new csv file
output_file <- "DoReCo_2_0_core_metadata_merged.csv"
write.csv(final_df, file = output_file, row.names = FALSE)
