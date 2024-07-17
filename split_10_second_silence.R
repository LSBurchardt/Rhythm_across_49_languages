# how many silent breaks do we have that are longer than xx seconds?
# Threshold: 3x mean of whole dataset, approximately 10 seconds, for now we use 10 seconds

library(tidyverse)

# Initialize an empty dataframe to store the filenames
files_with_large_differences <- data.frame(filename = character(), stringsAsFactors = FALSE)
counter <- 0
# Path to the directory containing the CSV files
directory <- "language_data/runapr24/"

# List all CSV files in the directory
csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)

# Loop through each CSV file
for (file in csv_files) {
  # Read the CSV file
  data <- read_delim(file, delim = ";")
  
  # Calculate differences between adjacent rows in the first column
  differences <- diff(data$start_time)
  
  # Check if any difference is >= 10
  if (any(differences >= 30)) {
    # If so, save the filename
    counter_30 <- counter + 1
    files_with_large_differences <- rbind(files_with_large_differences, data.frame(filename = file, stringsAsFactors = FALSE))
  }
}

# Output the dataframe containing filenames with large differences
print(files_with_large_differences)
