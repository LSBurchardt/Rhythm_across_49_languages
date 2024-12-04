# Rhythm Analysis of DoReCo languages
# Involved: Susanne Fuchs, Ludger Paschen, Lara S. Burchardt
# R Codes from: Lara S. Burchardt 

# Script  5 of 6

# post processing of data, rhythm analysis results from RANTO are joined back together with meta data
# additional metadata like geographical coordinates and average height per country are added

###############################################################################

# 00: load packages -----

if (!require(install.load)) {
  install.packages("install.load")
}

library(install.load)

install_load("tidyverse", "psych", "tidygeocoder", "countrycode", "devtools")

# 01: load data ----- 

## rhythm results
rhythm_results_doreco_ioi <- read_delim("rhythm_analysis_results/rhythm_analysis_median_io_pause99_split_fs_20.csv", delim = ",")

# meta data

## languguage info, speaker info, including complexity score and tone language information
meta_data_file <- read_delim("unsplit_ipu_data_for_rhythm_analysis_including_meta_data_run_Oct24.csv", delim = ",")

meta_data_file_subset <- meta_data_file %>% 
  select(file, Glottocode = "glottocode" ,speaker, speaker_age, speaker_sex,genre, synthesis, tone)

## additional meta info from doreco (final version?)

meta_languages <- read_delim("doreco_languages_metadata_2_0.csv", delim = ",")

## average height of men for selected countries

# height information from Baten & Blum (2015) dataset
# https://clio-infra.eu/Indicators/Height.html downloaded from here, only column names were
# changed slightly to fit the format 
# Myanmar was renamed to "Myanmar(Burma)" as it was named like that in the geocoding lists
heights <- read_delim("average-height-of-men-for-selected-countries.csv", delim = ";")

# 02: add meta data about language from Doreco to rhythm results-----

for(i in 1:length(rhythm_results_doreco_ioi$filename)){
  names <- str_split(rhythm_results_doreco_ioi$filename[i], pattern = "_")
  names <- names[[1]]
  rhythm_results_doreco_ioi$file[i] <- names[[5]]   #filename from raw file, to use to join rhythm results with meta data file
}

#  meta data like speaker sex and speaker age needs to be extracted/joined from raw data extraction from Database

rhythm_results_doreco_ioi_meta <- left_join(rhythm_results_doreco_ioi, meta_data_file_subset, by = "file", multiple = "any")


rhythm_results_doreco_ioi_meta <- left_join(rhythm_results_doreco_ioi_meta, meta_languages, by = "Glottocode" , multiple = "any")

# 03: add additional meta data ----

# 03a: adding lattitude/longitude ----

for(a in 1:nrow(rhythm_results_doreco_ioi_meta)){
  #  for(a in 1:5){
  lat_longs <- tibble::tribble(
    ~latitude,        ~longitude,
    rhythm_results_doreco_ioi_meta$Latitude[a],           rhythm_results_doreco_ioi_meta$Longitude[a]
  )
  
  reverse <-  lat_longs %>%
    reverse_geocode(lat = latitude, long = longitude, method = 'osm',
                    address = address_found, full_results = TRUE) %>%
    select(country, country_code)
  
  rhythm_results_doreco_ioi_meta$country[a] <- reverse$country
  rhythm_results_doreco_ioi_meta$country_code[a] <- reverse$country_code
  
  country_name_eng <- countrycode(reverse$country_code, "iso2c", "country.name")
  
  rhythm_results_doreco_ioi_meta$country_eng[a] <- country_name_eng
  
  rm(reverse)
  
}

# 03b: adding height information ----

heights <- heights %>% 
  select(country_eng, Year, Height)

heights_avg <- heights %>%
  group_by(country_eng) %>%
  group_by(country_eng) %>%
  summarise(earliest_height = Height[which.min(Year)],  # height of the earliest year
            earliest_year_height = min(Year),           # earliest year of average height measurment
            latest_height = Height[which.max(Year)],    # Get height for the latest year
            latest_year_height = max(Year),             # latest year of average height measurments
            avg_height = mean(Height, na.rm = TRUE),
            max_height = max(Height, na.rm = TRUE),
            min_height = min(Height, na.rm = TRUE))

rhythm_results_doreco_ioi_meta<- left_join(rhythm_results_doreco_ioi_meta, heights_avg, by = "country_eng", multiple = "any")

# 04: save final results table

# deselect some columns, that we do not need for the analysis for better clarity

rhythm_results_doreco_ioi_meta <- rhythm_results_doreco_ioi_meta %>% 
  select(-mean_min_dev_ioi, -mean_min_dev_fft, -fourier_beat, -ugof_fft,
         -silent_beats_fft, -silent_beats_ioi, -npvi_ugof_fft, -cv_ugof_fft,
         -freq_reso, -signal_length, -elements, -raw_element_seq)

saveRDS(rhythm_results_doreco_ioi_meta, file = "rhythm_results_doreco_ioi_meta_complete.rds")
