##Rhythm Analysis of DoReCo languages
# Involved: Susanne Fuchs, Ludger Paschen, Lara S. Burchardt
# R Codes from: Lara S. Burchardt 
# Affiliation: Leibniz Center General Linguistics, Leibniz-Zentrum Allgemeine Sprachwissenschaft Berlin

# Script 2 of 2

# analysis: rhythms were analyzed with RANTO (https://github.com/LSBurchardt/R_app_rhythm/tree/master/RhythmAnalysis)

###############################################################################

# 00: load packages ----

if (!require(install.load)) {
  install.packages("install.load")
}

library(install.load)

install_load("tidyverse", "psych")

# 01a: functions ----


# 01b: load data ----

# meta data languages

languages_meta <- read_delim("doreco_languages_metadata_v1.2.csv", delim = ",")

# meta data files

meta_data_file <- read_delim("unsplit_ipu_data_for_rhythm_analysis_including_meta_data.csv", delim = ",")

# rhythm analysis was run in 6 chunks, we load all 6 result tables to comine them

chunk_1 <- read_delim("language_data/rhythm_results/rhythm_analysis_doreco_chunk_1_fs_10.csv", delim = ",")
chunk_2 <- read_delim("language_data/rhythm_results/rhythm_analysis_doreco_chunk_2_fs_10.csv", delim = ",")
chunk_3 <- read_delim("language_data/rhythm_results/rhythm_analysis_doreco_chunk_3_fs_10.csv", delim = ",")
chunk_4 <- read_delim("language_data/rhythm_results/rhythm_analysis_doreco_chunk_4_fs_10.csv", delim = ",")
chunk_5 <- read_delim("language_data/rhythm_results/rhythm_analysis_doreco_chunk_5_fs_10.csv", delim = ",")
chunk_6 <- read_delim("language_data/rhythm_results/rhythm_analysis_doreco_chunk_6_fs_10.csv", delim = ",")

# 02: combining data ----

# 02a: combining rhythm results ----

rhythm_results_doreco <- data.frame()
rhythm_results_doreco <- rbind(rhythm_results_doreco, chunk_1)
rhythm_results_doreco <- rbind(rhythm_results_doreco, chunk_2)
rhythm_results_doreco <- rbind(rhythm_results_doreco, chunk_3)
rhythm_results_doreco <- rbind(rhythm_results_doreco, chunk_4)
rhythm_results_doreco <- rbind(rhythm_results_doreco, chunk_5)
rhythm_results_doreco <- rbind(rhythm_results_doreco, chunk_6)

# 02b: adding meta data ----

# meta data like language and speaker is extracted from the filename

for(i in 1:length(rhythm_results_doreco$filename)){
  names <- str_split(rhythm_results_doreco$filename[i], pattern = "_")
  names <- names[[1]]
  rhythm_results_doreco$language[i] <- names[[3]]
  rhythm_results_doreco$Glottocode[i] <- names[[3]]
  rhythm_results_doreco$speaker[i] <- names[[7]]
  rhythm_results_doreco$speaker[i] <- substring(rhythm_results_doreco$speaker[i], 1, nchar(rhythm_results_doreco$speaker[i])-4)
  rhythm_results_doreco$type[i] <- names[[1]]
  rhythm_results_doreco$file[i] <- names[[5]]   #filename from raw file, to use to join rhythm results with meta data file
}

#  meta data like speaker sex and speaker age needs to be extracted/joined from raw data extraction from Database



meta_data_file <- meta_data_file %>% 
  select(file, Speaker_Age, Speaker_Sex)

rhythm_results_doreco_meta <- left_join(rhythm_results_doreco, meta_data_file, by = "file", multiple = "any")

rhythm_results_doreco_meta <- left_join(rhythm_results_doreco_meta, languages_meta, by = "Glottocode", multiple = "any")
#03: analysis & visualizations ----

## rhythm & age----

### ipu ----
cor_age <- corr.test(rhythm_results_doreco_meta$ioi_beat, rhythm_results_doreco_meta$Speaker_Age, use = "pairwise.complete.obs")
# r = -0.01, p = 0.8

plot_scatter_rhythm_age_ipu <- rhythm_results_doreco_meta %>% 
  ggplot(aes(x= Speaker_Age, y = ioi_beat, fill = language, color = language))+
  geom_jitter()+
  theme_minimal()

# test one language for within language dependency, using URUM, as there are many datapoints there

rhythm_urum <- rhythm_results_doreco_meta %>% 
  dplyr::filter(language == "urum1249")

cor_age_urum <- corr.test(rhythm_urum$ioi_beat, rhythm_urum$Speaker_Age, use = "pairwise.complete.obs")
# r = 0.18, p = 0.04

plot_scatter_rhythm_age_ipu_urum <- rhythm_urum %>% 
  ggplot(aes(x= Speaker_Age, y = ioi_beat ))+
  geom_jitter(aes( fill = Speaker_Sex, color = Speaker_Sex))+
  geom_smooth(method = "lm")+
  theme_minimal()

## rhythm and sex ----

### ipu  -----
rhythm_men <- rhythm_results_doreco_meta %>% 
  dplyr::filter( Speaker_Sex == "m")

rhythm_women <- rhythm_results_doreco_meta %>% 
  dplyr::filter(Speaker_Sex == "f")

t.test(rhythm_men$ioi_beat, rhythm_women$ioi_beat)


# idea: lets model, what has the biggest impact on ipu rhythm: sex, age, language, language class?


# 04: visualization ----

# or visualization we order the languages by median ioi-beat
group_ordered <- with(rhythm_results_doreco,                       # Order boxes by median
                      reorder(language,
                              ioi_beat,
                              median))

rhythm_results_doreco_ordered <- rhythm_results_doreco                              # Create data with reordered group levels
rhythm_results_doreco_ordered$language <- factor(rhythm_results_doreco_ordered$language,
                             levels = levels(group_ordered))

rhythm_results_doreco_meta %>%
  group_by(Family) %>% 
  ggplot(aes(y = ioi_beat, x = Family, fill = Family))+
  geom_boxplot()+
  geom_jitter(alpha = 0.2)+
  theme_minimal()

rhythm_results_doreco_ordered %>%
  group_by(language) %>% 
  ggplot(aes(y = ioi_beat, x = language, fill = language))+
  geom_boxplot()+
  geom_jitter(alpha = 0.2)+
  theme_minimal()

rhythm_results_doreco_ordered %>%
  group_by(language) %>% 
  ggplot(aes(y = unbiased_cv, x = language, fill = language))+
  geom_boxplot()+
  theme_minimal()

rhythm_results_doreco_ordered %>%
  group_by(language) %>% 
  ggplot(aes(y = ugof_ioi, x = language, fill = language))+
  geom_boxplot()+
  geom_jitter(alpha = 0.2)+
  theme_minimal()

rhythm_results_doreco_ordered %>%
  ggplot(aes(x = ioi_beat))+
  geom_histogram(alpha = 0.4)+
  theme_minimal()

rhythm_results_doreco_meta %>% 
  ggplot(aes(y = ioi_beat))+
  geom_boxplot(aes(group = Speaker_Sex))+
  theme_minimal()
