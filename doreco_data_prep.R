# Rhythm Analysis of DoReCo languages
# Involved: Susanne Fuchs, Ludger Paschen, Lara S. Burchardt
# R Codes from: Lara S. Burchardt 
# Affiliation: Leibniz Center General Linguistics, Leibniz-Zentrum Allgemeine Sprachwissenschaft Berlin

# Script 1 of 2

# data preparation: data input includes ~150.000 ipus from more than 50 languages and
# multiple speakers per language
# we seperate the data into unintertupted instances of speech from one speaker, per language
# we extract onsets and offsets of ipus

###############################################################################

# 00: load packages ----

if (!require(install.load)) {
  install.packages("install.load")
}

library(install.load)

install_load("tidyverse", "stringr")

# 01a: functions ----


# 01b: load data ----

# ipu data

data_ipu <- read_delim("raw_data/ipus_rhythm_meta.csv", delim = ',')

# word data 

data_word <- read_delim("raw_data/DoReCo_1_2_wd_core_merged_meta.csv", delim = ",")

# 02: preparations ----

# 02a: identifier----

languages <- unique(data_ipu$lang)
languages_word <- unique(data_word$lang)

# 02b: filtering ----

## 02b-1 word filtering ----

#Status: alles rausfiltern was nicht "word" ist
#Speaker_ID: unique id für jeden Sprecher, identisch zu "speaker" Spalte in der anderen csv [bitte in dieser Tabelel nicht "speaker" verwenden]
#Speaker_Core: bitte alles rausfiltern was nicht "core" ist

data_word_filtered <- data_word %>% 
  dplyr::filter(Status == "word") %>% 
  dplyr::filter(Genre == "personal narrative" | Genre == "traditional narrative") %>% 
  dplyr::filter(Speaker_Core != "non-core")

# for Rhythm App we need the third column to be "element types", we just set everything to "a" as we are not using 
# this feautre for the DoReCo languages for now

data_word_filtered$element <- "a"

# change filename so, that it is not seperated with _ but with - to make string splitting easier in post processing
# (in post processing we want to split by _ but we don't want the filename to be splittet)

data_word_filtered$file <- str_replace_all(data_word_filtered$file, "_", "-")

# we apply the same transformation to the Speaker name

data_word_filtered$Speaker_ID <- str_replace_all(data_word_filtered$Speaker_ID, "_", "-")


## 02b-2 ipu filtering ----

#all_labelled: remove "yes" - das sind IPUs die ausschließlich aus Füllern, False Starts, Code-Switching, Gesang usw. bestehen
#Genre: remove alles außer "personal narrative" und "traditional narrative" - wir wollen uns erstmal nur Narrative anschauen, oder?
#  Speaker_Core: remove "non-core" - das sind Sprecher wo die Grenzen nicht manuell gesetzt wurden, daher sind die Dauern nicht verlässlich

data_ipu_filtered <- data_ipu %>% 
  dplyr::filter(all_labelled != "yes") %>% 
  dplyr::filter(Genre == "personal narrative" | Genre == "traditional narrative") %>% 
  dplyr::filter(Speaker_Core != "non-core")

# for Rhythm App we need the third column to be "element types", we just set everything to "a" as we are not using 
# this feautre for the DoReCo languages for now

data_ipu_filtered$element <- "a"

# change filename so, that it is not seperated with _ but with - to make string splitting easier in post processing
# (in post processing we want to split by _ but we don't want the filename to be splittet)

data_ipu_filtered$file <- str_replace_all(data_ipu_filtered$file, "_", "-")

# we apply the same transformation to the Speaker name

data_ipu_filtered$speaker <- str_replace_all(data_ipu_filtered$speaker, "_", "-")

# save filtered data table, to access meta data after rhythm analysis

write.table(data_ipu_filtered, file = "unsplit_ipu_data_for_rhythm_analysis_including_meta_data.csv", sep = ",", col.names = TRUE, row.names = FALSE)


# 03: file seperation ----


# 03a: for words ---- 

for (x in 1:length(languages_word)){
  test <-  data_word_filtered %>% 
    filter(lang == languages[x]) %>% 
    group_by(file, Speaker_ID) %>%
    select(start, end, element,Duration, lang, Genre, Speaker_ID, Speaker_Sex, Speaker_Age, file,wd ) %>% 
    group_walk(~ write.table(.x,paste0("language_data_word/word_language_", languages[x], "_file_" ,.y$file, "_speaker_" , .y$Speaker_ID , ".csv"), sep = ";", row.names = FALSE, dec = "."), .keep = TRUE)
}

# 03b: for ipus ----

for (x in 1:length(languages)){
  test <-  data_ipu_filtered %>% 
    filter(lang == languages[x]) %>% 
    group_by(file, speaker) %>%
    select(onset, offset, element, lang, Genre, speaker, Speaker_Sex, Speaker_Age, file) %>% 
    group_walk(~ write.table(.x,paste0("language_data/ipu_language_", languages[x], "_file_" ,.y$file, "_speaker_" , .y$speaker , ".csv"), sep = ";", row.names = FALSE, dec = "."), .keep = TRUE)
}