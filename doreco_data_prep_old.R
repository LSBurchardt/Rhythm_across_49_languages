#######################################
# DORECO Rhythm Analysis Project
# R Code for data preparation and analysis
# author: Lara S. Burchardt
# collaborators: Ludgar Paschen, Frank Seifert, Susanne Fuchs, Wim Pouw
#
######################################

## 00a: load packages ------

library(tidyverse)

## 00b: load data ------

data_all_ipus <- read_delim("output raw/output_2.csv")
data_all_words <- read_delim("output_2_words.csv")
languages <- unique(data_all_words$Language)
## 01: group data by language, speakerID and sequence

#example code with mtcars and cyl from: https://stackoverflow.com/questions/41233173/how-can-i-write-dplyr-groups-to-separate-files
#mtcars %>%
#  group_by(cyl) %>%
#  group_walk(~ write_csv(.x, paste0(.y$cyl, "test.csv")))

# grouped by filename and speaker ID, as in conversations it is one filename but severeal speakers
# need to check as two files and as one, to see, how it differs within a conversation, I think
# the breaks between speakers where easily seen on the recurrence plots

# for ipus
for (x in 1:length(languages)){
 test <-  data_all %>% 
  filter(Language == languages[x]) %>% 
  group_by(Filename, Speaker_ID) %>%
  select(IPU_Onset, IPU_NumWords, IPU_Duration, Language, Genre, Speaker_ID, Speaker_Sex, Speaker_Age, Filename) %>% 
  #relocate(IPU_Onset, IPU_NumWords, IPU_Duration, Language, Genre, Speaker_ID, Speaker_Sex, Speaker_Age) %>% 
  group_walk(~ write.table(.x,paste0(languages[x],.y$Filename,"_",.y$Speaker_ID, ".csv"), sep = ";", row.names = FALSE, dec = "."), .keep = TRUE)
}

# for word level

for (x in 1:length(languages)){
  word_level <-  data_all_words %>% 
    filter(Language == languages[x]) %>% 
    group_by(Filename, Speaker_ID) %>%
    #data.frame() %>% 
    select(Word_Onset, Filename, Speaker_ID) %>% 
    #select(Word_Onset, IPU_NumWords, IPU_Duration, Language, Genre, Speaker_ID, Speaker_Sex, Speaker_Age, Filename) %>% 
    #relocate(IPU_Onset, IPU_NumWords, IPU_Duration, Language, Genre, Speaker_ID, Speaker_Sex, Speaker_Age) %>% 
    group_walk(~ write.table(.x,paste0(languages[x],.y$Filename,"_",.y$Speaker_ID, "_wordlevel.csv"), sep = ";", row.names = FALSE,col.names = FALSE, dec = "."), .keep = TRUE)
}



#original with manual language nomination  
#data_all %>% 
#  filter(Language == "Bora") %>% 
#  group_by(Filename, Speaker_ID) %>% 
#  relocate(IPU_Onset, IPU_NumWords, IPU_Duration, Language, Genre, Speaker_ID, Speaker_Sex, Speaker_Age) %>% 
#  group_walk(~ write_csv(.x, paste0(.y$Filename,"_",.y$Speaker_ID, ".csv")))

