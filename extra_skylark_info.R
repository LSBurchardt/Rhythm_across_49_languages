# information skylarks from repository

library(tidyverse)

data <- read_delim("Burchardt_Briefer_Knoernschild_2021_skylarkSong_timepoints.csv", delim = ",")
data_sub <- data %>% 
  select(id = "ID", ioi = "IOI[sec]")

mean(data$`IOI[sec]`, na.rm = TRUE)


data_summary <- data %>% 
  group_by(id) %>% 
 summarize(ioi_beat = 1/median(ioi, na.rm = TRUE),
           ioi_cv = sd(ioi, na.rm = TRUE)/mean(ioi, na.rm = TRUE))
           
           
mean(data_summary$ioi_beat)

mean(data_summary$ioi_cv)
