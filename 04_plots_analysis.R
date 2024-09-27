# Rhythm Analysis of DoReCo languages
# Involved: Susanne Fuchs, Ludger Paschen, Lara S. Burchardt
# R Codes from: Lara S. Burchardt 
# Affiliation: Leibniz Center General Linguistics, Leibniz-Zentrum Allgemeine Sprachwissenschaft Berlin

# Script  6 of 6

# Plots and Analyses for rhythm analysis

###LP: Globally: Replace IPU by IOI (?)
###LP: Rename repository from "DoReCo" to something more specific like "IOI beat across languages"

###############################################################################

# 00: preparations -----

## 00a: load packages -----

if (!require(install.load)) {
  install.packages("install.load")
}

library(install.load)

install_load("tidyverse", "psych", "tidygeocoder", "countrycode", "devtools")

## 00b: prepare themes, color pallettes, etc. ----

# color blind friendly pallette with 28 colors for language families

colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", 
            "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", "#1f77b4", "#aec7e8", 
            "#ff7f0e", "#ffbb78", "#2ca02c", "#98df8a", "#d62728", "#ff9896", 
            "#9467bd", "#c5b0d5", "#8c564b", "#c49c94", "#e377c2", "#f7b6d2", 
            "#7f7f7f", "#c7c7c7", "#bcbd22", "#dbdb8d")


my_custom_theme <- theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

# 01: load data ----- 

doreco_rhythm_results_complete <- read_rds("rhythm_results_doreco_ipu_meta_complete.rds")


# 02: data wrangling ----

## 02a: sort median ioi beat ----

# for visualization we also produce a dataset where languages and language family are ordered by median ioi-beat

# Calculate median ioi_beat per language
median_ioi_lan <- doreco_rhythm_results_complete %>%
  group_by(Language) %>%
  summarize(median_ioi_beat = median(ioi_beat, na.rm = TRUE))

# Calculate median ioi_beat per language family
median_ioi_fam <- doreco_rhythm_results_complete %>%
  group_by(Family) %>%
  summarize(median_ioi_beat = median(ioi_beat, na.rm = TRUE))

# copy dataframe
doreco_rhythm_results_complete_ordered <- doreco_rhythm_results_complete 

# Reorder the levels of Language based on median ioi_beat
doreco_rhythm_results_complete_ordered$Language <- factor(doreco_rhythm_results_complete_ordered$Language,
                                                     levels = median_ioi_lan$Language[order(median_ioi_lan$median_ioi_beat)])
doreco_rhythm_results_complete_ordered$Family <- factor(doreco_rhythm_results_complete_ordered$Family,
                                                   levels = median_ioi_fam$Family[order(median_ioi_fam$median_ioi_beat)])


## 02b: summarize by file ----

# after splitting file because of long silences, we summarize by file, to avoid pseudoreplication and inflation of n

doreco_rhythm_results_complete_summarized_file <- doreco_rhythm_results_complete %>%
  filter(!is.na(ioi_beat)) %>%
  group_by(Family, file) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE), 
            across(where(is.character), first), 
            across(where(is.factor), first))


doreco_rhythm_results_complete_ordered_summarized_file <- doreco_rhythm_results_complete_ordered %>%
  filter(!is.na(ioi_beat)) %>%
  group_by(Family, file) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE), 
            across(where(is.character), first), 
            across(where(is.factor), first))

# 03: plots ----

# 03a: map plot ----

# map plot, where are languages spoken?
unique_coords <- unique(doreco_rhythm_results_complete[, c("Latitude", "Longitude")])

# Create a dataframe with unique coordinates
unique_coords_df <- data.frame(
  lon = unique_coords$Longitude,
  lat = unique_coords$Latitude)


map_world <- ggplot2::map_data('world')
land.colour   <- "grey75"
border.colour <- "grey10"
basemap= map_world
xmin= -140
xmax= 180
ymin= -50
ymax= 75

map <- ggplot() +
  coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
  geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
  geom_point(data = unique_coords_df, aes(x = lon, y = lat), color = "red", size = 2) +  # Add points
  ylab("Latitute [°]")+
  xlab("Longitude [°]")+
  ggtitle("Where are the languages spoken?")  ###LP: Change title to `Geographic distribution of the 49 languages in our sample'

map ###LP: Can this map be made centered around the International Date Line? This would be a nice-to-have, not high-priority

## 03b: ioi distribution plots -----

## ioi plots 
doreco_rhythm_results_complete %>% 
  ggplot(aes(x= ioi_beat))+
  geom_histogram(aes(y=stat((count)/sum(stat(count))*100)),
                 color = "white", fill = "darkblue")+
  theme_minimal()+
  ylab("Percentage [%]")+
  xlab("IPU Beat [Hz]")+
  my_custom_theme

doreco_rhythm_results_complete %>% 
  ggplot(aes(x= unbiased_cv))+
  geom_histogram(aes(y=stat((count)/sum(stat(count))*100)),
                 color = "white", fill = "darkblue", binwidth = 0.25)+
  theme_minimal()+
  ylab("Percentage [%]")+
  xlab("CV of IPU Durations")+
  my_custom_theme


## ioi beat language/ language family plots -----

#languages_ipu
###LP: How are languages sorted ?
doreco_rhythm_results_complete_ordered_summarized_file %>%
  dplyr::filter(is.na(ioi_beat) == FALSE) %>% 
  group_by(speaker, Language) %>% 
  ggplot(aes(y = ioi_beat, x = Language, fill = Family))+
  geom_boxplot()+
  geom_jitter(alpha = 0.2)+
  theme_minimal()+
  scale_fill_manual(values = colors)+
  theme(legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = -0.1),
    legend.title = element_blank(),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.35, 'cm'))+
  xlab("Languages")+
  ylab("IPU Beat [Hz]")+
  my_custom_theme

###LP: I would not use this plot as aggregating tone lgs by family makes no sense 
doreco_rhythm_results_complete_ordered_summarized_file %>%
  dplyr::filter(is.na(ioi_beat) == FALSE) %>%
  dplyr::filter(Family != "Isolate") %>% 
  #group_by(speaker,Family) %>%
  group_by(Family) %>%
  ggplot(aes(y = ioi_beat, x = Family, fill = tone))+
  geom_boxplot()+
  geom_jitter(alpha = 0.2)+
  theme_minimal()+
  theme(#legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = -0.1),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, 'cm'))+
  xlab("Language Family")+
  ylab("IPU Beat [Hz]")+
  ggtitle("Tonal Language -- Yes or No")


## 03c: sex differences ----

doreco_rhythm_results_complete_summarized_file %>%
  drop_na(speaker_sex) %>%
  group_by(speaker) %>% 
  ggplot(aes(y = ioi_beat, x = speaker_sex, fill = speaker_sex))+
  geom_boxplot()+
  #geom_jitter(aes( color = Language))
  theme_minimal()+
  xlab("IPU Level") ###LP: change to: "Sex"
  ###LP: add ylab("IOI Beat [Hz]")

doreco_rhythm_results_complete_summarized_file %>%
  drop_na(speaker_sex) %>%
  group_by(speaker) %>%  
  ggplot(aes(y = unbiased_cv, x = speaker_sex, fill = speaker_sex))+
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim = c(0,1))+
  #geom_jitter(aes( color = Language))
  theme_minimal()+
  xlab("IPU Level") ###LP: change to: "Sex"
  ###LP: add ylab("IOI Beat [Hz]")

doreco_rhythm_results_complete_summarized_file %>%
  drop_na(speaker_sex) %>%
  group_by(speaker) %>% 
  ggplot(aes(y = npvi, x = speaker_sex, fill = speaker_sex))+
  geom_boxplot()+
  #coord_cartesian(ylim = c(0,1))+
  #geom_jitter(aes( color = Language))
  theme_minimal()+
  xlab("IPU Level") ###LP: change to: "Sex"
  ###LP: add ylab("IOI Beat [Hz]")

## 03d age differences -----

# medium ioi beat per language vs. medium age per language
median_data <- aggregate(cbind(speaker_age, ioi_beat) ~ Language, data = doreco_rhythm_results_complete_summarized_file, median)

# Create a scatter plot with median ioi_beat against median age
### LP: I would remove this plot, don't see the need to average per language
ggplot(median_data, aes(x = speaker_age, y = ioi_beat)) +
  geom_point() +
  geom_smooth(method = "lm")+
  labs(title = "Median IPU Beat [Hz] vs. Median Age per Language",
       x = "Median Age",
       y = "Median IPU beat [Hz]") +
  theme_minimal()+
  my_custom_theme

# age as facet plot
doreco_rhythm_results_complete_summarized_file %>% 
  ggplot(aes(x= speaker_age, y = ioi_beat ))+
  geom_jitter(aes(fill = Language, color = Language))+
  geom_smooth(method = "lm")+
  facet_grid(rows = vars(Family))+
  theme_minimal()+
  xlab('Speaker Age ')+
  ylab("IPU beat [Hz]")+
  my_custom_theme

# age against ioi beat directly (potentially biased because of differences in age structure per language)
doreco_rhythm_results_complete_summarized_file %>% 
  ggplot(aes(x= speaker_age, y = ioi_beat ))+
  geom_jitter(aes(fill = Language, color = Language))+
  geom_smooth(method = "lm")+
  theme_minimal()+
  theme(legend.position = "none")+
  xlab('Speaker Age ')+
  ylab("IPU beat [Hz]")+
  my_custom_theme

## tone and morphological complexity ----

doreco_rhythm_results_complete_summarized_file %>%
  filter(is.na(tone) == FALSE) %>%
  group_by(speaker) %>% 
  ggplot(aes(x= tone, y = ioi_beat ))+
  geom_boxplot()+
  geom_jitter(size= 0.5, alpha = 0.5)+
  theme_minimal()+
  theme(legend.position = "none")+
  xlab(' Tonal Language')+###LP: 'Tone Language'
  ylab("IPU beat [Hz]")+
  my_custom_theme

doreco_rhythm_results_complete_summarized_file %>%
  group_by(speaker) %>% 
  ggplot(aes(x= synthesis, y = ioi_beat ))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_minimal()+
  theme(legend.position = "none")+
  xlab(' Morphological Complexity ')+###LP: 'Morphological synthesis'
  ylab("IPU beat [Hz]")+
  my_custom_theme


## different continents ----

doreco_rhythm_results_complete_summarized_file %>% 
  ggplot(aes(x= Area, y = ioi_beat ))+
  geom_boxplot()+
  geom_jitter(size= 0.5, alpha = 0.5)+
  theme_minimal()+
  theme(legend.position = "none")+
  xlab(' Area')+
  ylab("IPU beat [Hz]")+
  my_custom_theme

## height information -----

doreco_rhythm_results_complete_summarized_file %>% 
  ggplot(aes(x= avg_height, y = ioi_beat ))+
  geom_jitter(alpha = 0.5, aes(color = Area))+
  geom_smooth(method = "lm")+
  theme_minimal()+
  xlab('Average Height per Country [cm]')+
  ylab("IPU beat [Hz]")+
  my_custom_theme


# 04: statistics -----

# summarize

doreco_rhythm_results_complete %>%
  group_by(speaker) %>% 
  select('ioi_beat', 'unbiased_cv', 'npvi', 'n_elements') %>% 
  summary(na.rm = TRUE)
