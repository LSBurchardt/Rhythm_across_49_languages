# Rhythm Analysis of DoReCo languages
# Involved: Susanne Fuchs, Ludger Paschen, Lara S. Burchardt
# R Codes from: Lara S. Burchardt 
# Affiliation: Leibniz Center General Linguistics, Leibniz-Zentrum Allgemeine Sprachwissenschaft Berlin

# Script  6 of 6

# Plots and Analyses for rhythm analysis

###LP: Globally: Replace IPU by IOI (?) -- done
###LP: Rename repository from "DoReCo" to something more specific like "IOI beat across languages" -- done 

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
    axis.text.x = element_text(angle = 0, hjust = 0.5)
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

## 03a: map plot ----

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
  my_custom_theme
  #ggtitle("Where are the languages spoken?")  ###LP: Change title to `Geographic distribution of the 49 languages in our sample'
###LSB I wanted to delete the title anyway, but add this as a legend to the plot in the manuscript

map ###LP: Can this map be made centered around the International Date Line? This would be a nice-to-have, not high-priority --
###LSB we talked about this before, I tried a few different ways but couldn't find an easy way to do that with this plotting version

## 03b: ioi distribution plots -----

## ioi plots 
hist_ioi <- doreco_rhythm_results_complete %>% 
  ggplot(aes(x= ioi_beat))+
  geom_histogram(aes(y=stat((count)/sum(stat(count))*100)),
                 color = "white", fill = "darkblue")+
  theme_minimal()+
  ylab("Percentage [%]")+
  xlab("IOI Beat [Hz]")+
  annotate("text", x = 0.6, y = 9, label = "n = 1535")+
  my_custom_theme

hist_cv <- doreco_rhythm_results_complete %>% 
  ggplot(aes(x= unbiased_cv))+
  geom_histogram(aes(y=stat((count)/sum(stat(count))*100)),
                 color = "white", fill = "darkblue", binwidth = 0.25)+
  theme_minimal()+
  ylab("Percentage [%]")+
  xlab("CV of IOI Durations")+
  annotate("text", x = 1.5, y = 20, label = "n = 1535")+
  my_custom_theme


## ioi beat language/ language family plots

#languages_ipu
###LP: How are languages sorted ? --- see 02a, median ioi beat of language/language family

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
  ylab("IOI Beat [Hz]")+
  my_custom_theme


doreco_rhythm_results_complete_ordered_summarized_file %>%
  dplyr::filter(is.na(ioi_beat) == FALSE) %>%
  dplyr::filter(Family != "Isolate") %>% 
  #group_by(speaker,Family) %>%
  group_by(Family) %>%
  ggplot(aes(y = ioi_beat, x = Family))+
  geom_boxplot()+
  geom_jitter(alpha = 0.2)+
  theme_minimal()+
  theme(#legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = -0.1),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, 'cm'))+
  xlab("Language Family")+
  ylab("IOI Beat [Hz]")+
  ggtitle("Tonal Language -- Yes or No")


## 03c: sex differences ----

doreco_rhythm_results_complete_summarized_file %>%
  drop_na(speaker_sex) %>%
  group_by(speaker) %>% 
  ggplot(aes(y = ioi_beat, x = speaker_sex))+
  geom_boxplot(outliers = FALSE)+
  geom_jitter(alpha = 0.3, size = 0.8)+
  theme_minimal()+
  xlab("Gender")+
  ylab("IOI Beat [Hz]")+
  my_custom_theme+
  scale_x_discrete(labels = c("f" = "Female", "m" = "Male"))

doreco_rhythm_results_complete_summarized_file %>%
  drop_na(speaker_sex) %>%
  group_by(speaker) %>%  
  ggplot(aes(y = unbiased_cv, x = speaker_sex))+
  geom_boxplot(outliers = FALSE)+
  coord_cartesian(ylim = c(0.15,1.5))+
  geom_jitter(alpha = 0.3, size = 0.8)+
  theme_minimal()+
  xlab("Gender")+
  ylab("Coefficient of Variation")+
  my_custom_theme+
  scale_x_discrete(labels = c("f" = "Female", "m" = "Male"))

doreco_rhythm_results_complete_summarized_file %>%
  drop_na(speaker_sex) %>%
  group_by(speaker) %>% 
  ggplot(aes(y = npvi, x = speaker_sex))+
  geom_boxplot(outliers = FALSE)+
  #coord_cartesian(ylim = c(0,1))+
  geom_jitter(alpha = 0.3, size = 0.8)+
  theme_minimal()+
  xlab("Gender")+
  ylab("nPVI")+
  my_custom_theme+
  scale_x_discrete(labels = c("f" = "Female", "m" = "Male"))

## 03d age differences -----

# medium ioi beat per language vs. medium age per language
median_data <- aggregate(cbind(speaker_age, ioi_beat) ~ Language, data = doreco_rhythm_results_complete_summarized_file, median)

# Create a scatter plot with median ioi_beat against median age
### LP: I would remove this plot, don't see the need to average per language
###LSB: we discussed this, to avoid seeing a bias of differences in median age spoken per language
# I think this was actually your idea
ggplot(median_data, aes(x = speaker_age, y = ioi_beat)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkgrey")+
  labs(x = "Median Age per Language",
       y = "Median IOI Beat [Hz] per Language") +
  theme_minimal()+
  my_custom_theme

# age as facet plot
# doreco_rhythm_results_complete_summarized_file %>% 
#   ggplot(aes(x= speaker_age, y = ioi_beat ))+
#   geom_jitter(aes(fill = Language, color = Language))+
#   geom_smooth(method = "lm")+
#   facet_grid(rows = vars(Family))+
#   theme_minimal()+
#   xlab('Speaker Age ')+
#   ylab("IOI beat [Hz]")
 

# age against ioi beat directly (potentially biased because of differences in age structure per language)
doreco_rhythm_results_complete_summarized_file %>% 
  ggplot(aes(x= speaker_age, y = ioi_beat ))+
  geom_jitter(alpha = 0.8, size = 0.8)+
  geom_smooth(method = "lm", color = "darkgrey")+
  theme_minimal()+
  theme(legend.position = "none")+
  xlab('Speaker Age ')+
  ylab("IOI beat [Hz]")+
  my_custom_theme

## 03e: tone and morphological complexity ----

doreco_rhythm_results_complete_summarized_file %>%
  filter(is.na(tone) == FALSE) %>%
  group_by(speaker) %>% 
  ggplot(aes(x= tone, y = ioi_beat ))+
  geom_boxplot(outliers = FALSE)+
  geom_jitter(size= 0.5, alpha = 0.5)+
  theme_minimal()+
  theme(legend.position = "none")+
  xlab(' Tone Language')+
  ylab("IOI beat [Hz]")+
  my_custom_theme+
  scale_x_discrete(labels = c("no" = "No", "yes" = "Yes"))
  

doreco_rhythm_results_complete_summarized_file %>%
  group_by(speaker) %>% 
  ggplot(aes(x= synthesis, y = ioi_beat ))+
  geom_point()+
  geom_smooth(method = "lm", color = "darkgrey")+
  theme_minimal()+
  theme(legend.position = "none")+
  xlab('Morphological Synthesis')+
  ylab("IOI beat [Hz]")+
  my_custom_theme


## 03f: different continents ----

doreco_rhythm_results_complete_summarized_file %>% 
  ggplot(aes(x= Area, y = ioi_beat ))+
  geom_boxplot(outliers = FALSE)+
  geom_jitter(size= 0.5, alpha = 0.5)+
  theme_minimal()+
  theme(legend.position = "none")+
  xlab(' Area')+
  ylab("IOI beat [Hz]")+
  my_custom_theme

## 03g: height information -----

doreco_rhythm_results_complete_summarized_file %>% 
  ggplot(aes(x= avg_height, y = ioi_beat ))+
  geom_jitter(alpha = 0.5, aes(color = Area))+
  geom_smooth(method = "lm")+
  theme_minimal()+
  xlab('Average Height per Country [cm]')+
  ylab("IOI beat [Hz]")+
  my_custom_theme+
  scale_color_manual(values = colors)

# 04: plot grids -----

## 04a: Figure 1 ----

# Map, Histograms for IOI and CV distribution 

hist_plots <- cowplot::plot_grid(hist_ioi, hist_cv,
                   labels = c("C", "D"))

cowplot::plot_grid(map, hist_plots,
                   labels = c("B", "", ""), nrow = 2)

ggsave("manuscript_figure1_part2.jpg", dpi = 300,
       width = 20,
       height = 16)
# 05: statistics -----

# summarize

doreco_rhythm_results_complete %>%
  group_by(speaker) %>% 
  select('ioi_beat', 'unbiased_cv', 'npvi', 'n_elements') %>% 
  summary(na.rm = TRUE)
