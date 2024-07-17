# Codes from: "Human languages are less variable in rhythm than XXX"
# submitted to: 

# Post Processing, Analysis and Visualisations of DoReCo Rhythm Analysis
# Lara S. Burchardt, Ludger Paschen, Susanne Fuchs


# the pre-processed results are prepared in lines 21 to 115 and then saved, for a speedy process
# start with the saved .rds file (see line 115) and start with section 3, line 119

#00: packages -----
if (!require(install.load)) {
  install.packages("install.load")
}

library(install.load)

install_load("tidyverse", "psych","lsr", "tidygeocoder", "countrycode", "devtools", "cowplot")

#01: load data ----

# meta data languages

# languages_meta <- read_delim("doreco_languages_metadata_v1.3.csv", delim = ",")
# 
# # meta data files
# 
# meta_data_file <- read_delim("unsplit_ipu_data_for_rhythm_analysis_including_meta_data_run_April24.csv", delim = ",")
# 
# # meta data, morphologic complexity + ton language information
# 
# meta_data_morph_tone <- read_delim("Typo_Info_Rhythm_Project.csv", delim = ";")
# meta_data_morph_tone <- meta_data_morph_tone %>% 
#   select(Language, Glottocode, `Complexity `, Tone) %>% 
#   mutate(Complexity = `Complexity `)
# 
# # loading rhythm results 
# rhythm_results_doreco_ipu <- read_delim("language_data/results/rhythm_analysis_doreco_ipu_mean_apr24_fs_20.csv", delim = ",")
# 
# for(i in 1:length(rhythm_results_doreco_ipu$filename)){
#   names <- str_split(rhythm_results_doreco_ipu$filename[i], pattern = "_")
#   names <- names[[1]]
#   rhythm_results_doreco_ipu$language[i] <- names[[3]]
#   rhythm_results_doreco_ipu$Glottocode[i] <- names[[3]]
#   rhythm_results_doreco_ipu$speaker[i] <- names[[7]]
#   rhythm_results_doreco_ipu$speaker[i] <- substring(rhythm_results_doreco_ipu$speaker[i], 1, nchar(rhythm_results_doreco_ipu$speaker[i])-4)
#   rhythm_results_doreco_ipu$type[i] <- names[[1]]
#   rhythm_results_doreco_ipu$file[i] <- names[[5]]   #filename from raw file, to use to join rhythm results with meta data file
# }
# 
# #  meta data like speaker sex and speaker age needs to be extracted/joined from raw data extraction from Database
# 
# rhythm_results_doreco_ipu_meta <- left_join(rhythm_results_doreco_ipu, meta_data_file, by = "file", multiple = "any")
# 
# rhythm_results_doreco_ipu_meta <- left_join(rhythm_results_doreco_ipu_meta, languages_meta, by = "Glottocode", multiple = "any")
# 
# rhythm_results_doreco_ipu_meta<- left_join(rhythm_results_doreco_ipu_meta, meta_data_morph_tone, by = "Glottocode", multiple = "any")
# 
# # 02: data preparation -----
# 
# ## 02a: coordinates -----
# 
# for(a in 1:nrow(rhythm_results_doreco_ipu_meta)){
#   #  for(a in 1:5){
#   lat_longs <- tibble::tribble(
#     ~latitude,        ~longitude,
#     rhythm_results_doreco_ipu_meta$Latitude[a],           rhythm_results_doreco_ipu_meta$Longitude[a]
#   )
#   
#   reverse <-  lat_longs %>%
#     reverse_geocode(lat = latitude, long = longitude, method = 'osm',
#                     address = address_found, full_results = TRUE) %>%
#     select(country, country_code)
#   
#   rhythm_results_doreco_ipu_meta$country[a] <- reverse$country
#   rhythm_results_doreco_ipu_meta$country_code[a] <- reverse$country_code
#   
#   country_name_eng <- countrycode(reverse$country_code, "iso2c", "country.name")
#   
#   rhythm_results_doreco_ipu_meta$country_eng[a] <- country_name_eng
#   
#   rm(reverse)
#   
# }
# 
# ## 02b: average height per country ----
# 
# #join height information from Baten & Blum (2015) dataset with rhythm results
# # https://clio-infra.eu/Indicators/Height.html downloaded from here, only column names were
# # changed slightly to fit the format 
# # Myanmar was renamed to "Myanmar(Burma)" as it was named like that in the geocoding lists
# 
# 
# heights <- read_delim("average-height-of-men-for-selected-countries.csv", delim = ";")
# 
# heights <- heights %>% 
#   select(country_eng, Year, Height)
# 
# heights_avg <- heights %>%
#   group_by(country_eng) %>%
#   summarise(avg_height = mean(Height, na.rm = TRUE))
# 
# 
# rhythm_results_doreco_ipu_meta<- left_join(rhythm_results_doreco_ipu_meta, heights_avg, by = "country_eng", multiple = "any")
# 
# # use data from ipu dataframe to add country and height information to the word dataframe as well
# # we split the ipu dataframe, selecting only the relevant data: longitude,lattitude, country, country_eng, avg_height
# 
# country_height_info <- rhythm_results_doreco_ipu_meta %>% 
#   select(Longitude, Latitude, country, country_code, country_eng, avg_height)
# 
# rhythm_results_doreco_word_meta <- left_join(rhythm_results_doreco_word_meta, country_height_info, by = c("Longitude", "Latitude"), multiple = "any")
# 
# 
# 
# #saveRDS(rhythm_results_doreco_word_meta, file = "rhythm_results_doreco_word_meta.rds")
# saveRDS(rhythm_results_doreco_ipu_meta, file = "rhythm_results_doreco_ipu_meta.rds")

# 03: analysis ----

# in this chunk we load the final data version, that we prepared in the chunks above and saved in line 115 
# as these preparations need some time to calculate, we here give the option to knit the document faster, by loading the final dataframes from .rds

rhythm_results_doreco_ipu_meta <- readRDS("rhythm_results_doreco_ipu_meta.rds")

# delete duplicate Language column
rhythm_results_doreco_ipu_meta <- rhythm_results_doreco_ipu_meta %>% 
  mutate(Language = Language.x) %>% 
  select(-Language.y, -Language.x)


# add detailed complexity score

complexity_score <- read_delim("DoReCo_1_3_core_synthesis.csv", delim = ",")


rhythm_results_doreco_ipu_meta <- 

# 03a: re-order----
# Calculate median ioi_beat per language
median_ioi_lan <- rhythm_results_doreco_ipu_meta %>%
  group_by(Language) %>%
  summarize(median_ioi_beat = median(ioi_beat, na.rm = TRUE))

# Calculate median ioi_beat per language family
median_ioi_fam <- rhythm_results_doreco_ipu_meta %>%
  group_by(Family) %>%
  summarize(median_ioi_beat = median(ioi_beat, na.rm = TRUE))

# copy dataframe
rhythm_results_doreco_ipu_ordered <- rhythm_results_doreco_ipu_meta 

# Reorder the levels of Language based on median ioi_beat
rhythm_results_doreco_ipu_ordered$Language <- factor(rhythm_results_doreco_ipu_ordered$Language,
                                                     levels = median_ioi_lan$Language[order(median_ioi_lan$median_ioi_beat)])
rhythm_results_doreco_ipu_ordered$Family <- factor(rhythm_results_doreco_ipu_ordered$Family,
                                                   levels = median_ioi_fam$Family[order(median_ioi_fam$median_ioi_beat)])

# 03b: map plot -----
# map plot, where are languages spoken?
unique_coords <- unique(rhythm_results_doreco_ipu_meta[, c("Latitude", "Longitude")])

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
  xlab("Longitude [°]")#+
  #ggtitle("Where are the languages spoken?")


map





# 03c: Histogram IPU ----

hist_ioi_beat <- rhythm_results_doreco_ipu_meta %>% 
  ggplot(aes(x= ioi_beat))+
  geom_histogram(aes(y= after_stat(count/sum(count)) * 100),
                 color = "white", fill = "darkblue", bins = 60)+
  theme_minimal()+
  ylab("Percentage [%]")+
  xlab("IPU Beat [Hz] \n   ")+
  geom_text(aes(x = 0.7, y = 5, label = "n = 1481"), color = "black", size = 5)#+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18))

hist_ioi_cv <- rhythm_results_doreco_ipu_meta %>% 
  ggplot(aes(x= unbiased_cv))+
  geom_histogram(aes(y=after_stat(count/sum(count)) * 100),
                 color = "white", fill = "darkblue", bins = 60)+
  theme_minimal()+
  ylab("Percentage [%]")+
  #xlab(expression(paste("Coeffcient of Variation \n IPU Durations ")))
  xlab("Coefficient of Variation \n IPU Durations")+
  geom_text(aes(x = 2.5, y = 15, label = "n = 1481"), color = "black", size = 5)#+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18))



# Figure 1 ----

fig1_part_hist <- cowplot::plot_grid(hist_ioi_beat, hist_ioi_cv, labels = c("C", "D"),nrow = 1, rel_widths = c(0.5,0.5))

fig1_part_data <- cowplot::plot_grid(map, fig1_part_hist, nrow = 2, labels = c("B", ""), rel_heights  = c(0.5, 0.5))

ggsave("doreco_map.jpg", fig1_part_data,
       width = 18,
       height = 12,
       units = "cm")

# 03d: Boxplots IPU ----

colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", 
            "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", "#1f77b4", "#aec7e8", 
            "#ff7f0e", "#ffbb78", "#2ca02c", "#98df8a", "#d62728", "#ff9896", 
            "#9467bd", "#c5b0d5", "#8c564b", "#c49c94", "#e377c2", "#f7b6d2", 
            "#7f7f7f", "#c7c7c7", "#bcbd22", "#dbdb8d")

## by Speaker ## 

rhythm_results_doreco_ipu_ordered_summarized_speaker %>%
  dplyr::filter(is.na(ioi_beat) == FALSE) %>% 
  group_by(speaker, Language) %>% 
  ggplot(aes(y = ioi_beat, x = Language, fill = Family))+
  geom_boxplot()+
  geom_jitter(alpha = 0.2)+
  theme_minimal()+
  scale_fill_manual(values = colors)+
  theme(#legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = -0.1),
    legend.title = element_blank(),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.35, 'cm'))+
  xlab("Languages")+
  ylab("IPU Beat [Hz]")+
  ggtitle("Grouped by Speaker")


rhythm_results_doreco_ipu_ordered_summarized_speaker %>%
  dplyr::filter(is.na(ioi_beat) == FALSE) %>%
  dplyr::filter(Family != "Isolate") %>% 
  group_by(speaker,Family) %>%
  #group_by(Family) %>%
  ggplot(aes(y = ioi_beat, x = Family, fill = Tone))+
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
  ggtitle("Grouped by Speaker")

## by File ##

#box_languages <- rhythm_results_doreco_ipu_ordered_summarized_file %>%
box_languages <- rhythm_results_doreco_ipu_ordered %>%
  dplyr::filter(is.na(ioi_beat) == FALSE) %>% 
  group_by(Language) %>% 
  ggplot(aes(y = ioi_beat, x = Language, fill = Family))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(alpha = 0.2, size = 0.4)+
  theme_minimal()+
  scale_fill_manual(values = colors)+
  theme(legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = -0.1),
    legend.title = element_blank(),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.35, 'cm'),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18),
    title = element_text(size = 18))+
  xlab("Languages")+
  ylab("IPU Beat [Hz]")


box_languages_family <- rhythm_results_doreco_ipu_ordered %>%
  dplyr::filter(is.na(ioi_beat) == FALSE) %>%
  dplyr::filter(Family != "Isolate") %>% 
  #group_by(speaker,Family) %>%
  group_by(Family) %>%
  ggplot(aes(y = ioi_beat, x = Family, fill = Tone))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(alpha = 0.2, size = 0.4)+
  theme_minimal()+
  theme(legend.position = "bottom",
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = -0.1),
    #axis.title.x = element_text(hjust = 1),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, 'cm'),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18),
    title = element_text(size = 18))+
  xlab("Language Family")+
  ylab("IPU Beat [Hz]")


fig2 <- cowplot::plot_grid(box_languages, box_languages_family, labels = c("A", "B"), nrow = 2, rel_heights =  c(0.5,0.5))

ggsave("doreco_fig2.jpg", fig2,
       width = 20,
       height = 30,
       units = "cm")


# 03e: sex differences ----

male <- rhythm_results_doreco_ipu_meta %>% 
  filter(speaker_sex == "m")

female <- rhythm_results_doreco_ipu_meta %>% 
  filter(speaker_sex == "f")

t.test(male$ioi_beat, female$ioi_beat, paired = FALSE, var.equal = FALSE)

plot_sex_f_m <- rhythm_results_doreco_ipu_meta %>%
  drop_na(speaker_sex) %>%
  ggplot(aes(y = ioi_beat, x = speaker_sex))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(size= 0.5, alpha = 0.5)+
  theme_minimal()+
  scale_x_discrete(labels=c("Female","Male")) +
  #ylab("IOI Beat [Hz]")+
  xlab("Sex")+
  geom_segment(aes(x = "f", xend = "m", y = 0.82, yend = 0.82),
               color = "black", linewidth = 1)+
  geom_text(aes(x = 1.5, y = 0.85, label = "ns"), color = "black", size = 4, check_overlap = TRUE)+
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title.y =  element_blank(),
        axis.title = element_text(size = 18),
        title = element_text(size = 18))
  

# 03f: age differences ----

median_data <- aggregate(cbind(speaker_age, ioi_beat) ~ Language, data = rhythm_results_doreco_ipu_meta_summarized_file, median)

cor_age <-corr.test(rhythm_results_doreco_ipu_meta$speaker_age, rhythm_results_doreco_ipu_meta$ioi_beat)

# Create a scatter plot with median ioi_beat against median age
plot_age <- rhythm_results_doreco_ipu_meta %>% 
  ggplot(aes(x= speaker_age, y = ioi_beat ))+
  geom_jitter(aes(fill = Family, color = Family))+
  scale_fill_manual(values = colors)+
  geom_smooth(method = "lm")+
  theme_minimal()+
  theme(legend.position = "none")+
  xlab('Speaker Age ')+
  ylab("IOI Beat [Hz]")+
  geom_text(aes(x = 90, y = 0.7, label = "r = -0.06"), color = "black", size = 4, check_overlap = TRUE)+
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        title = element_text(size = 18))


# 03g : tone differences ----
tone <- rhythm_results_doreco_ipu_meta %>% 
  filter(Tone == "yes")

no_tone <- rhythm_results_doreco_ipu_meta %>% 
  filter(Tone == "no")

t.test(tone$ioi_beat, no_tone$ioi_beat, paired = FALSE, var.equal = FALSE)
cohensD(x= tone$ioi_beat, y= no_tone$ioi_beat)


plot_tone <- rhythm_results_doreco_ipu_meta %>%
  filter(is.na(Tone) == FALSE) %>%
  ggplot(aes(x= Tone, y = ioi_beat ))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(size= 0.5, alpha = 0.5)+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        title = element_text(size = 18))+
  xlab(' Tonal Language')+
  ylab("IOI Beat [Hz]")+
  geom_segment(aes(x = "no", xend = "yes", y = 0.76, yend = 0.76),
                                    color = "black", linewidth = 1)+
  geom_text(aes(x = 1.5, y = 0.82, label = "p = 0.002 ; D = 0.18"), color = "black", size = 4, check_overlap = TRUE)


# 03h: morphologic complexity ----

# Define the groups for comparison
group_names <- na.omit(unique(rhythm_results_doreco_ipu_meta$Complexity))

# Initialize an empty dataframe to store results
result_summary <- data.frame(
  group1 = character(),
  group2 = character(),
  p_value = numeric(),
  adj_p_value = numeric(),
  cohen_d = numeric()
)

# Perform t-tests for each pair of groups
for (i in 1:length(group_names)) {
  for (j in (i+1):length(group_names)) {
    group1 <- group_names[i]
    group2 <- group_names[j]
    
    subset1 <- rhythm_results_doreco_ipu_meta %>%
      filter(Complexity == group1) %>%
      select(ioi_beat)
    
    subset2 <- rhythm_results_doreco_ipu_meta %>%
      filter(Complexity == group2) %>%
      select(ioi_beat)
    
    # Check if both subsets have at least one observation
    if (nrow(subset1) > 0 && nrow(subset2) > 0) {
      # Perform t-test
      result <- t.test(subset1$ioi_beat, subset2$ioi_beat, var.equal = FALSE)
      
      # Calculate Cohen's d
      cohens_d <- cohensD(subset1$ioi_beat, subset2$ioi_beat)
      
      # Add the results to the dataframe
      result_summary <- rbind(result_summary, data.frame(
        group1 = group1,
        group2 = group2,
        p_value = result$p.value,
        adj_p_value = NA,  # Placeholder for adjusted p-value
        cohen_d = cohens_d#$estimate
      ))
    }
  }
}

# Adjust p-values using Bonferroni correction
result_summary$adj_p_value <- p.adjust(result_summary$p_value, method = "bonferroni")

# Print the summary


# Convert p-values to character to display as "<0.001" when significant
result_summary$p_value_summary <- ifelse(result_summary$p_value < 0.05, "*", sprintf("%.3f", result_summary$p_value))

result_summary <- result_summary[1:3,1:6]

print(result_summary)

# plot
plot_morph <- rhythm_results_doreco_ipu_meta %>%
  group_by(speaker) %>% 
  ggplot(aes(x= Complexity, y = ioi_beat, group = Complexity ))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(size= 0.5, alpha = 0.5)+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title.y =  element_blank(),
        axis.title = element_text(size = 18),
        title = element_text(size = 18))+
  xlab(' Morphological Complexity ')+
  geom_segment(aes(x = 1, xend = 2, y = 0.70, yend = 0.70),
               color = "black", linewidth = 1)+
  geom_segment(aes(x = 2, xend = 3, y = 0.75, yend = 0.75),
               color = "black", linewidth = 1)+
  geom_segment(aes(x = 1, xend = 3, y = 0.80, yend = 0.80),
               color = "black", linewidth = 1)+
  geom_text(aes(x = 1.5, y = 0.72, label = "p adj.= 1 ; D = 0.04"), color = "black", size = 4, check_overlap = TRUE)+
  geom_text(aes(x = 2.5, y = 0.77, label = "p adj. = 0.19 ; D = 0.12"), color = "black", size = 4, check_overlap = TRUE)+
  geom_text(aes(x = 2, y = 0.82, label = "p adj. = 0.05* ; D = 0.16"), color = "black", size = 4, check_overlap = TRUE)

  
fig3 <- cowplot::plot_grid(plot_age, plot_sex_f_m, plot_tone, plot_morph, labels = c("A", "B", "C", "D"), nrow = 2, ncol = 2, h.align = TRUE)


ggsave("fig3.jpg", fig3,
       width = 18,
       height = 22,
       units = "cm",
       dpi = 300)
