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

install_load("tidyverse", "psych", "tidygeocoder", "countrycode", "devtools",
             "lme4", "maps", "effsize","praatpicture", "grid", "ggplotify", "magick")

## 00b: prepare themes, color palettes, etc. ----

# color blind friendly palette with 28 colors for language families

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

#ioi_data <- read_delim("iois_ipu_99quantilebreaks_means.csv", delim = ",")

ioi_data_alternative <- read_delim("unsplit_ipu_data_for_rhythm_analysis_including_meta_data_run_Oct24.csv", delim = ",")

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


## 02c: z-scores ----

mean_ioi_beat <- mean(doreco_rhythm_results_complete$ioi_beat, na.rm = TRUE)
sd_ioi_beat <- sd(doreco_rhythm_results_complete$ioi_beat, na.rm = TRUE)

z_scores <- as.numeric((doreco_rhythm_results_complete$ioi_beat - mean_ioi_beat) / sd_ioi_beat)
z_scores <- as.data.frame(z_scores)

language_zscore <- cbind(z_scores, doreco_rhythm_results_complete$Language, as.numeric(doreco_rhythm_results_complete$ioi_beat))
colnames(language_zscore) <- c("zscore", "language", "ioi_beat")

outlier_indices_top <- language_zscore %>% 
  filter(z_scores >= 1.96)   # 1.96 corresponds to two tailed test, above/below will combine to be 5% --> significance level

min_outlier <- min(outlier_indices_top$ioi_beat) # vertical line x-position for density plot 
n_outliers_top <- nrow(outlier_indices_top)

outlier_indices_bottom <- language_zscore %>% 
  filter(z_scores <= -1.96)

max_outlier <- max(outlier_indices_bottom$ioi_beat) # vertical line x-position for density plot
n_outliers_bottom <- nrow(outlier_indices_bottom)

doreco_rhythm_results_complete <- cbind(doreco_rhythm_results_complete, z_scores)

all_zscore_outliers <- doreco_rhythm_results_complete %>% 
  filter(z_scores >= 1.96 | z_scores <= -1.96)

n_elements_95 <- nrow(doreco_rhythm_results_complete)-nrow(all_zscore_outliers)

# 03: statistics -----

# summarize

doreco_rhythm_results_complete %>%
  group_by(speaker) %>% 
  select('ioi_beat', 'unbiased_cv', 'npvi', 'n_elements') %>% 
  summary(na.rm = TRUE)


summary_by_language_median <- doreco_rhythm_results_complete %>% 
  group_by(Language) %>% 
  summarize(median_ioi = round(median(ioi_beat, na.rm = TRUE), digits = 2),
            median_elements = median(n_elements, na.rm = TRUE),
            speaker_nr = length(unique(speaker)),
            file_nr = length(unique(file)),
            filename_nr = length(unique(filename)))

summary_by_speaker <- doreco_rhythm_results_complete %>% 
  group_by(speaker) %>% 
  summarize(median_ioi_speaker = round(median(ioi_beat, na.rm = TRUE), digits = 2),
            filename_nr = length(unique(filename)),
            file_nr = length(unique(file)))

summary_by_language_iois <- ioi_data_alternative %>% 
  group_by(glottocode) %>% 
  summarize(nr_io_units = length(glottocode))
            
           
## 03a: correlations and effect sizes ----


### effect sizes ----
# (cohen's D, psych package)
# effect size gender

men <- doreco_rhythm_results_complete %>% 
  filter(speaker_sex == "m")
women <- doreco_rhythm_results_complete %>% 
  filter(speaker_sex == "f")

d_gender <- cohen.d(men$ioi_beat, women$ioi_beat)
# d = 0.10 --> negligible

t.test(men$ioi_beat, women$ioi_beat, var.equal = FALSE)
# p = 0.0002 --> significant but negligible

# effect size tone language

toneyes <- doreco_rhythm_results_complete %>% 
  filter(tone == "yes")
toneno <- doreco_rhythm_results_complete %>% 
  filter(tone == "no")

d_tone <- cohen.d(toneyes$ioi_beat, toneno$ioi_beat)
# d = 0.09 --> negligible

t.test(toneyes$ioi_beat, toneno$ioi_beat, var.equal = FALSE)
# p = 0.006 --> significant but negligible, change to former results most likely due to large sample size

### correlations ----

# correlation with median age

cor_age <- corr.test(doreco_rhythm_results_complete$ioi_beat, doreco_rhythm_results_complete$speaker_age)
r_age <- cor_age$r
R_squared_age <- r_age^2

# ~ 0.013 % of variance are explained by age --> negligible

# correlation with morphological synthesis

cor_morph <- corr.test(doreco_rhythm_results_complete$ioi_beat, doreco_rhythm_results_complete$synthesis)
r_morph <- cor_morph$r
R_squared_morph <- r_morph^2

# 1.08% of variance explained, so even though statistically significant, very small effect size




## 03b linear mixed models -----
# SF: effect of tone on ioi_beat?
m1<-lmer(ioi_beat~tone+(1+tone|speaker)+(1+tone|Family), data=doreco_rhythm_results_complete)
summary(m1)
qqnorm(residuals(m1))


# SF: effect of morphology on ioi_beat? This model is not perfect
m2<-lmer(ioi_beat~scale(synthesis)+(0+scale(synthesis)|speaker)+(1+scale(synthesis)|Family), data=doreco_rhythm_results_complete)
summary(m2)
qqnorm(residuals(m2))

# Speaker effects due to gender, age?
m3<-lmer(ioi_beat~speaker_sex+(1+speaker_sex|Family), data=doreco_rhythm_results_complete)
summary(m3)
qqnorm(residuals(m3))

m4<-lmer(ioi_beat~scale(speaker_age)+(1+scale(speaker_age)|Family), data=doreco_rhythm_results_complete)
summary(m4)
qqnorm(residuals(m4))

m5<-lmer(ioi_beat~scale(avg_height)+(1+scale(avg_height)|Family), data=doreco_rhythm_results_complete)
summary(m5)
qqnorm(residuals(m5))




# 04: plots ----

## 04a: map plot ----

# map plot, where are languages spoken?
unique_coords <- unique(doreco_rhythm_results_complete[, c("Latitude", "Longitude")])

# Create a dataframe with unique coordinates
unique_coords_df <- data.frame(
  lon = unique_coords$Longitude,
  lat = unique_coords$Latitude)

unique_coords_df <- unique_coords_df %>%
  mutate(lon = ifelse(lon < 0, lon + 360, lon))

# Fortify the map data
mp1 <- fortify(map(fill=TRUE, plot=FALSE))
mp2 <- mp1
mp2$long <- mp2$long + 360
mp2$group <- mp2$group + max(mp2$group) + 1
mp <- rbind(mp1, mp2)

# Create the base plot with the world map

map <- ggplot() +  # No aesthetics here
  #geom_path(data = mp, aes(x = long, y = lat, group = group), fill = "grey75", color = "grey10") +  # World map outline
  geom_path(data = mp, aes(x = long, y = lat, group = group), fill = "#F2DDC1", color = "grey10") +  # World map outline
  geom_point(data = unique_coords_df, aes(x = lon, y = lat), color = "red", size = 2) +  # Your points
  scale_x_continuous(limits = c(0, 360)) +  # Set x limits
  coord_fixed(ratio = 1) +  # Ensure aspect ratio is fixed
  theme_minimal() + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 12, vjust = -0.5),
    axis.title.y = element_text(size = 12, vjust = 0.5)
  ) +
  labs(
    x = "Longitude [°]",
    y = "Latitude [°]"
  )


# version 2

doreco_rhythm_results_complete_summarized_file$Longitude<-sapply(doreco_rhythm_results_complete_summarized_file$Longitude,function(x) ifelse(x<(-25),x + 360,x))
world <- map_data('world', interior=F, wrap=c(-25,335), ylim=c(-54,79))

map_doreco <- ggplot() +
  geom_polygon(
    data=world,
    aes(x=long,y=lat,group=group),
    colour="grey",fill="#F2DDC1" ,linewidth=0.2, 
  ) + 
  geom_jitter(
    data = doreco_rhythm_results_complete_summarized_file, aes(x = Longitude, y = Latitude), color = "red", size = 2
  ) +
  #scale_x_continuous(limits = c(0, 360)) +  # Set x limits
  coord_fixed(ratio = 1) +  # Ensure aspect ratio is fixed
  #scale_fill_viridis_d(option="D") +
  # geom_label_repel(box.padding=0.5, point.padding=0.5,
  #                  data=languages, aes(Longitude, Latitude, label=Name), 
  #                  min.segment.length=unit(0, 'lines'),
  #                  size=5, max.overlaps=99) +
  scale_x_continuous(name=NULL, breaks=NULL) +
  scale_y_continuous(name=NULL, breaks=NULL) +
  theme_minimal() +
  theme(legend.position="none") 

ggsave("map_plot_fig1_part1.jpg", dpi = 300,
       width = 12,
       height = 6)

## 04b: ioi distribution plots -----

## raw interval distribution

hist_ioi_raw <- ioi_data_alternative %>% 
  ggplot(aes(x= io_duration))+
  geom_histogram(aes(y=stat((count)/sum(stat(count))*100)),
                 color = "white", fill = "darkblue", bins = 100)+
  theme_minimal()+
  coord_cartesian(xlim = c(0,10))+
  annotate("text", x = 5, y = 5, label = paste("n =", nrow(ioi_data_alternative)), size = 6)+
  ylab("Percentage [%]")+
  xlab("IOI [sec]")+
  #annotate("text", x = 0.6, y = 9, label = "n = 1535")+
  my_custom_theme
print(hist_ioi_raw)


## ioi beat plots 
hist_ioi_beat <- doreco_rhythm_results_complete %>% 
  ggplot(aes(x= ioi_beat))+
  geom_histogram(aes(y=stat((count)/sum(stat(count))*100)),
                 color = "white", fill = "darkblue")+
  theme_minimal()+
  ylab("Percentage [%]")+
  xlab("IOI Beat [Hz]")+
  annotate("text", x = 1.5, y = 9, label = paste("n =", nrow(doreco_rhythm_results_complete)), size = 6)+
  my_custom_theme
print(hist_ioi_beat)

hist_cv <- doreco_rhythm_results_complete %>% 
  ggplot(aes(x= unbiased_cv))+
  geom_histogram(aes(y=stat((count)/sum(stat(count))*100)),
                 color = "white", fill = "darkblue", binwidth = 0.1)+
  theme_minimal()+
  ylab("Percentage [%]")+
  xlab("CV of IOI Durations per Sequence")+
  annotate("text", x = 1.0, y = 15, label = paste("n =", nrow(doreco_rhythm_results_complete)), , size = 6)+
  my_custom_theme
print(hist_cv)

## 04c: zscore plots ----
# density plot of all calculated ioi beats across all the dataset

density_ioibeat <- 
  doreco_rhythm_results_complete %>% 
  ggplot(aes(x = ioi_beat))+
  geom_density()+
  geom_vline(xintercept = min_outlier, linetype="dotted", linewidth = 2 )+  # zscore > 2, corresponding ioi beat
  geom_vline(xintercept = max_outlier, linetype="dotted", linewidth = 2)+  # zscore < -2, corresponding ioi beat
  geom_jitter(aes(y = -1, color = Language), alpha = 0.5, size = 0.5)+
  annotate("text", x = max_outlier-0.5, y = -0.3, label = paste("n =", n_outliers_bottom), size = 5)+
  annotate("text", x = max_outlier-0.5, y = 2, label = expression("Z-Score "<="-1.96"), size = 5)+
  annotate("text", x = 0.5, y = -0.3, label = paste("n =",n_elements_95 ), size = 5)+
  annotate("text", x = 1.5, y = -0.3, label = paste("n =", n_outliers_top), size = 5)+
  annotate("text", x = 1.5, y = 2, label = expression("Z-Score ">="1.96"), size = 5)+  
  coord_cartesian(xlim = c(-0.5, 3))+
  ylab("Density")+
  xlab("IOI Beat [Hz]")+
  my_custom_theme

#ggsave("plot_density_zscore.jpg", dpi = 300,
#       width = 12,
#       height = 4)

## what are the outliers? is there a trend here? age? 
doreco_rhythm_results_complete %>% 
  ggplot(aes(x= unbiased_cv, y = z_scores))+
  geom_point()+
  geom_smooth()

doreco_rhythm_results_complete %>% 
  ggplot(aes(x= npvi, y = z_scores))+
  geom_point()+
  geom_smooth()

doreco_rhythm_results_complete %>% 
  ggplot(aes(x= n_elements, y = z_scores))+
  geom_point()

doreco_rhythm_results_complete %>% 
  ggplot(aes(x= synthesis, y = z_scores))+
  geom_point()

doreco_rhythm_results_complete %>% 
  ggplot(aes(x= speaker_age, y = z_scores))+
  geom_point()

doreco_rhythm_results_complete %>% 
  ggplot(aes(x = speaker_sex, y = z_scores))+
  geom_boxplot()

doreco_rhythm_results_complete %>% 
  ggplot(aes(x= speaker_age, y = avg_height))+
  geom_point()

doreco_rhythm_results_complete %>% 
  ggplot(aes(x = Area, y = z_scores))+
  geom_boxplot()

doreco_rhythm_results_complete %>% 
  ggplot(aes(x = tone, y = z_scores))+
  geom_boxplot()

## 04d: ioi beat language/ language family plots ----

#languages_ipu

boxplot_languages <- doreco_rhythm_results_complete_ordered %>% 
  dplyr::filter(is.na(ioi_beat) == FALSE) %>% 
  group_by(speaker, Language) %>% 
  ggplot(aes(y = ioi_beat, x = Language, fill = Family))+
  geom_boxplot(outliers = FALSE)+
  geom_jitter(alpha = 0.2, size = 0.7)+
  theme_minimal()+
  scale_fill_manual(values = colors)+
  coord_cartesian(ylim = c(0,1.75))+
  theme(legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = -0.1),
    legend.title = element_blank(),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.35, 'cm'))+
  xlab("Languages")+
  ylab("IOI Beat [Hz]")+
  my_custom_theme+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))

# not in publication, sorted by Language Family, note "Isolates" are not included
doreco_rhythm_results_complete_ordered %>%
  dplyr::filter(is.na(ioi_beat) == FALSE) %>%
  dplyr::filter(Family != "Isolate") %>% 
  #group_by(speaker,Family) %>%
  group_by(Family) %>%
  ggplot(aes(y = ioi_beat, x = Family))+
  geom_boxplot(outliers = FALSE)+
  geom_jitter(alpha = 0.2)+
  theme_minimal()+
  theme(#legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = -0.1),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, 'cm'))+
  xlab("Language Family")+
  ylab("IOI Beat [Hz]")


## 04e: sex differences ----

box_gender <- doreco_rhythm_results_complete_summarized_file %>%
  drop_na(speaker_sex) %>%
  group_by(speaker) %>% 
  ggplot(aes(y = ioi_beat, x = speaker_sex))+
  geom_boxplot(outliers = FALSE)+
  geom_jitter(alpha = 0.3, size = 0.8)+
  theme_minimal()+
  xlab("Gender")+
  ylab("IOI Beat [Hz]")+
  my_custom_theme+
  scale_x_discrete(labels = c("f" = "Female", "m" = "Male"))+
  annotate("text", x = 1.5, y = 0.8, label = paste("Cohen's D = ", round(d_gender$estimate, 2)), size = 5)

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

## 04f age differences -----

# medium ioi beat per language vs. medium age per language
median_data <- aggregate(cbind(speaker_age, ioi_beat) ~ Language, data = doreco_rhythm_results_complete_summarized_file, median)

# Create a scatter plot with median ioi_beat against median age
### LP: I would remove this plot, don't see the need to average per language
###LSB: we discussed this, to avoid seeing a bias of differences in median age spoken per language

ggplot(median_data, aes(x = speaker_age, y = ioi_beat)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#0072B2")+
  labs(x = "Median Age per Language",
       y = "Median IOI Beat [Hz] per Language") +
  theme_minimal()+
  my_custom_theme
 

# age against ioi beat directly (potentially biased because of differences in age structure per language)
scatter_age <- doreco_rhythm_results_complete_summarized_file %>% 
  ggplot(aes(x= speaker_age, y = ioi_beat ))+
  geom_jitter(alpha = 0.8, size = 0.8)+
  geom_smooth(method = "lm", color = "#0072B2")+
  theme_minimal()+
  theme(legend.position = "none")+
  xlab('Speaker Age ')+
  ylab("IOI beat [Hz]")+
  my_custom_theme+
  annotate("text", x = 80, y = 0.8, label = paste("R² =", round(R_squared_age, 3)), size = 5)
  

## 04g: tone and morphological complexity ----

box_tone <- doreco_rhythm_results_complete_summarized_file %>%
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
  scale_x_discrete(labels = c("no" = "No", "yes" = "Yes"))+
  annotate("text", x = 1.5, y = 0.8, label = paste("Cohen's D = ", round(d_tone$estimate, 2)), size = 5)

  
###SF: Can you explain how morphological synsthesis is calculated. For each speaker here?
### This means the lower the morphological complexity the higher the IOI beat? 

cor_morph <- cor(doreco_rhythm_results_complete$ioi_beat, doreco_rhythm_results_complete$synthesis,
                 na.rm = TRUE)

scatter_morph <- doreco_rhythm_results_complete_summarized_file %>%
  group_by(speaker) %>% 
  ggplot(aes(x= synthesis, y = ioi_beat ))+
  geom_point()+
  geom_smooth(method = "lm", color = "#0072B2")+
  theme_minimal()+
  theme(legend.position = "none")+
  xlab('Morphological Synthesis')+
  ylab("IOI beat [Hz]")+
  my_custom_theme+
  annotate("text", x = 3, y = 0.8, label = paste("R² =", round(R_squared_morph, 2)), size = 5)


## 04h: different continents ----

doreco_rhythm_results_complete_summarized_file %>% 
  ggplot(aes(x= Area, y = ioi_beat ))+
  geom_boxplot(outliers = FALSE)+
  geom_jitter(size= 0.5, alpha = 0.5)+
  theme_minimal()+
  theme(legend.position = "none")+
  xlab(' Area')+
  ylab("IOI beat [Hz]")+
  my_custom_theme

## 04i: height information -----

doreco_rhythm_results_complete_summarized_file %>% 
  ggplot(aes(x= avg_height, y = ioi_beat ))+
  geom_jitter(alpha = 0.5, aes(color = Area))+
  geom_smooth(method = "lm")+
  theme_minimal()+
  xlab('Average Height per Country [cm]')+
  ylab("IOI beat [Hz]")+
  my_custom_theme+
  scale_color_manual(values = colors)

## 04j: n element information ----

hist_n_element <- doreco_rhythm_results_complete %>% 
  ggplot(aes(x= n_elements))+
  geom_histogram(aes(y=stat((count)/sum(stat(count))*100)),
                 color = "white", fill = "darkblue", 
                 boundary = 4)+
  theme_minimal()+
  #coord_cartesian(xlim = c(4,270))+
  ylab("Percentage [%]")+
  xlab("n Elements per Sequence")+
  annotate("text", x = 100, y = 20, label = paste("n =", nrow(rhythm_results_doreco_ipu)), size = 6)+
  my_custom_theme
print(hist_n_element)


# n elements vs. cv 

doreco_rhythm_results_complete %>% 
  ggplot(aes(x= n_elements, y = unbiased_cv))+
  geom_point(alpha = 0.5, shape = 21)+
  #geom_smooth(method = "lm")+
  theme_minimal()+
  #coord_cartesian(xlim = c(4,270))+
  ylab("CV")+
  xlab("n Elements per Sequence")+
  #annotate("text", x = 100, y = 20, label = paste("n =", nrow(rhythm_results_doreco_ipu)), size = 6)+
  my_custom_theme

cor(doreco_rhythm_results_complete$n_elements, doreco_rhythm_results_complete$unbiased_cv)
# r = 0.02 --> negligible, but higher variability for shorter sequences 

## 04k: Annotation Plot from Praat ------

png("annotation_plot_fig1_part2.png", width = 12, height = 6, units = "in",  res = 300)

annotation_plot <- praatpicture(
  'BeAM_199X_HumanInLandOfDeath_flk_fragment.wav',
  start = 0, 
  end = 3.3,
  frames = c('sound', 'spectrogram', 'TextGrid'),
  proportion = c(30, 40, 30),
  spec_freqRange = c(0, 16000),
  
  tg_tiers = c('text-dolgan', 'text-english', 'word-dolgan', 'word-english'),
  tg_color = c('blue', 'black', 'black', 'black'),
  tg_focusTier = 'all',
  tg_focusTierLineType = c('solid', 'solid', 'dashed', 'dashed'),
  tg_focusTierColor = c('black', 'black', 'black', 'black'),
  
  draw_arrow = c('spectrogram', 0.478, 8500, 2.321, 8500, code = 3,
                 length = 0.15, 
                 angle = 20, col = 'blue', lwd = 2, lty = 'solid'),
  annotate = list(
    c('spectrogram', 1.3, 7000, col = 'blue', font = 2, cex = 1.8, labels = 'Inter-Onset-Interval (IOI)')#,
    #c('spectrogram', 2.0, 5000, col = 'red', font = 2, cex = 1.8, labels = 'Second Annotation')
  )
)

dev.off()

# 05: plot grids -----

## 05a: Figure 1 ----

# part 3 of Figure 1:  Histograms IOI distribution [sec]  

hist_plots <- cowplot::plot_grid(hist_ioi_raw, hist_cv,hist_n_element,
                   labels = c("C", "D", "E"), ncol = 3)

ggsave("hist_plot_fig1_part3.jpg", dpi = 300, hist_plots,
       width = 12,
       height = 6)

#part 1: annotations and part 2: map are generated and saved further up, figures where
# combined outside of R

## 05b: Figure 2-----

# Density Plot & Language Boxplots

cowplot::plot_grid(density_ioibeat, boxplot_languages,
                   nrow = 2,
                   rel_heights = c(0.4, 0.6),
                   labels = c("A", "B"))

ggsave("manuscript_figure2.jpg", dpi = 300,
       width = 28,
       height = 26,
       units = "cm")

## 05c: Figure 3 ----

cowplot::plot_grid(scatter_age, scatter_morph,
                   box_gender, box_tone, ncol = 2,
                   labels = c("A", "B", "C", "D"))


ggsave("manuscript_figure3.jpg", dpi = 300,
       width = 22,
       height = 20,
       units = "cm")


