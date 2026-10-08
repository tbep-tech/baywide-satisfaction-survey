library(tidyverse)

# Load data
final_data <- read.csv("survey_data/bss.csv")

bss <- final_data[,-1]
rownames(bss) <- final_data[,1]


######### UNDERWATER ######### 


######### *--- Data Preparation ######### 

# Non-experts

bss_underwater <- bss %>%
  # Include everyone but the experts
  filter(EXPERT == "No") %>%
  # Keep only what we need
  select(AUDIENCE, ORIGIN, DURATION, AGE, WQ_IMAGE_UNDER, WQ_UNDER_QUALITY:WQ_UNDER_EXPECT) %>%
  # Calculate years living in Tampa Bay
  mutate(YEARS = ifelse(is.na(ORIGIN), NA,
                          ifelse(ORIGIN == "Yes", AGE, DURATION))) %>%
  select(-c(ORIGIN, DURATION, AGE)) %>%
  # Set WQA as reference level for comparisons
  mutate(WQ_IMAGE_UNDER = factor(WQ_IMAGE_UNDER, levels = c("WQA","WQB","WQC","WQD"))) %>%
  # Convert expectation to binary indicator of normalcy
  mutate(WQ_UNDER_EXPECT = ifelse(is.na(WQ_UNDER_EXPECT), NA,
                                  ifelse(WQ_UNDER_EXPECT == "Yes", 1, 0))) %>%
  # Rename metrics for easier tracking
  rename_with(~ sub("^WQ_UNDER_", "", .x), starts_with("WQ_UNDER_")) %>%
  drop_na() %>%
  # Remove respondents who assigned the same score to every characteristic (potential biased/invalid responses)
  rowwise() %>%
  filter(n_distinct(c_across(SMELL:CONTAM)) > 1) %>%  # approx <1% dropped
  ungroup() %>%
  mutate(Sample = AUDIENCE)

# Objective Image Characteristics

wq_characteristics <- read.csv("image_data/wq_characteristics.csv")
wq_char_underwater <- wq_characteristics %>%
  # Include underwater images
  filter(IMAGE == "WQA" | IMAGE == "WQB" | IMAGE == "WQC" | IMAGE == "WQD") %>%
  # Keep only what we need
  select(-c(RB_RATIO, GB_RATIO)) %>%
  # Set WQA as reference level for comparisons
  mutate(WQ_IMAGE_UNDER = factor(IMAGE, levels = c("WQA","WQB","WQC","WQD"))) %>%
  drop_na()

# Add objective characteristics to survey data

bss_underwater <- merge(bss_underwater, wq_char_underwater, by = "WQ_IMAGE_UNDER", all.x = TRUE)



# Experts

experts <- read.csv("survey_data/expert_image_evaluations.csv")
experts_underwater <- experts %>%
  # Include just the underwater quality experts
  filter(IMAGE == "WQA" | IMAGE == "WQB" | IMAGE == "WQC" | IMAGE == "WQD") %>%
  # Keep only what we need
  select(IMAGE, QUALITY:EXPECT) %>%
  # Convert expectation to binary indicator of normalcy
  mutate(EXPECT = ifelse(is.na(EXPECT), NA,
                         ifelse(EXPECT == "Yes", 1, 0))) %>%
  # Set WQA as reference level for comparisons
  mutate(WQ_IMAGE_UNDER = factor(IMAGE, levels = c("WQA","WQB","WQC","WQD"))) %>%
  mutate(Sample = "Experts")

means_experts <- experts_underwater %>%
  group_by(WQ_IMAGE_UNDER) %>%
  summarise(
    mean = mean(QUALITY, na.rm = TRUE),
    sd = sd(QUALITY, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(Sample = "Experts")

dv_list <- c("SMELL", "SAFESWIM", "SAFEWILD", "SEWAGE", 
             "CONTAM", "EXPECT")
means_experts_char <- experts_underwater %>%
  pivot_longer(cols = all_of(dv_list), 
               names_to = "Characteristic", 
               values_to = "Score") %>%
  group_by(WQ_IMAGE_UNDER, Characteristic) %>%
  summarise(
    mean = mean(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(Sample = "Experts")



# PLOTS

means_df <- bss_underwater %>%
  group_by(WQ_IMAGE_UNDER, Sample) %>%
  summarise(
    mean = mean(QUALITY, na.rm = TRUE),
    sd = sd(QUALITY, na.rm = TRUE),
    .groups = "drop")

means_all <- rbind(means_df, means_experts)


# Means
plot_UNDER_QUALITY <- ggplot(means_all, aes(x = WQ_IMAGE_UNDER, y = mean, color = Sample)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("Experts" = "#D7632B", "In-network" = "#00806E", "Public" = "#000000")) +
  scale_y_continuous(limits = c(0, 10.4), breaks = seq(0, 10, by = 2)) +
  labs(x = "Underwater Image", y = "Water Quality Score") +
  theme_minimal() +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = 'transparent', color = NA),
        legend.box.background = element_rect(fill = 'transparent', color = NA),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        panel.grid  = element_blank())

# Boxplots
all_underwater <- bind_rows(bss_underwater, experts_underwater)
plot_UNDER_QUALITY <- ggplot(all_underwater, aes(x = WQ_IMAGE_UNDER, y = QUALITY, fill = Sample, color = Sample)) +
  geom_boxplot(width = 0.7, outlier.shape = NA, alpha = 0.5) +  # hide outliers for cleaner look
  scale_fill_manual(values = c("Experts" = "#00806E", "Public" = "#000000")) +
  scale_color_manual(values = c("Experts" = "#00806E", "Public" = "#000000")) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  labs(x = "Underwater Image", y = "Water Quality Score") +
  theme_minimal() +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = 'transparent', color = NA),
        legend.box.background = element_rect(fill = 'transparent', color = NA),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        panel.grid  = element_blank())

# PLOTS

means_df <- bss_underwater %>%
  pivot_longer(cols = all_of(dv_list), 
               names_to = "Characteristic", 
               values_to = "Score") %>%
  group_by(WQ_IMAGE_UNDER, Sample, Characteristic) %>%
  summarise(
    mean = mean(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE),
    .groups = "drop") 

means_all <- bind_rows(means_df, means_experts_char) %>%
  filter(Characteristic != "EXPECT") %>%
  mutate(Characteristic = factor(Characteristic, levels = c("SMELL","SAFESWIM","SAFEWILD","SEWAGE","CONTAM")))


# Means
plot_UNDER_CHARACTERISTICS <- ggplot(means_all, aes(x = WQ_IMAGE_UNDER, y = mean, color = Sample, 
                                                    group = interaction(Sample, Characteristic))) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("Experts" = "#D7632B", "In-network" = "#00806E", "Public" = "#000000")) +
  facet_wrap(~ Characteristic, ncol = 3, nrow = 2) +
  scale_y_continuous(limits = c(0, 10.7), breaks = seq(0, 10, by = 2)) +
  labs(x = "Underwater Image", y = "Rating") +
  theme_minimal() +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = 'transparent', color = NA),
        legend.box.background = element_rect(fill = 'transparent', color = NA),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        panel.grid  = element_blank())


# Boxplots
all_underwater <- bind_rows(bss_underwater, experts_underwater)
plot_UNDER_CHARACTERISTICS <- ggplot(all_underwater %>%
         pivot_longer(cols = all_of(dv_list), names_to = "Characteristic", values_to = "Score") %>%
         mutate(Characteristic = factor(Characteristic, levels = c("SMELL","SAFESWIM","SAFEWILD","SEWAGE","CONTAM"))),
       aes(x = WQ_IMAGE_UNDER, y = Score, fill = Sample, color = Sample)) +
  geom_boxplot(width = 0.7, outlier.shape = NA, alpha = 0.5) +  # hide outliers for cleaner look
  scale_fill_manual(values = c("Experts" = "#00806E", "Public" = "#000000")) +
  scale_color_manual(values = c("Experts" = "#00806E", "Public" = "#000000")) +
  facet_wrap(~ Characteristic, ncol = 3, nrow = 2) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  labs(x = "Underwater Image", y = "Rating") +
  theme_minimal() +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = 'transparent', color = NA),
        legend.box.background = element_rect(fill = 'transparent', color = NA),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        panel.grid  = element_blank())


all_underwater <- bind_rows(bss_underwater, experts_underwater)
EXPECT <- all_underwater %>%
  drop_na(EXPECT) %>%
  mutate(EXPECT = ifelse(EXPECT == 1, "Yes", "No")) %>%
  group_by(Sample, WQ_IMAGE_UNDER, EXPECT) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_EXPECT <- ggplot(EXPECT %>%
                            filter(EXPECT == "Yes"),
                          aes(x = WQ_IMAGE_UNDER, y = pct, fill = Sample, color = Sample)) +
  geom_col(width = 0.7, position = position_dodge(width = 0.7), alpha = 0.7) +
  scale_fill_manual(values = c("Experts" = "#D7632B", "In-network" = "#00806E", "Public" = "#000000")) +
  scale_color_manual(values = c("Experts" = "#D7632B", "In-network" = "#00806E", "Public" = "#000000")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(x = "Underwater Image", y = "Expected in Tampa Bay (%)") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = 'transparent'),
    plot.background = element_rect(fill = 'transparent', color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = 'transparent', color = NA),
    legend.box.background = element_rect(fill = 'transparent', color = NA),
    strip.text = element_text(face = "bold"),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    panel.grid = element_blank())





######### SURFACE WATER ######### 


######### *--- Data Preparation ######### 

# Non-experts

bss_ABOVEwater <- bss %>%
  # Include everyone but the experts
  filter(EXPERT == "No") %>%
  # Keep only what we need
  select(AUDIENCE, ORIGIN, DURATION, AGE, WQ_IMAGE_ABOVE, WQ_ABOVE_QUALITY:WQ_ABOVE_EXPECT) %>%
  # Calculate years living in Tampa Bay
  mutate(YEARS = ifelse(is.na(ORIGIN), NA,
                        ifelse(ORIGIN == "Yes", AGE, DURATION))) %>%
  select(-c(ORIGIN, DURATION, AGE)) %>%
  # Set WQW as reference level for comparisons
  mutate(WQ_IMAGE_ABOVE = factor(WQ_IMAGE_ABOVE, levels = c("WQW","WQX","WQY","WQZ"))) %>%
  # Convert expectation to binary indicator of normalcy
  mutate(WQ_ABOVE_EXPECT = ifelse(is.na(WQ_ABOVE_EXPECT), NA,
                                  ifelse(WQ_ABOVE_EXPECT == "Yes", 1, 0))) %>%
  # Rename metrics for easier tracking
  rename_with(~ sub("^WQ_ABOVE_", "", .x), starts_with("WQ_ABOVE_")) %>%
  drop_na() %>%
  # Remove respondents who assigned the same score to every characteristic (potential biased/invalid responses)
  rowwise() %>%
  filter(n_distinct(c_across(SMELL:CONTAM)) > 1) %>%  # approx <1% dropped
  ungroup() %>%
  mutate(Sample = AUDIENCE)

# Objective Image Characteristics

wq_characteristics <- read.csv("image_data/wq_characteristics.csv")
wq_char_ABOVEwater <- wq_characteristics %>%
  # Include ABOVEwater images
  filter(IMAGE == "WQW" | IMAGE == "WQX" | IMAGE == "WQY" | IMAGE == "WQZ") %>%
  # Keep only what we need
  select(-c(RB_RATIO_BACK:GB_RATIO_FORE)) %>%
  # Set WQW as reference level for comparisons
  mutate(WQ_IMAGE_ABOVE = factor(IMAGE, levels = c("WQW","WQX","WQY","WQZ"))) %>%
  drop_na()

# Add objective characteristics to survey data

bss_ABOVEwater <- merge(bss_ABOVEwater, wq_char_ABOVEwater, by = "WQ_IMAGE_ABOVE", all.x = TRUE)



# Experts

experts <- read.csv("survey_data/expert_image_evaluations.csv")
experts_ABOVEwater <- experts %>%
  # Include just the ABOVEwater quality experts
  filter(IMAGE == "WQW" | IMAGE == "WQX" | IMAGE == "WQY" | IMAGE == "WQZ") %>%
  # Keep only what we need
  select(IMAGE, QUALITY:EXPECT) %>%
  # Convert expectation to binary indicator of normalcy
  mutate(EXPECT = ifelse(is.na(EXPECT), NA,
                         ifelse(EXPECT == "Yes", 1, 0))) %>%
  # Set WQW as reference level for comparisons
  mutate(WQ_IMAGE_ABOVE = factor(IMAGE, levels = c("WQW","WQX","WQY","WQZ"))) %>%
  mutate(Sample = "Experts")

means_experts <- experts_ABOVEwater %>%
  group_by(WQ_IMAGE_ABOVE) %>%
  summarise(
    mean = mean(QUALITY, na.rm = TRUE),
    sd = sd(QUALITY, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(Sample = "Experts")

dv_list <- c("SMELL", "SAFESWIM", "SAFEWILD", "SEWAGE", 
             "CONTAM", "EXPECT")
means_experts_char <- experts_ABOVEwater %>%
  pivot_longer(cols = all_of(dv_list), 
               names_to = "Characteristic", 
               values_to = "Score") %>%
  group_by(WQ_IMAGE_ABOVE, Characteristic) %>%
  summarise(
    mean = mean(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(Sample = "Experts")



# PLOTS

means_df <- bss_ABOVEwater %>%
  group_by(WQ_IMAGE_ABOVE, Sample) %>%
  summarise(
    mean = mean(QUALITY, na.rm = TRUE),
    sd = sd(QUALITY, na.rm = TRUE),
    .groups = "drop") 

means_all <- rbind(means_df, means_experts)


# Means
plot_ABOVE_QUALITY <- ggplot(means_all, aes(x = WQ_IMAGE_ABOVE, y = mean, color = Sample)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("Experts" = "#D7632B", "In-network" = "#00806E", "Public" = "#000000")) +
  scale_y_continuous(limits = c(0, 10.4), breaks = seq(0, 10, by = 2)) +
  labs(x = "Surface Water Image", y = "Water Quality Score") +
  theme_minimal() +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = 'transparent', color = NA),
        legend.box.background = element_rect(fill = 'transparent', color = NA),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        panel.grid  = element_blank())

# Boxplots
all_ABOVEwater <- bind_rows(bss_ABOVEwater, experts_ABOVEwater)
plot_ABOVE_QUALITY <- ggplot(all_ABOVEwater, aes(x = WQ_IMAGE_ABOVE, y = QUALITY, fill = Sample, color = Sample)) +
  geom_boxplot(width = 0.7, outlier.shape = NA, alpha = 0.5) +  # hide outliers for cleaner look
  scale_fill_manual(values = c("Experts" = "#00806E", "Public" = "#000000")) +
  scale_color_manual(values = c("Experts" = "#00806E", "Public" = "#000000")) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  labs(x = "Surface Water Image", y = "Water Quality Score") +
  theme_minimal() +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = 'transparent', color = NA),
        legend.box.background = element_rect(fill = 'transparent', color = NA),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        panel.grid  = element_blank())

# PLOTS

means_df <- bss_ABOVEwater %>%
  pivot_longer(cols = all_of(dv_list), 
               names_to = "Characteristic", 
               values_to = "Score") %>%
  group_by(WQ_IMAGE_ABOVE, Sample, Characteristic) %>%
  summarise(
    mean = mean(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE),
    .groups = "drop") 

means_all <- bind_rows(means_df, means_experts_char) %>%
  filter(Characteristic != "EXPECT") %>%
  mutate(Characteristic = factor(Characteristic, levels = c("SMELL","SAFESWIM","SAFEWILD","SEWAGE","CONTAM")))


# Means
plot_ABOVE_CHARACTERISTICS <- ggplot(means_all, aes(x = WQ_IMAGE_ABOVE, y = mean, color = Sample, 
                                                    group = interaction(Sample, Characteristic))) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("Experts" = "#D7632B", "In-network" = "#00806E", "Public" = "#000000")) +
  facet_wrap(~ Characteristic, ncol = 3, nrow = 2) +
  scale_y_continuous(limits = c(-0.2, 10.1), breaks = seq(0, 10, by = 2)) +
  labs(x = "Surface Water Image", y = "Rating") +
  theme_minimal() +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = 'transparent', color = NA),
        legend.box.background = element_rect(fill = 'transparent', color = NA),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        panel.grid  = element_blank())


# Boxplots
all_ABOVEwater <- bind_rows(bss_ABOVEwater, experts_ABOVEwater)
plot_ABOVE_CHARACTERISTICS <- ggplot(all_ABOVEwater %>%
                                       pivot_longer(cols = all_of(dv_list), names_to = "Characteristic", values_to = "Score") %>%
                                       mutate(Characteristic = factor(Characteristic, levels = c("SMELL","SAFESWIM","SAFEWILD","SEWAGE","CONTAM"))),
                                     aes(x = WQ_IMAGE_ABOVE, y = Score, fill = Sample, color = Sample)) +
  geom_boxplot(width = 0.7, outlier.shape = NA, alpha = 0.5) +  # hide outliers for cleaner look
  scale_fill_manual(values = c("Experts" = "#00806E", "Public" = "#000000")) +
  scale_color_manual(values = c("Experts" = "#00806E", "Public" = "#000000")) +
  facet_wrap(~ Characteristic, ncol = 3, nrow = 2) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  labs(x = "Surface Water Image", y = "Rating") +
  theme_minimal() +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = 'transparent', color = NA),
        legend.box.background = element_rect(fill = 'transparent', color = NA),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        panel.grid  = element_blank())


all_ABOVEwater <- bind_rows(bss_ABOVEwater, experts_ABOVEwater)
EXPECT <- all_ABOVEwater %>%
  drop_na(EXPECT) %>%
  mutate(EXPECT = ifelse(EXPECT == 1, "Yes", "No")) %>%
  group_by(Sample, WQ_IMAGE_ABOVE, EXPECT) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_EXPECT <- ggplot(EXPECT %>%
                        filter(EXPECT == "Yes"),
                      aes(x = WQ_IMAGE_ABOVE, y = pct, fill = Sample, color = Sample)) +
  geom_col(width = 0.7, position = position_dodge(width = 0.7), alpha = 0.7) +
  scale_fill_manual(values = c("Experts" = "#D7632B", "In-network" = "#00806E", "Public" = "#000000")) +
  scale_color_manual(values = c("Experts" = "#D7632B", "In-network" = "#00806E", "Public" = "#000000")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(x = "Surface Water Image", y = "Expected in Tampa Bay (%)") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = 'transparent'),
    plot.background = element_rect(fill = 'transparent', color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = 'transparent', color = NA),
    legend.box.background = element_rect(fill = 'transparent', color = NA),
    strip.text = element_text(face = "bold"),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    panel.grid = element_blank())



