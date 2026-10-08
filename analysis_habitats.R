library(tidyverse)

# Load data
final_data <- read.csv("survey_data/bss.csv")

bss <- final_data[,-1]
rownames(bss) <- final_data[,1]


######### SHORELINE DESIGNS ######### 


######### *--- Data Preparation ######### 

# Non-experts

bss_shorelines <- bss %>%
  # Include everyone but the experts
  filter(EXPERT == "No") %>%
  # Keep only what we need
  select(AUDIENCE, NATRELATED, NAT_SATISFACTION, HAB_IMAGE_SHORE:HAB_SHORE_ECONOMY) %>%
  # Rename habitat images for easier tracking
  mutate(HAB_IMAGE_SHORE = ifelse(is.na(HAB_IMAGE_SHORE), NA, 
                                  ifelse(HAB_IMAGE_SHORE == "HABA", "Oyster",
                                         ifelse(HAB_IMAGE_SHORE == "HABB", "Reef ball",
                                                ifelse(HAB_IMAGE_SHORE == "HABC", "Rip rap", "Seawall"))))) %>%
  # Set seawall as reference level for comparisons
  mutate(HAB_IMAGE_SHORE = factor(HAB_IMAGE_SHORE, levels = c("Seawall","Rip rap","Reef ball","Oyster"))) %>%
  # Rename metrics for easier tracking
  rename_with(~ sub("^HAB_SHORE_", "", .x), starts_with("HAB_SHORE_")) %>%
  drop_na() %>%
  # Remove respondents who assigned the same score to every characteristic (potential biased/invalid responses)
  rowwise() %>%
  filter(n_distinct(c_across(NATURAL:ECONOMY)) > 1) %>%  # approx 5% dropped
  ungroup() %>%
  mutate(Sample = AUDIENCE)


# Experts

experts <- read.csv("survey_data/expert_image_evaluations.csv")
experts_shorelines <- experts %>%
  # Include just the shoreline experts
  filter(IMAGE == "HABA" | IMAGE == "HABB" | IMAGE == "HABC" | IMAGE == "HABD") %>%
  # Keep only what we need
  select(IMAGE, DESIRE:ECONOMY) %>%
  # Rename habitat images for easier tracking
  mutate(HAB_IMAGE_SHORE = ifelse(is.na(IMAGE), NA, 
                                  ifelse(IMAGE == "HABA", "Oyster",
                                         ifelse(IMAGE == "HABB", "Reef ball",
                                                ifelse(IMAGE == "HABC", "Rip rap", "Seawall"))))) %>%
  # Set seawall as reference level for comparisons
  mutate(HAB_IMAGE_SHORE = factor(HAB_IMAGE_SHORE, levels = c("Seawall","Rip rap","Reef ball","Oyster"))) %>%
  mutate(Sample = "Experts")

means_experts <- experts_shorelines %>%
  group_by(HAB_IMAGE_SHORE) %>%
  summarise(
    mean = mean(DESIRE, na.rm = TRUE),
    sd = sd(DESIRE, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(Sample = "Experts")

dv_list <- c("NATURAL", "BEAUTY", "UNIQUE", "SAFETY", 
             "RECREATE", "SOCIALIZE", "ECONOMY")
means_experts_char <- experts_shorelines %>%
  pivot_longer(cols = all_of(dv_list), 
               names_to = "Characteristic", 
               values_to = "Score") %>%
  group_by(HAB_IMAGE_SHORE, Characteristic) %>%
  summarise(
    mean = mean(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(Sample = "Experts")





# PLOTS

means_df <- bss_shorelines %>%
  group_by(HAB_IMAGE_SHORE, Sample) %>%
  summarise(
    mean = mean(DESIRE, na.rm = TRUE),
    sd = sd(DESIRE, na.rm = TRUE),
    .groups = "drop")

means_all <- rbind(means_df, means_experts)


# Means
plot_SHORE_DESIRE <- ggplot(means_all, aes(x = HAB_IMAGE_SHORE, y = mean, color = Sample)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.2, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("Experts" = "#D7632B", "In-network" = "#00806E", "Public" = "#000000")) +
  scale_y_continuous(limits = c(0, 10.3), breaks = seq(0, 10, by = 2)) +
  labs(x = "Shoreline Type", y = "Mean Desirability Score") +
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
all_shorelines <- bind_rows(bss_shorelines, experts_shorelines)
plot_SHORE_DESIRE <- ggplot(all_shorelines, aes(x = HAB_IMAGE_SHORE, y = DESIRE, fill = Sample, color = Sample)) +
  geom_boxplot(width = 0.7, outlier.shape = NA, alpha = 0.5) +  # hide outliers for cleaner look
  scale_fill_manual(values = c("Experts" = "#00806E", "Public" = "#000000")) +
  scale_color_manual(values = c("Experts" = "#00806E", "Public" = "#000000")) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  labs(x = "Shoreline Type", y = "Desirability Score") +
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

means_df <- bss_shorelines %>%
  pivot_longer(cols = all_of(dv_list), 
               names_to = "Characteristic", 
               values_to = "Score") %>%
  group_by(HAB_IMAGE_SHORE, Sample, Characteristic) %>%
  summarise(
    mean = mean(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE),
    .groups = "drop") 


means_all <- bind_rows(means_df, means_experts_char) %>%
  mutate(Characteristic = factor(Characteristic, levels = c("NATURAL","BEAUTY","UNIQUE","SAFETY","RECREATE","SOCIALIZE","ECONOMY")))


# Means
plot_SHORE_CHARACTERISTICS <- ggplot(means_all, aes(x = HAB_IMAGE_SHORE, y = mean, color = Sample, 
                                                    group = interaction(Sample, Characteristic))) +
  geom_line(position = position_dodge(width = 0.5), linetype = "33",  # longer dots, more spacing
            size = 0.8) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.2, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("Experts" = "#D7632B", "In-network" = "#00806E", "Public" = "#000000")) +
  facet_wrap(~ Characteristic, ncol = 4, nrow = 2) +
  scale_y_continuous(limits = c(0, 10.7), breaks = seq(0, 10, by = 2)) +
  labs(x = "Shoreline Type", y = "Mean Rating") +
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
all_shorelines <- bind_rows(bss_shorelines, experts_shorelines)

plot_SHORE_CHARACTERISTICS <- ggplot(all_shorelines %>%
                                       pivot_longer(cols = all_of(dv_list), names_to = "Characteristic", values_to = "Score") %>%
                                       mutate(Characteristic = factor(Characteristic, levels = c("NATURAL","BEAUTY","UNIQUE","SAFETY","RECREATE","SOCIALIZE","ECONOMY"))),
                                     aes(x = HAB_IMAGE_SHORE, y = Score, fill = Sample, color = Sample)) +
  geom_boxplot(width = 0.7, outlier.shape = NA, alpha = 0.5) +  # hide outliers for cleaner look
  scale_fill_manual(values = c("Experts" = "#00806E", "Public" = "#000000")) +
  scale_color_manual(values = c("Experts" = "#00806E", "Public" = "#000000")) +
  facet_wrap(~ Characteristic, ncol = 4, nrow = 2) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  labs(x = "Shoreline Type", y = "Rating") +
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





######### HABITATS (WILDERNESS) ######### 


######### *--- Data Preparation ######### 

# Non-experts

bss_habitats <- bss %>%
  # Include everyone but the experts
  filter(EXPERT == "No") %>%
  # Keep only what we need
  select(AUDIENCE, NATRELATED, NAT_SATISFACTION, HAB_IMAGE_WILD:HAB_WILD_ECONOMY) %>%
  # Rename habitat images for easier tracking
  mutate(HAB_IMAGE_WILD = ifelse(is.na(HAB_IMAGE_WILD), NA, 
                                 ifelse(HAB_IMAGE_WILD == "HABW", "Upland (Flatwood)",
                                        ifelse(HAB_IMAGE_WILD == "HABX", "Wetland (Forested)",
                                               ifelse(HAB_IMAGE_WILD == "HABY", "Wetland (Non-forested)", "Oyster bar"))))) %>%
  # Set upland as reference level for comparisons
  mutate(HAB_IMAGE_WILD = factor(HAB_IMAGE_WILD, levels = c("Upland (Flatwood)","Wetland (Forested)","Wetland (Non-forested)","Oyster bar"))) %>%
  # Rename metrics for easier tracking
  rename_with(~ sub("^HAB_WILD_", "", .x), starts_with("HAB_WILD_")) %>%
  drop_na() %>%
  # Remove respondents who assigned the same score to every characteristic (potential biased/invalid responses)
  rowwise() %>%
  filter(n_distinct(c_across(NATURAL:ECONOMY)) > 1) %>%  # approx 5% dropped
  ungroup() %>%
  mutate(Sample = AUDIENCE)


# Experts

#experts <- read.csv("survey_data/expert_image_evaluations.csv")
experts_habitats <- experts %>%
  # Include just the wilderness habitat experts
  filter(IMAGE == "HABW" | IMAGE == "HABX" | IMAGE == "HABY" | IMAGE == "HABZ") %>%
  # Keep only what we need
  select(IMAGE, DESIRE:ECONOMY) %>%
  # Rename habitat images for easier tracking
  mutate(HAB_IMAGE_WILD = ifelse(is.na(IMAGE), NA, 
                                 ifelse(IMAGE == "HABW", "Upland (Flatwood)",
                                        ifelse(IMAGE == "HABX", "Wetland (Forested)",
                                               ifelse(IMAGE == "HABY", "Wetland (Non-forested)", "Oyster bar"))))) %>%
  # Set upland as reference level for comparisons
  mutate(HAB_IMAGE_WILD = factor(HAB_IMAGE_WILD, levels = c("Upland (Flatwood)","Wetland (Forested)","Wetland (Non-forested)","Oyster bar"))) %>%
  mutate(Sample = "Experts")

means_experts <- experts_habitats %>%
  group_by(HAB_IMAGE_WILD) %>%
  summarise(
    mean = mean(DESIRE, na.rm = TRUE),
    sd = sd(DESIRE, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(Sample = "Experts")

dv_list <- c("NATURAL", "BEAUTY", "UNIQUE", "SAFETY", 
             "RECREATE", "SOCIALIZE", "ECONOMY")
means_experts_char <- experts_habitats %>%
  pivot_longer(cols = all_of(dv_list), 
               names_to = "Characteristic", 
               values_to = "Score") %>%
  group_by(HAB_IMAGE_WILD, Characteristic) %>%
  summarise(
    mean = mean(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(Sample = "Experts")



# PLOTS

means_df <- bss_habitats %>%
  group_by(HAB_IMAGE_WILD, Sample) %>%
  summarise(
    mean = mean(DESIRE, na.rm = TRUE),
    sd = sd(DESIRE, na.rm = TRUE),
    .groups = "drop")

means_all <- rbind(means_df, means_experts)


# Means
plot_WILD_DESIRE <- ggplot(means_all, aes(x = HAB_IMAGE_WILD, y = mean, color = Sample)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.2, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("Experts" = "#D7632B", "In-network" = "#00806E", "Public" = "#000000")) +
  scale_y_continuous(limits = c(0, 10.8), breaks = seq(0, 10, by = 2)) +
  labs(x = "Habitat Type", y = "Mean Desirability Score") +
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
all_habitats <- bind_rows(bss_habitats, experts_habitats)
plot_WILD_DESIRE <- ggplot(all_habitats, aes(x = HAB_IMAGE_WILD, y = DESIRE, fill = Sample, color = Sample)) +
  geom_boxplot(width = 0.7, outlier.shape = NA, alpha = 0.5) +  # hide outliers for cleaner look
  scale_fill_manual(values = c("Experts" = "#00806E", "Public" = "#000000")) +
  scale_color_manual(values = c("Experts" = "#00806E", "Public" = "#000000")) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  labs(x = "Habitat Type", y = "Desirability Score") +
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

means_df <- bss_habitats %>%
  pivot_longer(cols = all_of(dv_list), 
               names_to = "Characteristic", 
               values_to = "Score") %>%
  group_by(HAB_IMAGE_WILD, Sample, Characteristic) %>%
  summarise(
    mean = mean(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE),
    .groups = "drop") 

means_all <- bind_rows(means_df, means_experts_char)



# Means
plot_WILD_CHARACTERISTICS <- ggplot(means_all, aes(x = HAB_IMAGE_WILD, y = mean, color = Sample, 
                                                   group = interaction(Sample, Characteristic))) +
  geom_line(position = position_dodge(width = 0.5), linetype = "33",  # longer dots, more spacing
            size = 0.8) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.2, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("Experts" = "#D7632B", "In-network" = "#00806E", "Public" = "#000000")) +
  facet_wrap(~ Characteristic, ncol = 4, nrow = 2) +
  ylim(0,10.5) +
  labs(x = "Habitat Type", y = "Mean Rating") +
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
all_habitats <- bind_rows(bss_habitats, experts_habitats)
plot_WILD_CHARACTERISTICS <- ggplot(all_habitats %>%
                                      pivot_longer(cols = all_of(dv_list), names_to = "Characteristic", values_to = "Score") %>%
                                      mutate(Characteristic = factor(Characteristic, levels = c("NATURAL","BEAUTY","UNIQUE","SAFETY","RECREATE","SOCIALIZE","ECONOMY"))),
                                    aes(x = HAB_IMAGE_WILD, y = Score, fill = Sample, color = Sample)) +
  geom_boxplot(width = 0.7, outlier.shape = NA, alpha = 0.5) +  # hide outliers for cleaner look
  scale_fill_manual(values = c("Experts" = "#00806E", "Public" = "#000000")) +
  scale_color_manual(values = c("Experts" = "#00806E", "Public" = "#000000")) +
  facet_wrap(~ Characteristic, ncol = 4, nrow = 2) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 8)) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  labs(x = "Habitat Type", y = "Rating") +
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

