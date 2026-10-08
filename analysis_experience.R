library(tidyverse)

# Load data
final_data <- read.csv("survey_data/bss.csv")

bss <- final_data


######### EXTINCTION OF EXPERIENCE ######### 


######### *--- Data Preparation ######### 

bss_ee <- bss %>%
  # Keep only what we need
  select(ResponseId, AUDIENCE, COUNTY, ORIGIN:RACE_AGG, NATRELATED, NAT_SATISFACTION, EE_INDIRECT_ROOM_CURRENT:EE_DIRECT_WILD_PAST) %>%
  # Reclassify EE values as approximate times (in hours)
  mutate(across(starts_with("EE_"),
                ~ifelse(is.na(.x), NA,
                        ifelse(.x == "None", 0,
                               ifelse(.x == "Under 1 hr", 0.5,
                                      ifelse(.x == "1-2 hrs", 1.5,
                                             ifelse(.x == "3-4 hrs", 3.5,
                                                    ifelse(.x == "5-6 hrs", 5.5,
                                                           ifelse(.x == "7-8 hrs", 7.5,
                                                                  ifelse(.x == "9-10 hrs", 9.5,
                                                                         ifelse(.x == "11-12 hrs", 11.5, 13.5))))))))))) %>%
  # Calculate approximate number of years from each person's reference point
  mutate(YEARS = ifelse(is.na(ORIGIN), NA,
                        ifelse(ORIGIN == "Yes", AGE - 18,
                               ifelse(ORIGIN == "No" & ARRIVAL < 18, AGE - 18,
                                      ifelse(ORIGIN == "No" & ARRIVAL >= 18, AGE - ARRIVAL, NA))))) %>%
  # Round up people who have been here less than a year (0) to 1 year
  mutate(YEARS = ifelse(YEARS == 0, 1, YEARS)) %>%
  # Remove negative values (people saying they are currently younger than when they first arrived)
  mutate(YEARS = ifelse(YEARS < 0, NA, YEARS)) %>%
  # Reclassify sociodemographic variables
  mutate(COUNTY = ifelse(is.na(COUNTY), NA, 
                         ifelse(COUNTY == "Manatee", "Manatee*",
                                ifelse(COUNTY == "Sarasota", "Manatee*", COUNTY))),
         EDUCATION = ifelse(is.na(EDUCATION), NA, 
                            ifelse(EDUCATION == "Did not complete high school", "High School or less",
                                   ifelse(EDUCATION == "High school diploma or GED", "High School or less",
                                          ifelse(EDUCATION == "Vocational or Trade School degree", "Vocational degree",
                                                 ifelse(EDUCATION == "Undergraduate degree (e.g., Associate's, Bachelor's)", 
                                                        "Undergraduate degree", "Graduate degree"))))),
         WHITE = ifelse(is.na(RACE_AGG), NA,
                        ifelse(RACE_AGG == "White", 1, 0)),
         MALE = ifelse(is.na(GENDER), NA,
                       ifelse(GENDER == "Male", 1, 0))) %>%
  mutate(COUNTY = factor(COUNTY, levels = c("Hillsborough","Manatee*","Pasco","Pinellas", "Polk")),
         EDUCATION = factor(EDUCATION, levels = c("High School or less","Vocational degree","Undergraduate degree","Graduate degree")),
         ORIGIN = ifelse(is.na(ORIGIN), NA,
                         ifelse(ORIGIN == "Yes", 1, 0))) %>%
  # Remove variables no longer needed
  select(-c(ARRIVAL, DURATION, GENDER, RACE_AGG)) %>%
  drop_na() %>%
  # Remove respondents who assigned the same value to every nature experience (potential biased/invalid responses)
  rowwise() %>%
  filter(n_distinct(c_across(EE_INDIRECT_ROOM_CURRENT:EE_DIRECT_WILD_PAST)) > 1) %>%  # approx 4% dropped
  ungroup()


######### *--- Calculations ######### 

bss_ee <- bss_ee %>%
  # For each experience, calculate the change over time
  mutate(EE_INDIRECT_ROOM_CHANGE = EE_INDIRECT_ROOM_CURRENT - EE_INDIRECT_ROOM_PAST,
         EE_INDIRECT_MEDIA_CHANGE = EE_INDIRECT_MEDIA_CURRENT - EE_INDIRECT_MEDIA_PAST,
         EE_DIRECT_SPORT_CHANGE = EE_DIRECT_SPORT_CURRENT - EE_DIRECT_SPORT_PAST,
         EE_DIRECT_URBAN_CHANGE = EE_DIRECT_URBAN_CURRENT - EE_DIRECT_URBAN_PAST,
         EE_DIRECT_FARM_CHANGE = EE_DIRECT_FARM_CURRENT - EE_DIRECT_FARM_PAST,
         EE_DIRECT_WILD_CHANGE = EE_DIRECT_WILD_CURRENT - EE_DIRECT_WILD_PAST) %>%
  # For each experience, calculate the annual rate of change
  mutate(EE_INDIRECT_ROOM_RATE = EE_INDIRECT_ROOM_CHANGE/YEARS,
         EE_INDIRECT_MEDIA_RATE = EE_INDIRECT_MEDIA_CHANGE/YEARS,
         EE_DIRECT_SPORT_RATE = EE_DIRECT_SPORT_CHANGE/YEARS,
         EE_DIRECT_URBAN_RATE = EE_DIRECT_URBAN_CHANGE/YEARS,
         EE_DIRECT_FARM_RATE = EE_DIRECT_FARM_CHANGE/YEARS,
         EE_DIRECT_WILD_RATE = EE_DIRECT_WILD_CHANGE/YEARS) %>%
  # Summarize net change for all direct/indirect experiences
  mutate(EE_INDIRECT_CHANGE = EE_INDIRECT_ROOM_CHANGE + EE_INDIRECT_MEDIA_CHANGE,
         EE_DIRECT_CHANGE = EE_DIRECT_SPORT_CHANGE + EE_DIRECT_URBAN_CHANGE + EE_DIRECT_FARM_CHANGE + EE_DIRECT_WILD_CHANGE)


######### *--- Plots ######### 

######### *----- Means ######### 

bss_ee <- bss_ee %>%
  mutate(YEARSCLASS = ifelse(YEARS <= 4, "1-4",
                                ifelse(YEARS >= 5 & YEARS <= 10, "5-9",
                                       ifelse(YEARS >= 10 & YEARS <= 19, "10-19",
                                              ifelse(YEARS >= 20 & YEARS <= 29, "20-29", "30+"))))) %>%
  mutate(YEARSCLASS = factor(YEARSCLASS, levels = c("1-4", "5-9", "10-19","20-29","30+")))

df_long <- bss_ee %>%
  pivot_longer(
    cols = c(EE_INDIRECT_ROOM_CURRENT,
             EE_INDIRECT_MEDIA_CURRENT,
             EE_DIRECT_SPORT_CURRENT,
             EE_DIRECT_URBAN_CURRENT,
             EE_DIRECT_FARM_CURRENT,
             EE_DIRECT_WILD_CURRENT),
    names_to = "category",
    values_to = "value"
  )
df_long <- df_long %>%
  mutate(category = recode(category,
                           EE_INDIRECT_ROOM_CURRENT = "ROOM",
                           EE_INDIRECT_MEDIA_CURRENT = "MEDIA",
                           EE_DIRECT_SPORT_CURRENT = "SPORT",
                           EE_DIRECT_URBAN_CURRENT = "URBAN",
                           EE_DIRECT_FARM_CURRENT = "FARM",
                           EE_DIRECT_WILD_CURRENT = "WILD"),
         category = factor(category, levels = c("ROOM", "MEDIA", "SPORT", "URBAN", "FARM", "WILD")))


means_df <- df_long %>%
  group_by(category, AUDIENCE) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    .groups = "drop")


# Means
plot_EE_CURRENT_INDIRECT <- ggplot(means_df %>%
         filter(category == "ROOM" | category == "MEDIA"), 
       aes(x = category, y = mean, color = AUDIENCE)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("In-network" = "#00806E", "Public" = "#000000")) +
  scale_y_continuous(limits = c(-2.1, 12), breaks = seq(0, 12, by = 3)) +
  labs(x = "Indirect Experience", y = "Current Hours per Day") +
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

plot_EE_CURRENT_DIRECT <- ggplot(means_df %>%
         filter(category != "ROOM" & category != "MEDIA"), 
       aes(x = category, y = mean, color = AUDIENCE)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("In-network" = "#00806E", "Public" = "#000000")) +
  scale_y_continuous(limits = c(-2.1, 12), breaks = seq(0, 12, by = 3)) +
  labs(x = "Direct Experience", y = "Current Hours per Week") +
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


means_df <- df_long %>%
  group_by(category, AUDIENCE, YEARSCLASS) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    .groups = "drop")


plot_EE_CURRENT_INDIRECT_YEARS <- ggplot(means_df %>%
         filter(category == "ROOM" | category == "MEDIA"), 
       aes(x = YEARSCLASS, y = mean, color = AUDIENCE)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("In-network" = "#00806E", "Public" = "#000000")) +
  scale_y_continuous(limits = c(-1, 13.6), breaks = seq(0, 12, by = 3)) +
  labs(x = "Years in Tampa Bay", y = "Current Hours per Day") +
  facet_wrap(~ category, scales = "free_y") +
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

plot_EE_CURRENT_DIRECT_YEARS <- ggplot(means_df %>%
         filter(category != "ROOM" & category != "MEDIA"), 
       aes(x = YEARSCLASS, y = mean, color = AUDIENCE)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("In-network" = "#00806E", "Public" = "#000000")) +
  scale_y_continuous(limits = c(-2.6, 11), breaks = seq(0, 10, by = 2)) +
  labs(x = "Direct Experience", y = "Current Hours per Week") +
  facet_wrap(~ category, scales = "free_y") +
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
ggplot(df_long, aes(x = YEARSCLASS, y = value, fill = "#000000")) +
  geom_boxplot(width = 0.7, outlier.shape = NA, alpha = 0.5) +  # hide outliers for cleaner look
  scale_fill_manual(values = "#000000") +
  scale_color_manual(values = "#000000") +
  #scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  labs(x = "Years in Tampa Bay", y = "Hours of experience (current)") +
  facet_wrap(~ category, scales = "free_y") +
  theme_minimal() +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        legend.background = element_rect(fill = 'transparent', color = NA),
        legend.box.background = element_rect(fill = 'transparent', color = NA),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        panel.grid  = element_blank())




######### *----- Change vs. Time ######### 

### Gains vs Losses
change_vars <- grep("_CHANGE$", names(bss_ee), value = TRUE)
ee_change <- bss_ee %>%
  select(AUDIENCE, YEARS, YEARSCLASS, all_of(change_vars)) %>%
  pivot_longer(
    cols = all_of(change_vars),
    names_to = "experience",
    values_to = "CHANGE") %>%
  mutate(DIFFERENCE = ifelse(CHANGE == 0, "No change", ifelse(CHANGE > 0, "Gain", ifelse(CHANGE < 0, "Loss", NA))))

difference_df <- ee_change %>%
  group_by(AUDIENCE, experience, DIFFERENCE) %>%
  summarise(count = n()) %>%
  mutate(pct = count/sum(count)*100) %>%
  mutate(DIFFERENCE = factor(DIFFERENCE, levels = c("Gain","No change","Loss")))

# Pie charts
plot_EE_CHANGE_DIFFERENCES <- ggplot(difference_df, aes(x = "", y = pct, fill = DIFFERENCE)) +
  geom_col(color = "white") +
  scale_fill_manual(name = "Difference", values = c("#244276","#D9D9D9","#7CACFF")) +
  coord_polar(theta = "y", direction = -1) +
  facet_wrap(~experience + AUDIENCE, ncol = 4) + 
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 12))


## Average change for those experiencing gains/losses
rates_df <- ee_change %>%
  filter(DIFFERENCE != "No change") %>%
  group_by(experience, YEARSCLASS, DIFFERENCE) %>%
  summarise(
    mean = mean(CHANGE, na.rm = TRUE),
    sd = sd(CHANGE, na.rm = TRUE),
    .groups = "drop")


plot_rate_GAINS <- ggplot(rates_df %>%
         filter(DIFFERENCE == "Gain"), 
       aes(x = YEARSCLASS, y = mean)) +
  geom_col(width = 0.7, position = position_dodge(width = 0.7)) +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 2)) +
  labs(x = "Years in Tampa Bay", y = "Average Gain in Hours") +
  facet_wrap(~ experience, scales = "free_y") +
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

plot_rate_LOSSES <- ggplot(rates_df %>%
         filter(DIFFERENCE == "Loss"), 
       aes(x = YEARSCLASS, y = mean)) +
  geom_col(width = 0.7, position = position_dodge(width = 0.7)) +
  scale_y_continuous(limits = c(-6, 0), breaks = seq(-6, 0, by = 2)) +
  labs(x = "Years in Tampa Bay", y = "Average Loss in Hours") +
  facet_wrap(~ experience, scales = "free_y") +
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



### Trends

## Everyone 
ggplot(ee_change, aes(x = YEARS, y = CHANGE)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ experience, scales = "free_y") +
  labs(
    x = "Years",
    y = "Change (Current - Past)") +
  theme_minimal()

## Excluding those who never did and still don't

ee_ROOM <- bss_ee %>%
  filter(!(EE_INDIRECT_ROOM_CURRENT == 0 & EE_INDIRECT_ROOM_PAST == 0)) %>%
  select(ResponseId, AUDIENCE, YEARS, CHANGE = EE_INDIRECT_ROOM_CHANGE, RATE = EE_INDIRECT_ROOM_RATE) %>%
  mutate(EXPERIENCE = "ROOM")
ee_MEDIA <- bss_ee %>%
  filter(!(EE_INDIRECT_MEDIA_CURRENT == 0 & EE_INDIRECT_MEDIA_PAST == 0)) %>%
  select(ResponseId, AUDIENCE, YEARS, CHANGE = EE_INDIRECT_MEDIA_CHANGE, RATE = EE_INDIRECT_MEDIA_RATE) %>%
  mutate(EXPERIENCE = "MEDIA")
ee_SPORT <- bss_ee %>%
  filter(!(EE_DIRECT_SPORT_CURRENT == 0 & EE_DIRECT_SPORT_PAST == 0)) %>%
  select(ResponseId, AUDIENCE, YEARS, CHANGE = EE_DIRECT_SPORT_CHANGE, RATE = EE_DIRECT_SPORT_RATE) %>%
  mutate(EXPERIENCE = "SPORT")
ee_URBAN <- bss_ee %>%
  filter(!(EE_DIRECT_URBAN_CURRENT == 0 & EE_DIRECT_URBAN_PAST == 0)) %>%
  select(ResponseId, AUDIENCE, YEARS, CHANGE = EE_DIRECT_URBAN_CHANGE, RATE = EE_DIRECT_URBAN_RATE) %>%
  mutate(EXPERIENCE = "URBAN")
ee_FARM <- bss_ee %>%
  filter(!(EE_DIRECT_FARM_CURRENT == 0 & EE_DIRECT_FARM_PAST == 0)) %>%
  select(ResponseId, AUDIENCE, YEARS, CHANGE = EE_DIRECT_FARM_CHANGE, RATE = EE_DIRECT_FARM_RATE) %>%
  mutate(EXPERIENCE = "FARM")
ee_WILD <- bss_ee %>%
  filter(!(EE_DIRECT_WILD_CURRENT == 0 & EE_DIRECT_WILD_PAST == 0)) %>%
  select(ResponseId, AUDIENCE, YEARS, CHANGE = EE_DIRECT_WILD_CHANGE, RATE = EE_DIRECT_WILD_RATE) %>%
  mutate(EXPERIENCE = "WILD")
df_plot <- bind_rows(ee_ROOM, ee_MEDIA, ee_SPORT, ee_URBAN, ee_URBAN, ee_FARM, ee_WILD) %>%
  mutate(EXPERIENCE = factor(EXPERIENCE, levels = c("ROOM", "MEDIA", "SPORT", "URBAN", "FARM", "WILD"))) %>%
  as.data.frame()

plot_EE <- ggplot(df_plot, aes(x = YEARS, y = CHANGE)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ EXPERIENCE, scales = "free_y") +
  scale_y_continuous(limits = c(-15, 15), breaks = seq(-15, 15, by = 5)) +
  #ylim(-15,15) +
  labs(
    x = "Years in Tampa Bay",
    y = "Change in Hours per Week",
    title = "Change in Experience Over Time (excluding 0/0 entries)"
  ) +
  theme_minimal()



