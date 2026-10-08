library(tidyverse)
library(broom)
library(ggtext)

# Load data
final_data <- read.csv("survey_data/bss.csv")

bss <- final_data[,-1]
rownames(bss) <- final_data[,1]


########## TBEP ##################

#### *-- Unfamiliar ####

PERCEPT <- bss %>%
  select(AUDIENCE, TBEP_PERCEPT_ABOUT:TBEP_PERCEPT_INTEREST) %>%
  pivot_longer(cols = -AUDIENCE, names_to = "PERCEPTION", values_to = "value") %>%
  drop_na(value) %>%
  mutate(PERCEPTION = ifelse(grepl("CRED", PERCEPTION), "Credible", 
                             ifelse(grepl("UNBIAS", PERCEPTION), "Unbiased",
                                    ifelse(grepl("TRUST", PERCEPTION), "Trustworthy",
                                           ifelse(grepl("ABOUT", PERCEPTION), "Inferable",
                                                  ifelse(grepl("INTEREST", PERCEPTION), "Interested", NA)))))) %>%
  group_by(PERCEPTION, value) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)  %>%
  mutate(PERCEPTION = factor(PERCEPTION, levels = c("Interested","Trustworthy", "Unbiased", "Credible","Inferable")),
         value = factor(value, levels = c("Strongly agree","Agree","Somewhat agree","Neither agree nor disagree",
                                          "Somewhat disagree","Disagree","Strongly disagree")))
plot_PERCEPT <- ggplot(PERCEPT, aes(x=PERCEPTION, y=pct, fill=value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(values = c("Strongly disagree" = "#983E12",
                               "Disagree" = "#D7632B",
                               "Somewhat disagree" = "#FFA67C",
                               "Neither agree nor disagree" = "#D9D9D9",
                               "Somewhat agree" = "#7CACFF",
                               "Agree" = "#466EB4",
                               "Strongly agree" = "#244276")) +
  xlab("Perception of TBEP") +
  ylab("Respondents (%)") +
  coord_flip() +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        axis.ticks.y = element_blank(),
        panel.grid  = element_blank())



DESIRE <- bss %>%
  filter(FAMILIARITY != "Yes") %>% 
  select(AUDIENCE, TBEP_DESIRE_TAC:TBEP_DESIRE_TBERF) %>%
  pivot_longer(cols = -AUDIENCE, names_to = "ACTIVITY", values_to = "value") %>%
  group_by(ACTIVITY, value) %>%
  drop_na(value) %>%
  filter(value != "I have participated in this before") %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)  %>%
  mutate(ACTIVITY = factor(ACTIVITY, levels = c("TBEP_DESIRE_TBERF","TBEP_DESIRE_BMG","TBEP_DESIRE_PSCI","TBEP_DESIRE_GAD",
                                                "TBEP_DESIRE_REC","TBEP_DESIRE_CAC","TBEP_DESIRE_HRC","TBEP_DESIRE_TAC")),
         value = factor(value, levels = c("Very interested","Somewhat interested","Not interested")))
# order by very interested
activity_order <- DESIRE %>%
  filter(value == "Very interested") %>%
  arrange(desc(pct)) %>%
  pull(ACTIVITY)
DESIRE$ACTIVITY <- factor(DESIRE$ACTIVITY, levels = rev(activity_order))

plot_DESIRE <- ggplot(DESIRE, aes(x=ACTIVITY, y=pct, fill=value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(values = c("Not interested" = "#C3D7FA",
                               "Somewhat interested" = "#6896E5",
                               "Very interested" = "#244276")) +
  scale_x_discrete(labels = c(
    "TBEP_DESIRE_TAC" = "Meetings on water quality",
    "TBEP_DESIRE_HRC" = "Meetings on habitats/wildlife",
    "TBEP_DESIRE_CAC" = "Meetings on community outreach",
    "TBEP_DESIRE_REC" = "Outdoor recreation events",
    "TBEP_DESIRE_GAD" = "Volunteer events",
    "TBEP_DESIRE_PSCI" = "Participatory science",
    "TBEP_DESIRE_BMG" = "Local grant funding (BMG)",
    "TBEP_DESIRE_TBERF" = "Large grant funding (TBERF)")) +
  xlab("TBEP Activity") +
  ylab("Respondents (%)") +
  coord_flip() +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        axis.ticks.y = element_blank(),
        panel.grid  = element_blank())




#### *-- Familiar ####

EXPOSURE <- bss %>%
  filter(FAMILIARITY == "Yes") %>% 
  select(AUDIENCE, TBEP_EXPOSURE_MEET:TBEP_EXPOSURE_OTHER) %>%
  pivot_longer(cols = -AUDIENCE, names_to = "EXPOSURE", values_to = "value") %>%
  group_by(AUDIENCE, EXPOSURE) %>%
  summarise(n = n(),
            Count = sum(value == 1, na.rm = TRUE), .groups = "drop") %>%
  mutate(pct = Count/n*100) %>%
  mutate(EXPOSURE = factor(EXPOSURE, levels = c("TBEP_EXPOSURE_OTHER","TBEP_EXPOSURE_WORD","TBEP_EXPOSURE_MEDIA","TBEP_EXPOSURE_WEB",
                                                "TBEP_EXPOSURE_EMAIL","TBEP_EXPOSURE_COLLAB","TBEP_EXPOSURE_SCIENC","TBEP_EXPOSURE_FUND",
                                                "TBEP_EXPOSURE_EVENT","TBEP_EXPOSURE_MEET")),
         AUDIENCE = factor(AUDIENCE, levels = sort(unique(AUDIENCE), decreasing = TRUE)))
plot_EXPOSURE <- ggplot(EXPOSURE, aes(x = EXPOSURE, y = pct, fill = AUDIENCE)) +
  geom_bar(position=position_dodge2(reverse=TRUE), stat="identity") +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  #ylim(0,50) + 
  scale_x_discrete(labels = c(
    "TBEP_EXPOSURE_OTHER" = "Something else",
    "TBEP_EXPOSURE_WORD" = "Word of mouth",
    "TBEP_EXPOSURE_MEDIA" = "Media coverage",
    "TBEP_EXPOSURE_WEB" = "Website",
    "TBEP_EXPOSURE_EMAIL" = "Emails",
    "TBEP_EXPOSURE_COLLAB" = "Collaborations",
    "TBEP_EXPOSURE_SCIENC" = "Research or publishing",
    "TBEP_EXPOSURE_FUND" = "Applied for funding",
    "TBEP_EXPOSURE_EVENT" = "Community events",
    "TBEP_EXPOSURE_MEET" = "Committee meetings"),
    expand = expansion(add = 0.75)) +
  facet_wrap(~AUDIENCE) + 
  labs(x = "Exposure to TBEP", y = "Respondents (%)") +
  coord_flip() +
  theme_minimal() +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = 'transparent', color = NA),
        legend.box.background = element_rect(fill = 'transparent', color = NA),
        legend.position = "none",
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        panel.grid  = element_blank())



ENGAGE <- bss %>%
  filter(FAMILIARITY == "Yes") %>% 
  select(AUDIENCE, TBEP_ENGAGE_PB:TBEP_ENGAGE_COLLAB) %>%
  pivot_longer(cols = -AUDIENCE, names_to = "ENGAGE", values_to = "value") %>%
  group_by(AUDIENCE, ENGAGE, value) %>%
  drop_na(value)%>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100) %>%
  mutate(ENGAGE = factor(ENGAGE, levels = c("TBEP_ENGAGE_COLLAB","TBEP_ENGAGE_SCIENC","TBEP_ENGAGE_BMG","TBEP_ENGAGE_TBERF",
                                            "TBEP_ENGAGE_EVENT","TBEP_ENGAGE_PSCI","TBEP_ENGAGE_GAD","TBEP_ENGAGE_RAMP",
                                            "TBEP_ENGAGE_SWG","TBEP_ENGAGE_MAC","TBEP_ENGAGE_SSS","TBEP_ENGAGE_OSS",
                                            "TBEP_ENGAGE_CAC","TBEP_ENGAGE_HRC","TBEP_ENGAGE_NMC","TBEP_ENGAGE_TAC",
                                            "TBEP_ENGAGE_MB","TBEP_ENGAGE_PB")),
         value = factor(value, levels = c("Multiple times per year","About once a year","Several times","Once or twice",
                                          "Never")),
         AUDIENCE = factor(AUDIENCE, levels = c("Public", "In-network")))
plot_ENGAGE <- ggplot(ENGAGE, aes(x=ENGAGE, y=pct, fill=value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(values = c("Multiple times per year" = "#244276",
                               "About once a year" = "#3667B7",
                               "Several times" = "#6896E5",
                               "Once or twice" = "#9EC0FB",
                               "Never" = "#C3D7FA")) +
  scale_x_discrete(labels = c(
    "TBEP_ENGAGE_PB" = "Policy Board",
    "TBEP_ENGAGE_MB" = "Management Board",
    "TBEP_ENGAGE_TAC" = "TAC meetings",
    "TBEP_ENGAGE_NMC" = "NMC meetings",
    "TBEP_ENGAGE_HRC" = "HRC meetings",
    "TBEP_ENGAGE_CAC" = "CAC meetings",
    "TBEP_ENGAGE_OSS" = "OSS meetings",
    "TBEP_ENGAGE_SSS" = "SSS meetings",
    "TBEP_ENGAGE_MAC" = "MAC meetings",
    "TBEP_ENGAGE_SWG" = "SWFL SWG meetings",
    "TBEP_ENGAGE_RAMP" = "SWFL RAMP meetings",
    "TBEP_ENGAGE_GAD" = "Volunteer events",
    "TBEP_ENGAGE_PSCI" = "Participatory science",
    "TBEP_ENGAGE_EVENT" = "Other community events",
    "TBEP_ENGAGE_TBERF" = "Applied for TBERF",
    "TBEP_ENGAGE_BMG" = "Applied for BMG",
    "TBEP_ENGAGE_SCIENC" = "Research or publishing",
    "TBEP_ENGAGE_COLLAB" = "Project collaborations"),
    expand = expansion(add = 0.75)) +
  facet_wrap(~AUDIENCE) + 
  labs(x = "Type of Engagement with TBEP", y = "Respondents (%)") +
  coord_flip() +
  theme_minimal() +
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = 'transparent', color = NA),
        legend.box.background = element_rect(fill = 'transparent', color = NA),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.ticks.x = element_line(),
        panel.grid  = element_blank())

# By Partner/Non-partner
ENGAGE <- bss %>%
  filter(FAMILIARITY == "Yes") %>% 
  drop_na(PARTNERS) %>%
  select(PARTNERS, TBEP_ENGAGE_PB:TBEP_ENGAGE_COLLAB) %>%
  pivot_longer(cols = -PARTNERS, names_to = "ENGAGE", values_to = "value") %>%
  group_by(PARTNERS, ENGAGE, value) %>%
  drop_na(value)%>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100) %>%
  mutate(ENGAGE = factor(ENGAGE, levels = c("TBEP_ENGAGE_COLLAB","TBEP_ENGAGE_SCIENC","TBEP_ENGAGE_BMG","TBEP_ENGAGE_TBERF",
                                            "TBEP_ENGAGE_EVENT","TBEP_ENGAGE_PSCI","TBEP_ENGAGE_GAD","TBEP_ENGAGE_RAMP",
                                            "TBEP_ENGAGE_SWG","TBEP_ENGAGE_MAC","TBEP_ENGAGE_SSS","TBEP_ENGAGE_OSS",
                                            "TBEP_ENGAGE_CAC","TBEP_ENGAGE_HRC","TBEP_ENGAGE_NMC","TBEP_ENGAGE_TAC",
                                            "TBEP_ENGAGE_MB","TBEP_ENGAGE_PB")),
         value = factor(value, levels = c("Multiple times per year","About once a year","Several times","Once or twice",
                                          "Never")),
         PARTNERS = factor(PARTNERS, levels = c("No", "Yes")))
plot_ENGAGE <- ggplot(ENGAGE, aes(x=ENGAGE, y=pct, fill=value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(values = c("Multiple times per year" = "#244276",
                               "About once a year" = "#3667B7",
                               "Several times" = "#6896E5",
                               "Once or twice" = "#9EC0FB",
                               "Never" = "#C3D7FA")) +
  scale_x_discrete(labels = c(
    "TBEP_ENGAGE_PB" = "Policy Board",
    "TBEP_ENGAGE_MB" = "Management Board",
    "TBEP_ENGAGE_TAC" = "TAC meetings",
    "TBEP_ENGAGE_NMC" = "NMC meetings",
    "TBEP_ENGAGE_HRC" = "HRC meetings",
    "TBEP_ENGAGE_CAC" = "CAC meetings",
    "TBEP_ENGAGE_OSS" = "OSS meetings",
    "TBEP_ENGAGE_SSS" = "SSS meetings",
    "TBEP_ENGAGE_MAC" = "MAC meetings",
    "TBEP_ENGAGE_SWG" = "SWFL SWG meetings",
    "TBEP_ENGAGE_RAMP" = "SWFL RAMP meetings",
    "TBEP_ENGAGE_GAD" = "Volunteer events",
    "TBEP_ENGAGE_PSCI" = "Participatory science",
    "TBEP_ENGAGE_EVENT" = "Other community events",
    "TBEP_ENGAGE_TBERF" = "Applied for TBERF",
    "TBEP_ENGAGE_BMG" = "Applied for BMG",
    "TBEP_ENGAGE_SCIENC" = "Research or publishing",
    "TBEP_ENGAGE_COLLAB" = "Project collaborations"),
    expand = expansion(add = 0.75)) +
  facet_wrap(~PARTNERS) + 
  labs(x = "Type of Engagement with TBEP", y = "Respondents (%)") +
  coord_flip() +
  theme_minimal() +
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = 'transparent', color = NA),
        legend.box.background = element_rect(fill = 'transparent', color = NA),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.ticks.x = element_line(),
        panel.grid  = element_blank())

# By Partner/Non-partner (Actively engaged)
ENGAGE <- bss %>%
  filter(FAMILIARITY == "Yes") %>% 
  drop_na(PARTNERS) %>%
  select(PARTNERS, TBEP_ENGAGE_PB:TBEP_ENGAGE_COLLAB) %>%
  pivot_longer(cols = -PARTNERS, names_to = "ENGAGE", values_to = "value") %>%
  group_by(PARTNERS, ENGAGE, value) %>%
  drop_na(value)%>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100) %>%
  mutate(ENGAGE = factor(ENGAGE, levels = c("TBEP_ENGAGE_COLLAB","TBEP_ENGAGE_SCIENC","TBEP_ENGAGE_BMG","TBEP_ENGAGE_TBERF",
                                            "TBEP_ENGAGE_EVENT","TBEP_ENGAGE_PSCI","TBEP_ENGAGE_GAD","TBEP_ENGAGE_RAMP",
                                            "TBEP_ENGAGE_SWG","TBEP_ENGAGE_MAC","TBEP_ENGAGE_SSS","TBEP_ENGAGE_OSS",
                                            "TBEP_ENGAGE_CAC","TBEP_ENGAGE_HRC","TBEP_ENGAGE_NMC","TBEP_ENGAGE_TAC",
                                            "TBEP_ENGAGE_MB","TBEP_ENGAGE_PB")),
         value = factor(value, levels = c("Multiple times per year","About once a year","Several times","Once or twice",
                                          "Never")),
         PARTNERS = factor(PARTNERS, levels = c("No", "Yes")))
plot_ENGAGE <- ggplot(ENGAGE, aes(x=ENGAGE, y=pct, fill=value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(values = c("Multiple times per year" = "#244276",
                               "About once a year" = "#3667B7",
                               "Several times" = "white",
                               "Once or twice" = "white",
                               "Never" = "white")) +
  scale_x_discrete(labels = c(
    "TBEP_ENGAGE_PB" = "Policy Board",
    "TBEP_ENGAGE_MB" = "Management Board",
    "TBEP_ENGAGE_TAC" = "TAC meetings",
    "TBEP_ENGAGE_NMC" = "NMC meetings",
    "TBEP_ENGAGE_HRC" = "HRC meetings",
    "TBEP_ENGAGE_CAC" = "CAC meetings",
    "TBEP_ENGAGE_OSS" = "OSS meetings",
    "TBEP_ENGAGE_SSS" = "SSS meetings",
    "TBEP_ENGAGE_MAC" = "MAC meetings",
    "TBEP_ENGAGE_SWG" = "SWFL SWG meetings",
    "TBEP_ENGAGE_RAMP" = "SWFL RAMP meetings",
    "TBEP_ENGAGE_GAD" = "Volunteer events",
    "TBEP_ENGAGE_PSCI" = "Participatory science",
    "TBEP_ENGAGE_EVENT" = "Other community events",
    "TBEP_ENGAGE_TBERF" = "Applied for TBERF",
    "TBEP_ENGAGE_BMG" = "Applied for BMG",
    "TBEP_ENGAGE_SCIENC" = "Research or publishing",
    "TBEP_ENGAGE_COLLAB" = "Project collaborations"),
    expand = expansion(add = 0.75)) +
  facet_wrap(~PARTNERS) + 
  labs(x = "Type of Engagement with TBEP", y = "Respondents (%)") +
  coord_flip() +
  theme_minimal() +
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = 'transparent', color = NA),
        legend.box.background = element_rect(fill = 'transparent', color = NA),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.ticks.x = element_line(),
        panel.grid  = element_blank())



SAT <- bss %>%
  filter(FAMILIARITY == "Yes") %>% 
  select(AUDIENCE, TBEP_SAT_MEETNG:TBEP_SAT_COLLAB) %>%
  pivot_longer(cols = starts_with("TBEP_SAT_"), names_to = "ACTIVITY", values_to = "RATING") %>%
  mutate(AUDIENCE = factor(AUDIENCE, levels = c("Public", "In-network")))
SATmeans <- SAT %>%
  group_by(AUDIENCE, ACTIVITY) %>%
  summarise(mean_rating = mean(RATING, na.rm = TRUE), .groups = "drop")
activity_labels <- SAT %>%
  distinct(ACTIVITY, AUDIENCE) %>%
  mutate(
    label_text = case_when(
      ACTIVITY == "TBEP_SAT_MEETNG" ~ "Meetings",
      ACTIVITY == "TBEP_SAT_SCIENC" ~ "Research, Publishing",
      ACTIVITY == "TBEP_SAT_GRANT" ~ "Grant Programs",
      ACTIVITY == "TBEP_SAT_EVENT" ~ "Community Events",
      ACTIVITY == "TBEP_SAT_COLLAB" ~ "Staff Collaborations",
      TRUE ~ ACTIVITY),
    x_pos = -Inf, y_pos = Inf) %>%
  filter(AUDIENCE == "Public")
plot_SAT <- ggplot(SAT, aes(x = RATING, fill = AUDIENCE)) +
  geom_histogram(bins = 11, color = "white", alpha = 0.5) +
  geom_point(data = SATmeans, aes(x = mean_rating, y = 0, color = AUDIENCE),
             shape = 24, size = 4, show.legend = FALSE) +
  facet_grid(ACTIVITY ~ AUDIENCE, switch = "y") +
  geom_text(
    data = activity_labels,
    aes(x = x_pos, y = y_pos, label = label_text),
    inherit.aes = FALSE,
    hjust = -0.1,  # slightly inside left
    vjust = 1.5,   # slightly below top
    fontface = "bold",
    size = 3) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  labs(x = "Satisfaction Score", y = "Respondents") +
  theme_minimal() +
  theme(strip.background = element_blank(),       # remove background
        strip.text.y.left = element_blank(),       # for left strips (ACTIVITY)
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid = element_blank())


COMM <- bss %>%
  select(AUDIENCE, TBEP_COMM_CLARITY:TBEP_COMM_FREQ) %>%
  pivot_longer(cols = -AUDIENCE, names_to = "COMM", values_to = "value") %>%
  drop_na(value) %>%
  mutate(COMM = ifelse(grepl("CLARITY", COMM), "Understandable", 
                       ifelse(grepl("DISTIL", COMM), "Well-distilled",
                              ifelse(grepl("ENGAGE", COMM), "Engaging",
                                     ifelse(grepl("FREQ", COMM), "More Frequent", NA))))) %>%
  group_by(AUDIENCE, COMM, value) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)  %>%
  mutate(COMM = factor(COMM, levels = c("More Frequent", "Engaging", "Well-distilled", "Understandable")),
         value = factor(value, levels = c("Strongly agree","Agree","Somewhat agree","Neither agree nor disagree",
                                          "Somewhat disagree","Disagree","Strongly disagree")),
         AUDIENCE = factor(AUDIENCE, levels = c("Public", "In-network")))
plot_COMM <- ggplot(COMM, aes(x=COMM, y=pct, fill=value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(values = c("Strongly disagree" = "#983E12",
                               "Disagree" = "#D7632B",
                               "Somewhat disagree" = "#FFA67C",
                               "Neither agree nor disagree" = "#D9D9D9",
                               "Somewhat agree" = "#7CACFF",
                               "Agree" = "#466EB4",
                               "Strongly agree" = "#244276")) +
  labs(x = "Perception of TBEP Communications", y = "Respondents (%)") +
  coord_flip() +
  facet_wrap(~AUDIENCE) + 
  theme_minimal() +
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = 'transparent', color = NA),
        legend.box.background = element_rect(fill = 'transparent', color = NA),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.ticks.x = element_line(),
        panel.grid  = element_blank())


RESOURCE <- bss %>%
  select(AUDIENCE, TBEP_RESOURCE_WEB:TBEP_RESOURCE_LIB) %>%
  pivot_longer(cols = -AUDIENCE, names_to = "RESOURCE", values_to = "value") %>%
  drop_na(value) %>%
  mutate(RESOURCE = ifelse(grepl("WEB", RESOURCE), "Easy website navigation", 
                           ifelse(grepl("OS", RESOURCE), "Useful open science products",
                                  ifelse(grepl("EDU", RESOURCE), "Useful educational products",
                                         ifelse(grepl("BCC", RESOURCE), "Useful campaign materials",
                                                ifelse(grepl("LIB", RESOURCE), "Useful digital reference library", NA)))))) %>%
  group_by(AUDIENCE, RESOURCE, value) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)  %>%
  mutate(RESOURCE = factor(RESOURCE, levels = c("Useful digital reference library", "Useful campaign materials", 
                                                "Useful educational products", "Useful open science products", 
                                                "Easy website navigation")),
         value = factor(value, levels = c("Strongly agree","Agree","Somewhat agree","Neither agree nor disagree",
                                          "Somewhat disagree","Disagree","Strongly disagree")),
         AUDIENCE = factor(AUDIENCE, levels = c("Public", "In-network")))
plot_RESOURCE <- ggplot(RESOURCE, aes(x=RESOURCE, y=pct, fill=value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(values = c("Strongly disagree" = "#983E12",
                               "Disagree" = "#D7632B",
                               "Somewhat disagree" = "#FFA67C",
                               "Neither agree nor disagree" = "#D9D9D9",
                               "Somewhat agree" = "#7CACFF",
                               "Agree" = "#466EB4",
                               "Strongly agree" = "#244276")) +
  labs(x = "Perception of TBEP Resources", y = "Respondents (%)") +
  coord_flip() +
  facet_wrap(~AUDIENCE) + 
  theme_minimal() +
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = 'transparent', color = NA),
        legend.box.background = element_rect(fill = 'transparent', color = NA),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.ticks.x = element_line(),
        panel.grid  = element_blank())

ROLE <- bss %>%
  select(AUDIENCE, TBEP_ROLE_SYNTH:TBEP_ROLE_TRUST) %>%
  pivot_longer(cols = -AUDIENCE, names_to = "ROLE", values_to = "value") %>%
  drop_na(value) %>%
  mutate(ROLE = ifelse(grepl("SYNTH", ROLE), "Excellent synthesizer", 
                       ifelse(grepl("CONVEN", ROLE), "Brings people together",
                              ifelse(grepl("POLICY", ROLE), "Creates policy impact",
                                     ifelse(grepl("CRED", ROLE), "Credible",
                                            ifelse(grepl("UNBIAS", ROLE), "Unbiased",
                                                   ifelse(grepl("TRUST", ROLE), "Trustworthy", NA))))))) %>%
  group_by(AUDIENCE, ROLE, value) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)  %>%
  mutate(ROLE = factor(ROLE, levels = c("Trustworthy", "Unbiased", "Credible", "Creates policy impact", 
                                        "Brings people together", "Excellent synthesizer")),
         value = factor(value, levels = c("Strongly agree","Agree","Somewhat agree","Neither agree nor disagree",
                                          "Somewhat disagree","Disagree","Strongly disagree")),
         AUDIENCE = factor(AUDIENCE, levels = c("Public", "In-network")))
plot_ROLE <- ggplot(ROLE, aes(x=ROLE, y=pct, fill=value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(values = c("Strongly disagree" = "#983E12",
                               "Disagree" = "#D7632B",
                               "Somewhat disagree" = "#FFA67C",
                               "Neither agree nor disagree" = "#D9D9D9",
                               "Somewhat agree" = "#7CACFF",
                               "Agree" = "#466EB4",
                               "Strongly agree" = "#244276")) +
  labs(x = "Perception of TBEP's Role", y = "Respondents (%)") +
  coord_flip() +
  facet_wrap(~AUDIENCE) + 
  theme_minimal() +
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = 'transparent', color = NA),
        legend.box.background = element_rect(fill = 'transparent', color = NA),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.ticks.x = element_line(),
        panel.grid  = element_blank())





