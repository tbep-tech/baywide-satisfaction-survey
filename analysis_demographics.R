library(tidyverse)
library(broom)
library(ggtext)

# Load data
final_data <- read.csv("survey_data/bss.csv")

bss <- final_data[,-1]
rownames(bss) <- final_data[,1]


########## Sociodemographics ##################

AUDIENCE <- bss %>%
  group_by(AUDIENCE, EXPERT) %>%
  mutate(AUDIENCE = factor(AUDIENCE, 
                           levels = c("Public", "In-network")),
         EXPERT = factor(EXPERT, 
                           levels = c("No", "Habitat Restoration", "Water Quality"))) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_AUDIENCE <- ggplot(AUDIENCE, aes(x = "", y = pct, fill = EXPERT)) +
  geom_col(color = "white") +
  scale_fill_manual(name = "Expert", values = c("#5C524F","#02806E","#004F7E")) +
  coord_polar(theta = "y", direction = -1) +
  facet_wrap(~AUDIENCE) + 
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 12))

AGE <- bss %>%
  drop_na(AGE) %>%
  mutate(AGECLASS = ifelse(AGE <=24, "18-24",
                           ifelse(AGE >= 25 & AGE <=34, "25-34",
                                  ifelse(AGE >= 35 & AGE <=44, "35-44",
                                         ifelse(AGE >= 45 & AGE <=54, "45-54",
                                                ifelse(AGE >= 55 & AGE <=64, "55-64", "65 or older")))))) %>%
  group_by(AUDIENCE, AGECLASS) %>%
  mutate(AUDIENCE = factor(AUDIENCE, 
                           levels = c("Public", "In-network"))) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_AGE <- ggplot(AGE, aes(x = "", y = pct, fill = AGECLASS)) +
  geom_col(color = "white") +
  scale_fill_manual(name = "Age group", values = c("#96CEF0","#6CBAE9","#3E9CD5","#0070B3","#004D79","#002439")) +
  coord_polar(theta = "y", direction = -1) +
  facet_wrap(~AUDIENCE) + 
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 12))


GENDER <- bss %>%
  drop_na(GENDER) %>% 
  group_by(AUDIENCE, GENDER) %>%
  mutate(AUDIENCE = factor(AUDIENCE, 
                           levels = c("Public", "In-network")),
         GENDER = factor(GENDER, 
                         levels = c("Male", "Female", "Non-binary"))) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_GENDER <- ggplot(GENDER, aes(x = "", y = pct, fill = GENDER)) +
  geom_col(color = "white") +
  scale_fill_manual(name = "Gender", values = c("#02806E","#004F7E","#5C524F")) +
  coord_polar(theta = "y", direction = -1) +
  facet_wrap(~AUDIENCE) + 
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 12))


EDUCATION <- bss %>%
  drop_na(EDUCATION) %>% 
  group_by(AUDIENCE, EDUCATION) %>%
  mutate(AUDIENCE = factor(AUDIENCE, 
                           levels = c("Public", "In-network")),
         EDUCATION = factor(EDUCATION, 
                            levels = c("Did not complete high school", "High school diploma or GED", 
                                       "Vocational or Trade School degree", "Undergraduate degree (e.g., Associate's, Bachelor's)", 
                                       "Graduate degree (e.g., Master's, Doctorate)"))) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_EDUCATION <- ggplot(EDUCATION, aes(x = "", y = pct, fill = EDUCATION)) +
  geom_col(color = "white") +
  scale_fill_manual(name = "Education", values = c("#96CEF0","#3E9CD5","#0070B3","#004D79","#002439")) +
  coord_polar(theta = "y", direction = -1) +
  facet_wrap(~AUDIENCE) + 
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 12))


RACE <- bss %>%
  drop_na(RACE_AGG) %>% 
  group_by(AUDIENCE, RACE_AGG) %>%
  mutate(AUDIENCE = factor(AUDIENCE, 
                           levels = c("Public", "In-network")),
         RACE_AGG = factor(RACE_AGG, 
                           levels = c("Asian", "Black", "Hispanic", "White", "Other", "Multiple"))) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_RACE <- ggplot(RACE, aes(x = "", y = pct, fill = RACE_AGG)) +
  geom_col(color = "white") +
  scale_fill_manual(name = "Race/Ethnicity", values = c("#02806E","#004F7E","#5C524F","#DC9E00","#962C14","#000000"),
                    labels = c("Asian or Asian American","Black or African American","Hispanic or Latino/a","White or European",
                               "Other selection*","Multiple selections")) +
  coord_polar(theta = "y", direction = -1) +
  facet_wrap(~AUDIENCE) + 
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 12))


COUNTY <- bss %>%
  drop_na(COUNTY) %>% 
  group_by(AUDIENCE, COUNTY) %>%
  mutate(AUDIENCE = factor(AUDIENCE, 
                           levels = c("Public", "In-network"))) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_COUNTY <- ggplot(COUNTY, aes(x = "", y = pct, fill = COUNTY)) +
  geom_col(color = "white") +
  scale_fill_manual(name = "County", values = c("#02806E","#004F7E","#5C524F","#DC9E00","#962C14","#000000")) +
  coord_polar(theta = "y", direction = -1) +
  facet_wrap(~AUDIENCE) + 
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 12))


SEASONALITY <- bss %>%
  drop_na(SEASONALITY) %>% 
  group_by(AUDIENCE, SEASONALITY) %>%
  mutate(AUDIENCE = factor(AUDIENCE, 
                           levels = c("Public", "In-network")),
         SEASONALITY = factor(SEASONALITY, 
                              levels = c("1 - 3 months", "4 - 6 months", 
                                         "7 - 9 months", "10 - 12 months"))) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_SEASONALITY <- ggplot(SEASONALITY, aes(x = "", y = pct, fill = SEASONALITY)) +
  geom_col(color = "white") +
  scale_fill_manual(name = "Annual Residency", values = c("#96CEF0","#3E9CD5","#004D79","#002439")) +
  coord_polar(theta = "y", direction = -1) +
  facet_wrap(~AUDIENCE) + 
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 12))


SEASON_SPRING <- bss %>%
  drop_na(SEASON_SPRING) %>% 
  group_by(AUDIENCE, SEASON_SPRING) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Season = "Spring") %>%
  rename(Answer = SEASON_SPRING)
SEASON_SUMMER <- bss %>%
  drop_na(SEASON_SUMMER) %>% 
  group_by(AUDIENCE, SEASON_SUMMER) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Season = "Summer") %>%
  rename(Answer = SEASON_SUMMER)
SEASON_FALL <- bss %>%
  drop_na(SEASON_FALL) %>% 
  group_by(AUDIENCE, SEASON_FALL) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Season = "Fall") %>%
  rename(Answer = SEASON_FALL)
SEASON_WINTER <- bss %>%
  drop_na(SEASON_WINTER) %>% 
  group_by(AUDIENCE, SEASON_WINTER) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Season = "Winter") %>%
  rename(Answer = SEASON_WINTER)
SEASONS <- rbind(SEASON_SPRING, SEASON_SUMMER, SEASON_FALL, SEASON_WINTER) %>%
  mutate(Season = factor(Season, 
                         levels = c("Spring", "Summer", "Fall", "Winter")))
plot_SEASON <- ggplot(subset(SEASONS, Answer == 1), aes(x = Season, y = pct, fill = AUDIENCE)) +
  geom_bar(position=position_dodge2(reverse=TRUE), stat="identity") +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  ylim(0,100) + 
  labs(y = "Respondents (%)", fill = "Audience") +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = 'transparent', color = NA),
        legend.box.background = element_rect(fill = 'transparent', color = NA),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.ticks.x = element_blank(),
        panel.grid  = element_blank())


ORIGIN <- bss %>%
  drop_na(ORIGIN) %>% 
  group_by(AUDIENCE, ORIGIN) %>%
  mutate(AUDIENCE = factor(AUDIENCE, 
                           levels = c("Public", "In-network")),
         ORIGIN = factor(ORIGIN, 
                         levels = c("Yes", "No"))) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_ORIGIN <- ggplot(ORIGIN, aes(x = "", y = pct, fill = ORIGIN)) +
  geom_col(color = "white") +
  scale_fill_manual(name = "Lifelong Resident", values = c("#02806E","#004F7E")) +
  coord_polar(theta = "y", direction = -1) +
  facet_wrap(~AUDIENCE) + 
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 12))


DURATION <- bss %>%
  drop_na(DURATION) %>%
  mutate(DURATIONCLASS = ifelse(DURATION < 1, "Less than a year",
                                ifelse(DURATION >= 1 & DURATION <= 4, "1-4 years",
                                       ifelse(DURATION >= 5 & DURATION <= 9, "5-9 years",
                                              ifelse(DURATION >= 10 & DURATION <= 19, "10-19 years", "20 years or more"))))) %>%
  group_by(AUDIENCE, DURATIONCLASS) %>%
  mutate(AUDIENCE = factor(AUDIENCE, 
                           levels = c("Public", "In-network")),
         DURATIONCLASS = factor(DURATIONCLASS, 
                                levels = c("Less than a year", "1-4 years", 
                                           "5-9 years", "10-19 years", 
                                           "20 years or more"))) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_DURATION <- ggplot(DURATION, aes(x = "", y = pct, fill = DURATIONCLASS)) +
  geom_col(color = "white") +
  scale_fill_manual(name = "Time in Region", values = c("#96CEF0","#3E9CD5","#0070B3","#004D79","#002439")) +
  coord_polar(theta = "y", direction = -1) +
  facet_wrap(~AUDIENCE) + 
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 12))


FAMILIARITY <- bss %>%
  drop_na(FAMILIARITY) %>% 
  group_by(AUDIENCE, FAMILIARITY) %>%
  mutate(AUDIENCE = factor(AUDIENCE, 
                           levels = c("Public", "In-network")),
         FAMILIARITY = factor(FAMILIARITY, 
                              levels = c("Yes", "No", "Not sure"))) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_FAMILIARITY <- ggplot(FAMILIARITY, aes(x = "", y = pct, fill = FAMILIARITY)) +
  geom_col(color = "white") +
  scale_fill_manual(name = "Familiarity with TBEP", values = c("#02806E","#004F7E","#5C524F"),
                    labels = c("Familiar","Unfamiliar","Unsure")) +
  coord_polar(theta = "y", direction = -1) +
  facet_wrap(~AUDIENCE) + 
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 12))


PARTNERS <- bss %>%
  drop_na(PARTNERS) %>% 
  group_by(AUDIENCE, PARTNERS) %>%
  mutate(AUDIENCE = factor(AUDIENCE, 
                           levels = c("Public", "In-network")),
         PARTNERS = factor(PARTNERS, 
                           levels = c("Yes", "No"))) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_PARTNERS <- ggplot(PARTNERS, aes(x = "", y = pct, fill = PARTNERS)) +
  geom_col(color = "white") +
  scale_fill_manual(name = "Partner Employee", values = c("#02806E","#004F7E")) +
  coord_polar(theta = "y", direction = -1) +
  facet_wrap(~AUDIENCE) + 
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 12))


ORG <- bss %>%
  drop_na(ORGANIZATION) %>% 
  mutate(CityClear = ifelse(grepl("Clearwater", ORGANIZATION), 1, 0),
         CityStPete = ifelse(grepl("Peters", ORGANIZATION), 1, 0),
         CityTampa = ifelse(grepl("of Tampa", ORGANIZATION), 1, 0),
         EPCHC = ifelse(grepl("Protection Commission", ORGANIZATION), 1, 0),
         FDEP = ifelse(grepl("Department of Environmental", ORGANIZATION), 1, 0),
         FDOT = ifelse(grepl("Transportation", ORGANIZATION), 1, 0),
         FWC = ifelse(grepl("Conservation", ORGANIZATION), 1, 0),
         SeaGrant = ifelse(grepl("Grant", ORGANIZATION), 1, 0),
         Hillsborough = ifelse(grepl("(^|,)Hillsborough County(,|$)", ORGANIZATION), 1, 0),
         KAB = ifelse(grepl("Beautiful", ORGANIZATION), 1, 0),
         Manatee = ifelse(grepl("Manatee County", ORGANIZATION), 1, 0),
         Pasco = ifelse(grepl("Pasco", ORGANIZATION), 1, 0),
         Pinellas = ifelse(grepl("Pinellas", ORGANIZATION), 1, 0),
         PortManatee = ifelse(grepl("Port Manatee", ORGANIZATION), 1, 0),
         PortTBay = ifelse(grepl("Port Tampa", ORGANIZATION), 1, 0),
         SWFWMD = ifelse(grepl("District", ORGANIZATION), 1, 0),
         SunCISMA = ifelse(grepl("Suncoast", ORGANIZATION), 1, 0),
         TBRPC = ifelse(grepl("Council", ORGANIZATION), 1, 0),
         TBWatch = ifelse(grepl("Watch", ORGANIZATION), 1, 0),
         TBWater = ifelse(grepl("Bay Water", ORGANIZATION), 1, 0),
         UFIFAS = ifelse(grepl("University", ORGANIZATION), 1, 0),
         USACE = ifelse(grepl("Army", ORGANIZATION), 1, 0),
         USFWS = ifelse(grepl("Service", ORGANIZATION), 1, 0),
         USGS = ifelse(grepl("Survey", ORGANIZATION), 1, 0)) %>%
  select(AUDIENCE, CityClear:USGS)
ORG <- ORG %>%
  pivot_longer(cols = -AUDIENCE, names_to = "PARTNER", values_to = "value") %>%
  group_by(AUDIENCE, PARTNER) %>%
  summarise(Count = sum(value == 1, na.rm = TRUE), .groups = "drop") %>%
  mutate(PARTNER = factor(PARTNER, levels = sort(unique(PARTNER), decreasing = TRUE)),
         AUDIENCE = factor(AUDIENCE, levels = sort(unique(AUDIENCE), decreasing = TRUE)))
plot_ORG <- ggplot(ORG, aes(x = PARTNER, y = Count, fill = AUDIENCE)) +
  geom_bar(position=position_dodge2(reverse=TRUE), stat="identity") +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  #ylim(0,100) + 
  facet_wrap(~AUDIENCE, scales = "free_x") + 
  labs(x = "Partner Organization", y = "Respondents") +
  scale_x_discrete(labels = c(
    "CityClear" = "City of Clearwater",
    "CityStPete" = "City of St. Petersburg",
    "CityTampa" = "City of Tampa",
    "EPCHC" = "Environmental Protection Commission of Hillsborough County",
    "FDEP" = "Florida Department of Environmental Protection",
    "FDOT" = "Florida Department of Transportation",
    "FWC" = "Florida Fish and Wildlife Conservation Commission",
    "Hillsborough" = "Hillsborough County",
    "KAB" = "Keep America Beautiful (and affiliates)",
    "Manatee" = "Manatee County",
    "Pasco" = "Pasco County",
    "Pinellas" = "Pinellas County",
    "PortManatee" = "Port Manatee",
    "PortTBay" = "Port Tampa Bay",
    "SWFWMD" = "Southwest Florida Water Management District",
    "SeaGrant" = "Florida Sea Grant",
    "SunCISMA" = "Suncoast Cooperative Invasive Species Management Area",
    "TBRPC" = "Tampa Bay Regional Planning Council",
    "TBWatch" = "Tampa Bay Watch",
    "TBWater" = "Tampa Bay Water",
    "UFIFAS" = "University of Florida Institute of Food and Agricultural Sciences",
    "USACE" = "US Army Corps of Engineers",
    "USFWS" = "US Fish and Wildlife Service",
    "USGS" = "US Geological Survey"),
    expand = expansion(mult = c(0.04, 0.04))) +  # <-- 4% padding top & bottom 
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


ORG <- bss %>%
  drop_na(ORGANIZATION) %>% 
  mutate(CityClear = ifelse(grepl("Clearwater", ORGANIZATION), 1, 0),
         CityStPete = ifelse(grepl("Peters", ORGANIZATION), 1, 0),
         CityTampa = ifelse(grepl("of Tampa", ORGANIZATION), 1, 0),
         EPCHC = ifelse(grepl("Protection Commission", ORGANIZATION), 1, 0),
         FDEP = ifelse(grepl("Department of Environmental", ORGANIZATION), 1, 0),
         FDOT = ifelse(grepl("Transportation", ORGANIZATION), 1, 0),
         FWC = ifelse(grepl("Conservation", ORGANIZATION), 1, 0),
         SeaGrant = ifelse(grepl("Grant", ORGANIZATION), 1, 0),
         Hillsborough = ifelse(grepl("(^|,)Hillsborough County(,|$)", ORGANIZATION), 1, 0),
         KAB = ifelse(grepl("Beautiful", ORGANIZATION), 1, 0),
         Manatee = ifelse(grepl("Manatee County", ORGANIZATION), 1, 0),
         Pasco = ifelse(grepl("Pasco", ORGANIZATION), 1, 0),
         Pinellas = ifelse(grepl("Pinellas", ORGANIZATION), 1, 0),
         PortManatee = ifelse(grepl("Port Manatee", ORGANIZATION), 1, 0),
         PortTBay = ifelse(grepl("Port Tampa", ORGANIZATION), 1, 0),
         SWFWMD = ifelse(grepl("District", ORGANIZATION), 1, 0),
         SunCISMA = ifelse(grepl("Suncoast", ORGANIZATION), 1, 0),
         TBRPC = ifelse(grepl("Council", ORGANIZATION), 1, 0),
         TBWatch = ifelse(grepl("Watch", ORGANIZATION), 1, 0),
         TBWater = ifelse(grepl("Bay Water", ORGANIZATION), 1, 0),
         UFIFAS = ifelse(grepl("University", ORGANIZATION), 1, 0),
         USACE = ifelse(grepl("Army", ORGANIZATION), 1, 0),
         USFWS = ifelse(grepl("Service", ORGANIZATION), 1, 0),
         USGS = ifelse(grepl("Survey", ORGANIZATION), 1, 0)) %>%
  select(AUDIENCE, EXPERT, CityClear:USGS)
ORG_INNETWORK <- ORG %>%
  filter(AUDIENCE == "In-network") %>%
  select(-AUDIENCE) %>%
  pivot_longer(cols = -EXPERT, names_to = "PARTNER", values_to = "value") %>%
  group_by(EXPERT, PARTNER) %>%
  summarise(Count = sum(value == 1, na.rm = TRUE), .groups = "drop") %>%
  mutate(PARTNER = factor(PARTNER, levels = sort(unique(PARTNER), decreasing = TRUE)),
         EXPERT = factor(EXPERT, levels = c("No", "Habitat Restoration", "Water Quality")))
plot_ORG_INNETWORK <- ggplot(ORG_INNETWORK, aes(x = PARTNER, y = Count, fill = EXPERT)) +
  geom_bar(position=position_dodge2(reverse=TRUE), stat="identity") +
  scale_fill_manual(values = c("Habitat Restoration" = "#00806E",
                               "Water Quality" = "#004F7E",
                               "No" = "#000000")) +
  #ylim(0,100) + 
  facet_wrap(~EXPERT) + 
  labs(x = "Partner Organization", y = "Respondents") +
  scale_x_discrete(labels = c(
    "CityClear" = "City of Clearwater",
    "CityStPete" = "City of St. Petersburg",
    "CityTampa" = "City of Tampa",
    "EPCHC" = "Environmental Protection Commission of Hillsborough County",
    "FDEP" = "Florida Department of Environmental Protection",
    "FDOT" = "Florida Department of Transportation",
    "FWC" = "Florida Fish and Wildlife Conservation Commission",
    "Hillsborough" = "Hillsborough County",
    "KAB" = "Keep America Beautiful (and affiliates)",
    "Manatee" = "Manatee County",
    "Pasco" = "Pasco County",
    "Pinellas" = "Pinellas County",
    "PortManatee" = "Port Manatee",
    "PortTBay" = "Port Tampa Bay",
    "SWFWMD" = "Southwest Florida Water Management District",
    "SeaGrant" = "Florida Sea Grant",
    "SunCISMA" = "Suncoast Cooperative Invasive Species Management Area",
    "TBRPC" = "Tampa Bay Regional Planning Council",
    "TBWatch" = "Tampa Bay Watch",
    "TBWater" = "Tampa Bay Water",
    "UFIFAS" = "University of Florida Institute of Food and Agricultural Sciences",
    "USACE" = "US Army Corps of Engineers",
    "USFWS" = "US Fish and Wildlife Service",
    "USGS" = "US Geological Survey"),
    expand = expansion(mult = c(0.04, 0.04))) +  # <-- 4% padding top & bottom 
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


#### *---- Map ####

library(sf)
library(RColorBrewer)
library(patchwork)
library(ggspatial)


zipmap <- st_read("spatial_data/tbep_zipcodes.shp") 

zipmap <- st_read("spatial_data/tbep_zipcodes.shp") %>%
  st_transform(crs = 4326)

zipbound <- zipmap |>
  st_union() |>
  st_make_valid()


# Map sample sizes

zipcodes <- bss %>%
  select(EXPERT, ZIPCODE) %>%
  filter(EXPERT == "No") %>%
  # remove any non-numeric values in the ZIPCODE field 
  mutate(ZIPCODE = gsub("[^0-9]", "", ZIPCODE)) %>%
  drop_na() %>%
  group_by(ZIPCODE) %>%
  summarize(n = n())

zipmap_merged <- zipmap %>%
  inner_join(zipcodes, by = c("ZIP_CODE" = "ZIPCODE"))

# Get zip code extent
bbox <- st_bbox(zipmap_merged)
# Add padding
xpad <- (bbox["xmax"] - bbox["xmin"]) * 0.05
ypad <- (bbox["ymax"] - bbox["ymin"]) * 0.05

zipcode_n <- ggplot() +
  geom_sf(data = zipmap_merged,
          aes(fill = n),
          color = "black",
          alpha = 0.9) +
  coord_sf(
    xlim = c(bbox["xmin"] - xpad, bbox["xmax"] + xpad),
    ylim = c(bbox["ymin"] - ypad, bbox["ymax"] + ypad),
    expand = FALSE) +
  scale_fill_stepsn(
    colours = brewer.pal(9, "Blues"),
    breaks = c(1, 5, 10, 15, 20, 25),
    limits = c(1, 25),
    name = "Respondents") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = 'transparent', color = NA),
    plot.background = element_rect(fill = 'transparent', color = NA),
    panel.grid = element_blank(),
    panel.border = element_blank(),   # removes map border
    axis.text = element_blank(),      # removes lat/lon labels
    axis.ticks = element_blank(),     # removes tick marks
    axis.title = element_blank(),     # removes axis titleslegend.position = "right",
    legend.title = element_markdown(),
    legend.background = element_rect(fill = 'transparent', color = NA),
    legend.box.background = element_rect(fill = 'transparent', color = NA))



# Map Local Relevance

zipcodes <- bss %>%
  select(EXPERT, ZIPCODE, 
         SW1_RELEVANT_CITY, WW1_RELEVANT_CITY, COC4_RELEVANT_CITY, PH2_RELEVANT_CITY, BH6_RELEVANT_CITY, 
         BH9_RELEVANT_CITY, DR2_RELEVANT_CITY, PE1_RELEVANT_CITY, PA1_RELEVANT_CITY, CC1_RELEVANT_CITY) %>%
  filter(EXPERT == "No") %>%
  # remove any non-numeric values in the ZIPCODE field 
  mutate(ZIPCODE = gsub("[^0-9]", "", ZIPCODE)) %>%
  mutate(across(.cols = SW1_RELEVANT_CITY:CC1_RELEVANT_CITY,
                .fns = ~ ifelse(is.na(.), NA, 
                                ifelse(. == "Very low", 0, 
                                       ifelse(. == "Low", 1, 
                                              ifelse(. == "Moderate", 2, 
                                                     ifelse(. == "High", 3, 
                                                            ifelse(. == "Very high", 4, 
                                                                   ifelse(. == "No", 0, 
                                                                          ifelse(. == "Yes", 1, NA)))))))))) %>%
  drop_na() %>%
  group_by(ZIPCODE) %>%
  summarize(n = n(),
            across(contains("RELEVANT"), ~mean(.x, na.rm = TRUE)))

zipmap_merged <- zipmap %>%
  inner_join(zipcodes, by = c("ZIP_CODE" = "ZIPCODE")) %>%
  filter(n >= 5)

# Calculate maximum difference in scores across actions to see where there's more variation
zipmap_merged <- zipmap_merged %>%
  rowwise() %>%
  mutate(max_diff = max(c_across(ends_with("_CITY")), na.rm = TRUE) - min(c_across(ends_with("_CITY")), na.rm = TRUE)) %>%
  ungroup()




# Map variation in action scores (highlight those greater than 1)

local_variation <- ggplot() +
  # Base map (zipmap)
  geom_sf(data = zipmap,
          fill = "gray85",
          color = "white",
          size = 0.1) +
  # Overlay (zipmap_merged with max_diff)
  geom_sf(data = zipmap_merged,
          aes(fill = max_diff),
          color = ifelse(zipmap_merged$max_diff > 1, "black", "white"),
          size = ifelse(zipmap_merged$max_diff > 1, 0.8, 0.1),
          alpha = 0.9) +
  scale_fill_gradientn(
    colours = brewer.pal(9, "Reds"),
    limits = c(0, 2),
    name = "Maximum Variation<br><span style='font-size:10pt;'><i>across actions</i></span>") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = 'transparent', color = NA),
    plot.background = element_rect(fill = 'transparent', color = NA),
    panel.grid = element_blank(),
    panel.border = element_blank(),   # removes map border
    axis.text = element_blank(),      # removes lat/lon labels
    axis.ticks = element_blank(),     # removes tick marks
    axis.title = element_blank(),     # removes axis titleslegend.position = "right",
    legend.title = element_markdown(),
    legend.background = element_rect(fill = 'transparent', color = NA),
    legend.box.background = element_rect(fill = 'transparent', color = NA))


# Map scores for each action

make_map <- function(var, title_text) {
  ggplot() +
    # Overlay (zipmap_merged)
    geom_sf(data = zipmap_merged,
            aes(fill = .data[[var]]),
            color = NA,
            size = 0.8,
            alpha = 0.9) +
    scale_fill_gradientn(
      colours = rev(brewer.pal(11, "RdBu")),
      limits = c(0, 4),
      name = "Local Relevance<br><span style='font-size:10pt;'><i>mean score (n = 5+)</i></span>") +
    labs(title = title_text) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.background = element_rect(fill = 'transparent', color = NA),
      plot.background = element_rect(fill = 'transparent', color = NA),
      panel.grid = element_blank(),
      panel.border = element_blank(),   # removes map border
      axis.text = element_blank(),      # removes lat/lon labels
      axis.ticks = element_blank(),     # removes tick marks
      axis.title = element_blank(),     # removes axis titles
      legend.title = element_markdown(),
      legend.position = "right",
      legend.background = element_rect(fill = 'transparent', color = NA),
      legend.box.background = element_rect(fill = 'transparent', color = NA)) +
    # Base map (zipbound)
    geom_sf(data = zipbound,
            fill = NA,
            color = "black",
            size = 0.1)
}

vars <- c(
  "SW1_RELEVANT_CITY",
  "WW1_RELEVANT_CITY",
  "COC4_RELEVANT_CITY",
  "PH2_RELEVANT_CITY",
  "BH6_RELEVANT_CITY",
  "BH9_RELEVANT_CITY",
  "DR2_RELEVANT_CITY",
  "PE1_RELEVANT_CITY",
  "PA1_RELEVANT_CITY",
  "CC1_RELEVANT_CITY")
titles <- sub("_RELEVANT_CITY$", "", vars)
maps <- Map(make_map, vars, titles)
final_plot <- wrap_plots(maps, ncol = 5) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "right")
#final_plot


