library(tidyverse)
library(broom)
library(ggtext)

# Load data
final_data <- read.csv("survey_data/bss.csv")

bss <- final_data[,-1]
rownames(bss) <- final_data[,1]



########## CCMP ##################


CCMP_AWARENESS <- bss %>%
  drop_na(CCMP_AWARENESS) %>% 
  group_by(AUDIENCE, CCMP_AWARENESS) %>%
  mutate(AUDIENCE = factor(AUDIENCE, 
                           levels = c("Public", "In-network")),
         CCMP_AWARENESS = factor(CCMP_AWARENESS, 
                                 levels = c("Yes", "No", "Not sure"))) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_CCMP_AWARENESS <- ggplot(CCMP_AWARENESS, aes(x = "", y = pct, fill = CCMP_AWARENESS)) +
  geom_col(color = "white") +
  scale_fill_manual(name = "Familiarity with CCMP", values = c("#02806E","#004F7E","#5C524F"),
                    labels = c("Familiar","Unfamiliar","Unsure")) +
  coord_polar(theta = "y", direction = -1) +
  facet_wrap(~AUDIENCE) + 
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 12))


CCMP_READ <- bss %>%
  drop_na(CCMP_READ) %>% 
  group_by(AUDIENCE, CCMP_READ) %>%
  mutate(AUDIENCE = factor(AUDIENCE, 
                           levels = c("Public", "In-network")),
         CCMP_READ = factor(CCMP_READ, 
                            levels = c("Yes", "No", "Not sure"))) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)
plot_CCMP_READ <- ggplot(CCMP_READ, aes(x = "", y = pct, fill = CCMP_READ)) +
  geom_col(color = "white") +
  scale_fill_manual(name = "CCMP User", values = c("#02806E","#004F7E","#5C524F"),
                    labels = c("Yes","No","Unsure")) +
  coord_polar(theta = "y", direction = -1) +
  facet_wrap(~AUDIENCE) + 
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 12))


USES <- bss %>%
  filter(CCMP_READ == "Yes") %>% 
  select(AUDIENCE, CCMP_USE_ISSUE:CCMP_USE_OTHER) %>%
  pivot_longer(cols = -AUDIENCE, names_to = "USE", values_to = "value") %>%
  group_by(AUDIENCE, USE) %>%
  summarise(n = n(),
            Count = sum(value == 1, na.rm = TRUE), .groups = "drop") %>%
  mutate(pct = Count/n*100) %>%
  mutate(USE = factor(USE, levels = c("CCMP_USE_OTHER","CCMP_USE_NONE","CCMP_USE_GRANTEXT","CCMP_USE_GRANTINT",
                                      "CCMP_USE_RESTORE","CCMP_USE_RESEARCH","CCMP_USE_HELP","CCMP_USE_SOLVE",
                                      "CCMP_USE_ACTIONS","CCMP_USE_ISSUE")),
         AUDIENCE = factor(AUDIENCE, levels = sort(unique(AUDIENCE), decreasing = TRUE)))
plot_USES <- ggplot(USES, aes(x = USE, y = pct, fill = AUDIENCE)) +
  geom_bar(position=position_dodge2(reverse=TRUE), stat="identity") +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  facet_wrap(~AUDIENCE) + 
  labs(x = "Reason(s) for Using CCMP", y = "Respondents (%)") +
  ylim(0,100) + 
  coord_flip() +
  scale_x_discrete(labels = c(
    "CCMP_USE_OTHER" = "Something else",
    "CCMP_USE_NONE" = "General interest",
    "CCMP_USE_GRANTEXT" = "External grants",
    "CCMP_USE_GRANTINT" = "TBEP grants",
    "CCMP_USE_RESTORE" = "Restoration project",
    "CCMP_USE_RESEARCH" = "Research project",
    "CCMP_USE_HELP" = "Personally help",
    "CCMP_USE_SOLVE" = "Find solutions",
    "CCMP_USE_ACTIONS" = "Learn about actions",
    "CCMP_USE_ISSUE" = "Learn about issues"),
    expand = expansion(mult = c(0.1, 0.1))) +  # <-- 10% padding top & bottom 
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


# Function to generate correlograms
corrplot2 <- function(data,
                      method = "kendall",
                      sig.level = 0.05,
                      order = "original",
                      diag = FALSE,
                      type = "upper",
                      tl.srt = 90,
                      number.font = 2,
                      number.cex = 1,
                      mar = c(0, 0, 0, 0)) {
  library(corrplot)
  data_incomplete <- data
  data <- data[complete.cases(data), ]
  mat <- cor(data, method = method)
  cor.mtest <- function(mat, method) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], method = method)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  p.mat <- cor.mtest(data, method = method)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  corrplot(mat,
           method = "color", col = col(200), outline = "white", number.font = number.font,
           mar = mar, number.cex = number.cex,
           type = type, order = order,
           addCoef.col = "black", # add correlation coefficient
           tl.col = "black", tl.srt = tl.srt, # rotation of text labels
           # combine with significance level
           p.mat = p.mat, sig.level = sig.level, insig = "blank",
           # hide correlation coefficients on the diagonal
           diag = diag
  )
}


#### *-- SW1 ####

SW1 <- bss %>%
  select(AUDIENCE, SW1_ACHIEVEMENT:SW1_IMPORTANCE) %>%
  pivot_longer(cols = -AUDIENCE, names_to = "SW1_ASPECT", values_to = "value") %>%
  drop_na(value) %>%
  group_by(AUDIENCE, SW1_ASPECT, value) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         score = ifelse(value == "Very low", 0, 
                        ifelse(value == "Low", 1, 
                               ifelse(value == "Moderate", 2, 
                                      ifelse(value == "High", 3, 
                                             ifelse(value == "Very high", 4, 
                                                    ifelse(value == "No", 0, 
                                                           ifelse(value == "Yes", 1, NA)))))))) %>%
  mutate(ACTION = sub("_.*", "", SW1_ASPECT),
         ASPECT = sub("^[^_]*_", "", SW1_ASPECT)) %>%
  mutate(SW1_ASPECT = factor(SW1_ASPECT, levels = c("SW1_IMPORTANCE","SW1_FEASIBILITY","SW1_PROGRESS","SW1_RELEVANT_GROUPS",
                                                    "SW1_RELEVANT_REGION","SW1_RELEVANT_CITY","SW1_ACHIEVEMENT")),
         value = factor(value, levels = c("Yes","Not sure","No","Very high", "High",
                                          "Moderate","Low","Very low", "Unsure")),
         AUDIENCE = factor(AUDIENCE, levels = c("Public", "In-network")))
plot_SW1 <- ggplot(SW1, aes(x=SW1_ASPECT, y=pct, fill=value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(values = c("Yes" = "#244276",
                               "No" = "#C3D7FA",
                               "Not sure" = "#D9D9D9",
                               "Very high" = "#244276",
                               "High" = "#3667B7",
                               "Moderate" = "#6896E5",
                               "Low" = "#9EC0FB",
                               "Very low" = "#C3D7FA",
                               "Unsure" = "#D9D9D9")) +
  scale_x_discrete(labels = c(
    "SW1_ACHIEVEMENT" = "Partner achievements",
    "SW1_RELEVANT_CITY" = "Local relevance",
    "SW1_RELEVANT_REGION" = "Regional relevance",
    "SW1_RELEVANT_GROUPS" = "Social relevance",
    "SW1_PROGRESS" = "Progress made",
    "SW1_FEASIBILITY" = "Future feasibility",
    "SW1_IMPORTANCE" = "Future importance")) +
  labs(x = "Components of Action SW1", y = "Respondents (%)") +
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


# Correlations

SW1cor <- bss %>%
  dplyr::select(AUDIENCE, SW1_RELEVANT_CITY, SW1_RELEVANT_REGION, SW1_RELEVANT_GROUPS, SW1_PROGRESS, SW1_IMPORTANCE) %>%
  mutate(across(.cols = SW1_RELEVANT_CITY:SW1_IMPORTANCE,
                .fns = ~ ifelse(is.na(.), NA, 
                                ifelse(. == "Very low", 0, 
                                       ifelse(. == "Low", 1, 
                                              ifelse(. == "Moderate", 2, 
                                                     ifelse(. == "High", 3, 
                                                            ifelse(. == "Very high", 4, 
                                                                   ifelse(. == "No", 0, 
                                                                          ifelse(. == "Yes", 1, NA)))))))))) %>%
  drop_na()
cor_SW1_public <- SW1cor %>%
  filter(AUDIENCE == "Public") %>%
  select(-AUDIENCE)
cor_SW1_innetwork <- SW1cor %>%
  filter(AUDIENCE == "In-network")%>%
  select(-AUDIENCE)

corrplot2(
  data = cor_SW1_public,
  method = "kendall",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75)

corrplot2(
  data = cor_SW1_innetwork,
  method = "kendall",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75)


#### *-- WW1 ####
WW1 <- bss %>%
  select(AUDIENCE, WW1_ACHIEVEMENT:WW1_IMPORTANCE) %>%
  pivot_longer(cols = -AUDIENCE, names_to = "WW1_ASPECT", values_to = "value") %>%
  drop_na(value) %>%
  group_by(AUDIENCE, WW1_ASPECT, value) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         score = ifelse(value == "Very low", 0, 
                        ifelse(value == "Low", 1, 
                               ifelse(value == "Moderate", 2, 
                                      ifelse(value == "High", 3, 
                                             ifelse(value == "Very high", 4, 
                                                    ifelse(value == "No", 0, 
                                                           ifelse(value == "Yes", 1, NA)))))))) %>%
  mutate(ACTION = sub("_.*", "", WW1_ASPECT),
         ASPECT = sub("^[^_]*_", "", WW1_ASPECT)) %>%
  mutate(WW1_ASPECT = factor(WW1_ASPECT, levels = c("WW1_IMPORTANCE","WW1_FEASIBILITY","WW1_PROGRESS","WW1_RELEVANT_GROUPS",
                                                    "WW1_RELEVANT_REGION","WW1_RELEVANT_CITY","WW1_ACHIEVEMENT")),
         value = factor(value, levels = c("Yes","Not sure","No","Very high", "High",
                                          "Moderate","Low","Very low", "Unsure")),
         AUDIENCE = factor(AUDIENCE, levels = c("Public", "In-network")))
plot_WW1 <- ggplot(WW1, aes(x=WW1_ASPECT, y=pct, fill=value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(values = c("Yes" = "#244276",
                               "No" = "#C3D7FA",
                               "Not sure" = "#D9D9D9",
                               "Very high" = "#244276",
                               "High" = "#3667B7",
                               "Moderate" = "#6896E5",
                               "Low" = "#9EC0FB",
                               "Very low" = "#C3D7FA",
                               "Unsure" = "#D9D9D9")) +
  scale_x_discrete(labels = c(
    "WW1_ACHIEVEMENT" = "Partner achievements",
    "WW1_RELEVANT_CITY" = "Local relevance",
    "WW1_RELEVANT_REGION" = "Regional relevance",
    "WW1_RELEVANT_GROUPS" = "Social relevance",
    "WW1_PROGRESS" = "Progress made",
    "WW1_FEASIBILITY" = "Future feasibility",
    "WW1_IMPORTANCE" = "Future importance")) +
  labs(x = "Components of Action WW1", y = "Respondents (%)") +
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

# Correlations

WW1cor <- bss %>%
  dplyr::select(AUDIENCE, WW1_RELEVANT_CITY, WW1_RELEVANT_REGION, WW1_RELEVANT_GROUPS, WW1_PROGRESS, WW1_IMPORTANCE) %>%
  mutate(across(.cols = WW1_RELEVANT_CITY:WW1_IMPORTANCE,
                .fns = ~ ifelse(is.na(.), NA, 
                                ifelse(. == "Very low", 0, 
                                       ifelse(. == "Low", 1, 
                                              ifelse(. == "Moderate", 2, 
                                                     ifelse(. == "High", 3, 
                                                            ifelse(. == "Very high", 4, 
                                                                   ifelse(. == "No", 0, 
                                                                          ifelse(. == "Yes", 1, NA)))))))))) %>%
  drop_na()
cor_WW1_public <- WW1cor %>%
  filter(AUDIENCE == "Public") %>%
  select(-AUDIENCE)
cor_WW1_innetwork <- WW1cor %>%
  filter(AUDIENCE == "In-network")%>%
  select(-AUDIENCE)

corrplot2(
  data = cor_WW1_public,
  method = "kendall",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75)

corrplot2(
  data = cor_WW1_innetwork,
  method = "kendall",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75)

#### *-- COC4 ####
COC4 <- bss %>%
  select(AUDIENCE, COC4_ACHIEVEMENT:COC4_IMPORTANCE) %>%
  pivot_longer(cols = -AUDIENCE, names_to = "COC4_ASPECT", values_to = "value") %>%
  drop_na(value) %>%
  group_by(AUDIENCE, COC4_ASPECT, value) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         score = ifelse(value == "Very low", 0, 
                        ifelse(value == "Low", 1, 
                               ifelse(value == "Moderate", 2, 
                                      ifelse(value == "High", 3, 
                                             ifelse(value == "Very high", 4, 
                                                    ifelse(value == "No", 0, 
                                                           ifelse(value == "Yes", 1, NA)))))))) %>%
  mutate(ACTION = sub("_.*", "", COC4_ASPECT),
         ASPECT = sub("^[^_]*_", "", COC4_ASPECT)) %>%
  mutate(COC4_ASPECT = factor(COC4_ASPECT, levels = c("COC4_IMPORTANCE","COC4_FEASIBILITY","COC4_PROGRESS","COC4_RELEVANT_GROUPS",
                                                      "COC4_RELEVANT_REGION","COC4_RELEVANT_CITY","COC4_ACHIEVEMENT")),
         value = factor(value, levels = c("Yes","Not sure","No","Very high", "High",
                                          "Moderate","Low","Very low", "Unsure")),
         AUDIENCE = factor(AUDIENCE, levels = c("Public", "In-network")))
plot_COC4 <- ggplot(COC4, aes(x=COC4_ASPECT, y=pct, fill=value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(values = c("Yes" = "#244276",
                               "No" = "#C3D7FA",
                               "Not sure" = "#D9D9D9",
                               "Very high" = "#244276",
                               "High" = "#3667B7",
                               "Moderate" = "#6896E5",
                               "Low" = "#9EC0FB",
                               "Very low" = "#C3D7FA",
                               "Unsure" = "#D9D9D9")) +
  scale_x_discrete(labels = c(
    "COC4_ACHIEVEMENT" = "Partner achievements",
    "COC4_RELEVANT_CITY" = "Local relevance",
    "COC4_RELEVANT_REGION" = "Regional relevance",
    "COC4_RELEVANT_GROUPS" = "Social relevance",
    "COC4_PROGRESS" = "Progress made",
    "COC4_FEASIBILITY" = "Future feasibility",
    "COC4_IMPORTANCE" = "Future importance")) +
  labs(x = "Components of Action COC4", y = "Respondents (%)") +
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

# Correlations

COC4cor <- bss %>%
  dplyr::select(AUDIENCE, COC4_RELEVANT_CITY, COC4_RELEVANT_REGION, COC4_RELEVANT_GROUPS, COC4_PROGRESS, COC4_IMPORTANCE) %>%
  mutate(across(.cols = COC4_RELEVANT_CITY:COC4_IMPORTANCE,
                .fns = ~ ifelse(is.na(.), NA, 
                                ifelse(. == "Very low", 0, 
                                       ifelse(. == "Low", 1, 
                                              ifelse(. == "Moderate", 2, 
                                                     ifelse(. == "High", 3, 
                                                            ifelse(. == "Very high", 4, 
                                                                   ifelse(. == "No", 0, 
                                                                          ifelse(. == "Yes", 1, NA)))))))))) %>%
  drop_na()
cor_COC4_public <- COC4cor %>%
  filter(AUDIENCE == "Public") %>%
  select(-AUDIENCE)
cor_COC4_innetwork <- COC4cor %>%
  filter(AUDIENCE == "In-network")%>%
  select(-AUDIENCE)

corrplot2(
  data = cor_COC4_public,
  method = "kendall",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75)

corrplot2(
  data = cor_COC4_innetwork,
  method = "kendall",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75)

#### *-- PH2 ####
PH2 <- bss %>%
  select(AUDIENCE, PH2_ACHIEVEMENT:PH2_IMPORTANCE) %>%
  pivot_longer(cols = -AUDIENCE, names_to = "PH2_ASPECT", values_to = "value") %>%
  drop_na(value) %>%
  group_by(AUDIENCE, PH2_ASPECT, value) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         score = ifelse(value == "Very low", 0, 
                        ifelse(value == "Low", 1, 
                               ifelse(value == "Moderate", 2, 
                                      ifelse(value == "High", 3, 
                                             ifelse(value == "Very high", 4, 
                                                    ifelse(value == "No", 0, 
                                                           ifelse(value == "Yes", 1, NA)))))))) %>%
  mutate(ACTION = sub("_.*", "", PH2_ASPECT),
         ASPECT = sub("^[^_]*_", "", PH2_ASPECT)) %>%
  mutate(PH2_ASPECT = factor(PH2_ASPECT, levels = c("PH2_IMPORTANCE","PH2_FEASIBILITY","PH2_PROGRESS","PH2_RELEVANT_GROUPS",
                                                    "PH2_RELEVANT_REGION","PH2_RELEVANT_CITY","PH2_ACHIEVEMENT")),
         value = factor(value, levels = c("Yes","Not sure","No","Very high", "High",
                                          "Moderate","Low","Very low", "Unsure")),
         AUDIENCE = factor(AUDIENCE, levels = c("Public", "In-network")))
plot_PH2 <- ggplot(PH2, aes(x=PH2_ASPECT, y=pct, fill=value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(values = c("Yes" = "#244276",
                               "No" = "#C3D7FA",
                               "Not sure" = "#D9D9D9",
                               "Very high" = "#244276",
                               "High" = "#3667B7",
                               "Moderate" = "#6896E5",
                               "Low" = "#9EC0FB",
                               "Very low" = "#C3D7FA",
                               "Unsure" = "#D9D9D9")) +
  scale_x_discrete(labels = c(
    "PH2_ACHIEVEMENT" = "Partner achievements",
    "PH2_RELEVANT_CITY" = "Local relevance",
    "PH2_RELEVANT_REGION" = "Regional relevance",
    "PH2_RELEVANT_GROUPS" = "Social relevance",
    "PH2_PROGRESS" = "Progress made",
    "PH2_FEASIBILITY" = "Future feasibility",
    "PH2_IMPORTANCE" = "Future importance")) +
  labs(x = "Components of Action PH2", y = "Respondents (%)") +
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

# Correlations

PH2cor <- bss %>%
  dplyr::select(AUDIENCE, PH2_RELEVANT_CITY, PH2_RELEVANT_REGION, PH2_RELEVANT_GROUPS, PH2_PROGRESS, PH2_IMPORTANCE) %>%
  mutate(across(.cols = PH2_RELEVANT_CITY:PH2_IMPORTANCE,
                .fns = ~ ifelse(is.na(.), NA, 
                                ifelse(. == "Very low", 0, 
                                       ifelse(. == "Low", 1, 
                                              ifelse(. == "Moderate", 2, 
                                                     ifelse(. == "High", 3, 
                                                            ifelse(. == "Very high", 4, 
                                                                   ifelse(. == "No", 0, 
                                                                          ifelse(. == "Yes", 1, NA)))))))))) %>%
  drop_na()
cor_PH2_public <- PH2cor %>%
  filter(AUDIENCE == "Public") %>%
  select(-AUDIENCE)
cor_PH2_innetwork <- PH2cor %>%
  filter(AUDIENCE == "In-network")%>%
  select(-AUDIENCE)

corrplot2(
  data = cor_PH2_public,
  method = "kendall",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75)

corrplot2(
  data = cor_PH2_innetwork,
  method = "kendall",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75)

#### *-- BH6 ####
BH6 <- bss %>%
  select(AUDIENCE, BH6_ACHIEVEMENT:BH6_IMPORTANCE) %>%
  pivot_longer(cols = -AUDIENCE, names_to = "BH6_ASPECT", values_to = "value") %>%
  drop_na(value) %>%
  group_by(AUDIENCE, BH6_ASPECT, value) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         score = ifelse(value == "Very low", 0, 
                        ifelse(value == "Low", 1, 
                               ifelse(value == "Moderate", 2, 
                                      ifelse(value == "High", 3, 
                                             ifelse(value == "Very high", 4, 
                                                    ifelse(value == "No", 0, 
                                                           ifelse(value == "Yes", 1, NA)))))))) %>%
  mutate(ACTION = sub("_.*", "", BH6_ASPECT),
         ASPECT = sub("^[^_]*_", "", BH6_ASPECT)) %>%
  mutate(BH6_ASPECT = factor(BH6_ASPECT, levels = c("BH6_IMPORTANCE","BH6_FEASIBILITY","BH6_PROGRESS","BH6_RELEVANT_GROUPS",
                                                    "BH6_RELEVANT_REGION","BH6_RELEVANT_CITY","BH6_ACHIEVEMENT")),
         value = factor(value, levels = c("Yes","Not sure","No","Very high", "High",
                                          "Moderate","Low","Very low", "Unsure")),
         AUDIENCE = factor(AUDIENCE, levels = c("Public", "In-network")))
plot_BH6 <- ggplot(BH6, aes(x=BH6_ASPECT, y=pct, fill=value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(values = c("Yes" = "#244276",
                               "No" = "#C3D7FA",
                               "Not sure" = "#D9D9D9",
                               "Very high" = "#244276",
                               "High" = "#3667B7",
                               "Moderate" = "#6896E5",
                               "Low" = "#9EC0FB",
                               "Very low" = "#C3D7FA",
                               "Unsure" = "#D9D9D9")) +
  scale_x_discrete(labels = c(
    "BH6_ACHIEVEMENT" = "Partner achievements",
    "BH6_RELEVANT_CITY" = "Local relevance",
    "BH6_RELEVANT_REGION" = "Regional relevance",
    "BH6_RELEVANT_GROUPS" = "Social relevance",
    "BH6_PROGRESS" = "Progress made",
    "BH6_FEASIBILITY" = "Future feasibility",
    "BH6_IMPORTANCE" = "Future importance")) +
  labs(x = "Components of Action BH6", y = "Respondents (%)") +
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

# Correlations

BH6cor <- bss %>%
  dplyr::select(AUDIENCE, BH6_RELEVANT_CITY, BH6_RELEVANT_REGION, BH6_RELEVANT_GROUPS, BH6_PROGRESS, BH6_IMPORTANCE) %>%
  mutate(across(.cols = BH6_RELEVANT_CITY:BH6_IMPORTANCE,
                .fns = ~ ifelse(is.na(.), NA, 
                                ifelse(. == "Very low", 0, 
                                       ifelse(. == "Low", 1, 
                                              ifelse(. == "Moderate", 2, 
                                                     ifelse(. == "High", 3, 
                                                            ifelse(. == "Very high", 4, 
                                                                   ifelse(. == "No", 0, 
                                                                          ifelse(. == "Yes", 1, NA)))))))))) %>%
  drop_na()
cor_BH6_public <- BH6cor %>%
  filter(AUDIENCE == "Public") %>%
  select(-AUDIENCE)
cor_BH6_innetwork <- BH6cor %>%
  filter(AUDIENCE == "In-network")%>%
  select(-AUDIENCE)

corrplot2(
  data = cor_BH6_public,
  method = "kendall",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75)

corrplot2(
  data = cor_BH6_innetwork,
  method = "kendall",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75)

#### *-- BH9 ####
BH9 <- bss %>%
  select(AUDIENCE, BH9_ACHIEVEMENT:BH9_IMPORTANCE) %>%
  pivot_longer(cols = -AUDIENCE, names_to = "BH9_ASPECT", values_to = "value") %>%
  drop_na(value) %>%
  group_by(AUDIENCE, BH9_ASPECT, value) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         score = ifelse(value == "Very low", 0, 
                        ifelse(value == "Low", 1, 
                               ifelse(value == "Moderate", 2, 
                                      ifelse(value == "High", 3, 
                                             ifelse(value == "Very high", 4, 
                                                    ifelse(value == "No", 0, 
                                                           ifelse(value == "Yes", 1, NA)))))))) %>%
  mutate(ACTION = sub("_.*", "", BH9_ASPECT),
         ASPECT = sub("^[^_]*_", "", BH9_ASPECT)) %>%
  mutate(BH9_ASPECT = factor(BH9_ASPECT, levels = c("BH9_IMPORTANCE","BH9_FEASIBILITY","BH9_PROGRESS","BH9_RELEVANT_GROUPS",
                                                    "BH9_RELEVANT_REGION","BH9_RELEVANT_CITY","BH9_ACHIEVEMENT")),
         value = factor(value, levels = c("Yes","Not sure","No","Very high", "High",
                                          "Moderate","Low","Very low", "Unsure")),
         AUDIENCE = factor(AUDIENCE, levels = c("Public", "In-network")))
plot_BH9 <- ggplot(BH9, aes(x=BH9_ASPECT, y=pct, fill=value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(values = c("Yes" = "#244276",
                               "No" = "#C3D7FA",
                               "Not sure" = "#D9D9D9",
                               "Very high" = "#244276",
                               "High" = "#3667B7",
                               "Moderate" = "#6896E5",
                               "Low" = "#9EC0FB",
                               "Very low" = "#C3D7FA",
                               "Unsure" = "#D9D9D9")) +
  scale_x_discrete(labels = c(
    "BH9_ACHIEVEMENT" = "Partner achievements",
    "BH9_RELEVANT_CITY" = "Local relevance",
    "BH9_RELEVANT_REGION" = "Regional relevance",
    "BH9_RELEVANT_GROUPS" = "Social relevance",
    "BH9_PROGRESS" = "Progress made",
    "BH9_FEASIBILITY" = "Future feasibility",
    "BH9_IMPORTANCE" = "Future importance")) +
  labs(x = "Components of Action BH9", y = "Respondents (%)") +
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

# Correlations

BH9cor <- bss %>%
  dplyr::select(AUDIENCE, BH9_RELEVANT_CITY, BH9_RELEVANT_REGION, BH9_RELEVANT_GROUPS, BH9_PROGRESS, BH9_IMPORTANCE) %>%
  mutate(across(.cols = BH9_RELEVANT_CITY:BH9_IMPORTANCE,
                .fns = ~ ifelse(is.na(.), NA, 
                                ifelse(. == "Very low", 0, 
                                       ifelse(. == "Low", 1, 
                                              ifelse(. == "Moderate", 2, 
                                                     ifelse(. == "High", 3, 
                                                            ifelse(. == "Very high", 4, 
                                                                   ifelse(. == "No", 0, 
                                                                          ifelse(. == "Yes", 1, NA)))))))))) %>%
  drop_na()
cor_BH9_public <- BH9cor %>%
  filter(AUDIENCE == "Public") %>%
  select(-AUDIENCE)
cor_BH9_innetwork <- BH9cor %>%
  filter(AUDIENCE == "In-network")%>%
  select(-AUDIENCE)

corrplot2(
  data = cor_BH9_public,
  method = "kendall",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75)

corrplot2(
  data = cor_BH9_innetwork,
  method = "kendall",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75)

#### *-- DR2 ####
DR2 <- bss %>%
  select(AUDIENCE, DR2_ACHIEVEMENT:DR2_IMPORTANCE) %>%
  pivot_longer(cols = -AUDIENCE, names_to = "DR2_ASPECT", values_to = "value") %>%
  drop_na(value) %>%
  group_by(AUDIENCE, DR2_ASPECT, value) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         score = ifelse(value == "Very low", 0, 
                        ifelse(value == "Low", 1, 
                               ifelse(value == "Moderate", 2, 
                                      ifelse(value == "High", 3, 
                                             ifelse(value == "Very high", 4, 
                                                    ifelse(value == "No", 0, 
                                                           ifelse(value == "Yes", 1, NA)))))))) %>%
  mutate(ACTION = sub("_.*", "", DR2_ASPECT),
         ASPECT = sub("^[^_]*_", "", DR2_ASPECT)) %>%
  mutate(DR2_ASPECT = factor(DR2_ASPECT, levels = c("DR2_IMPORTANCE","DR2_FEASIBILITY","DR2_PROGRESS","DR2_RELEVANT_GROUPS",
                                                    "DR2_RELEVANT_REGION","DR2_RELEVANT_CITY","DR2_ACHIEVEMENT")),
         value = factor(value, levels = c("Yes","Not sure","No","Very high", "High",
                                          "Moderate","Low","Very low", "Unsure")),
         AUDIENCE = factor(AUDIENCE, levels = c("Public", "In-network")))
plot_DR2 <- ggplot(DR2, aes(x=DR2_ASPECT, y=pct, fill=value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(values = c("Yes" = "#244276",
                               "No" = "#C3D7FA",
                               "Not sure" = "#D9D9D9",
                               "Very high" = "#244276",
                               "High" = "#3667B7",
                               "Moderate" = "#6896E5",
                               "Low" = "#9EC0FB",
                               "Very low" = "#C3D7FA",
                               "Unsure" = "#D9D9D9")) +
  scale_x_discrete(labels = c(
    "DR2_ACHIEVEMENT" = "Partner achievements",
    "DR2_RELEVANT_CITY" = "Local relevance",
    "DR2_RELEVANT_REGION" = "Regional relevance",
    "DR2_RELEVANT_GROUPS" = "Social relevance",
    "DR2_PROGRESS" = "Progress made",
    "DR2_FEASIBILITY" = "Future feasibility",
    "DR2_IMPORTANCE" = "Future importance")) +
  labs(x = "Components of Action DR2", y = "Respondents (%)") +
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

# Correlations

DR2cor <- bss %>%
  dplyr::select(AUDIENCE, DR2_RELEVANT_CITY, DR2_RELEVANT_REGION, DR2_RELEVANT_GROUPS, DR2_PROGRESS, DR2_IMPORTANCE) %>%
  mutate(across(.cols = DR2_RELEVANT_CITY:DR2_IMPORTANCE,
                .fns = ~ ifelse(is.na(.), NA, 
                                ifelse(. == "Very low", 0, 
                                       ifelse(. == "Low", 1, 
                                              ifelse(. == "Moderate", 2, 
                                                     ifelse(. == "High", 3, 
                                                            ifelse(. == "Very high", 4, 
                                                                   ifelse(. == "No", 0, 
                                                                          ifelse(. == "Yes", 1, NA)))))))))) %>%
  drop_na()
cor_DR2_public <- DR2cor %>%
  filter(AUDIENCE == "Public") %>%
  select(-AUDIENCE)
cor_DR2_innetwork <- DR2cor %>%
  filter(AUDIENCE == "In-network")%>%
  select(-AUDIENCE)

corrplot2(
  data = cor_DR2_public,
  method = "kendall",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75)

corrplot2(
  data = cor_DR2_innetwork,
  method = "kendall",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75)

#### *-- PE1 ####
PE1 <- bss %>%
  select(AUDIENCE, PE1_ACHIEVEMENT:PE1_IMPORTANCE) %>%
  pivot_longer(cols = -AUDIENCE, names_to = "PE1_ASPECT", values_to = "value") %>%
  drop_na(value) %>%
  group_by(AUDIENCE, PE1_ASPECT, value) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         score = ifelse(value == "Very low", 0, 
                        ifelse(value == "Low", 1, 
                               ifelse(value == "Moderate", 2, 
                                      ifelse(value == "High", 3, 
                                             ifelse(value == "Very high", 4, 
                                                    ifelse(value == "No", 0, 
                                                           ifelse(value == "Yes", 1, NA)))))))) %>%
  mutate(ACTION = sub("_.*", "", PE1_ASPECT),
         ASPECT = sub("^[^_]*_", "", PE1_ASPECT)) %>%
  mutate(PE1_ASPECT = factor(PE1_ASPECT, levels = c("PE1_IMPORTANCE","PE1_FEASIBILITY","PE1_PROGRESS","PE1_RELEVANT_GROUPS",
                                                    "PE1_RELEVANT_REGION","PE1_RELEVANT_CITY","PE1_ACHIEVEMENT")),
         value = factor(value, levels = c("Yes","Not sure","No","Very high", "High",
                                          "Moderate","Low","Very low", "Unsure")),
         AUDIENCE = factor(AUDIENCE, levels = c("Public", "In-network")))
plot_PE1 <- ggplot(PE1, aes(x=PE1_ASPECT, y=pct, fill=value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(values = c("Yes" = "#244276",
                               "No" = "#C3D7FA",
                               "Not sure" = "#D9D9D9",
                               "Very high" = "#244276",
                               "High" = "#3667B7",
                               "Moderate" = "#6896E5",
                               "Low" = "#9EC0FB",
                               "Very low" = "#C3D7FA",
                               "Unsure" = "#D9D9D9")) +
  scale_x_discrete(labels = c(
    "PE1_ACHIEVEMENT" = "Partner achievements",
    "PE1_RELEVANT_CITY" = "Local relevance",
    "PE1_RELEVANT_REGION" = "Regional relevance",
    "PE1_RELEVANT_GROUPS" = "Social relevance",
    "PE1_PROGRESS" = "Progress made",
    "PE1_FEASIBILITY" = "Future feasibility",
    "PE1_IMPORTANCE" = "Future importance")) +
  labs(x = "Components of Action PE1", y = "Respondents (%)") +
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

# Correlations

PE1cor <- bss %>%
  dplyr::select(AUDIENCE, PE1_RELEVANT_CITY, PE1_RELEVANT_REGION, PE1_RELEVANT_GROUPS, PE1_PROGRESS, PE1_IMPORTANCE) %>%
  mutate(across(.cols = PE1_RELEVANT_CITY:PE1_IMPORTANCE,
                .fns = ~ ifelse(is.na(.), NA, 
                                ifelse(. == "Very low", 0, 
                                       ifelse(. == "Low", 1, 
                                              ifelse(. == "Moderate", 2, 
                                                     ifelse(. == "High", 3, 
                                                            ifelse(. == "Very high", 4, 
                                                                   ifelse(. == "No", 0, 
                                                                          ifelse(. == "Yes", 1, NA)))))))))) %>%
  drop_na()
cor_PE1_public <- PE1cor %>%
  filter(AUDIENCE == "Public") %>%
  select(-AUDIENCE)
cor_PE1_innetwork <- PE1cor %>%
  filter(AUDIENCE == "In-network")%>%
  select(-AUDIENCE)

corrplot2(
  data = cor_PE1_public,
  method = "kendall",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75)

corrplot2(
  data = cor_PE1_innetwork,
  method = "kendall",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75)

#### *-- PA1 ####
PA1 <- bss %>%
  select(AUDIENCE, PA1_ACHIEVEMENT:PA1_IMPORTANCE) %>%
  pivot_longer(cols = -AUDIENCE, names_to = "PA1_ASPECT", values_to = "value") %>%
  drop_na(value) %>%
  group_by(AUDIENCE, PA1_ASPECT, value) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         score = ifelse(value == "Very low", 0, 
                        ifelse(value == "Low", 1, 
                               ifelse(value == "Moderate", 2, 
                                      ifelse(value == "High", 3, 
                                             ifelse(value == "Very high", 4, 
                                                    ifelse(value == "No", 0, 
                                                           ifelse(value == "Yes", 1, NA)))))))) %>%
  mutate(ACTION = sub("_.*", "", PA1_ASPECT),
         ASPECT = sub("^[^_]*_", "", PA1_ASPECT)) %>%
  mutate(PA1_ASPECT = factor(PA1_ASPECT, levels = c("PA1_IMPORTANCE","PA1_FEASIBILITY","PA1_PROGRESS","PA1_RELEVANT_GROUPS",
                                                    "PA1_RELEVANT_REGION","PA1_RELEVANT_CITY","PA1_ACHIEVEMENT")),
         value = factor(value, levels = c("Yes","Not sure","No","Very high", "High",
                                          "Moderate","Low","Very low", "Unsure")),
         AUDIENCE = factor(AUDIENCE, levels = c("Public", "In-network")))
plot_PA1 <- ggplot(PA1, aes(x=PA1_ASPECT, y=pct, fill=value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(values = c("Yes" = "#244276",
                               "No" = "#C3D7FA",
                               "Not sure" = "#D9D9D9",
                               "Very high" = "#244276",
                               "High" = "#3667B7",
                               "Moderate" = "#6896E5",
                               "Low" = "#9EC0FB",
                               "Very low" = "#C3D7FA",
                               "Unsure" = "#D9D9D9")) +
  scale_x_discrete(labels = c(
    "PA1_ACHIEVEMENT" = "Partner achievements",
    "PA1_RELEVANT_CITY" = "Local relevance",
    "PA1_RELEVANT_REGION" = "Regional relevance",
    "PA1_RELEVANT_GROUPS" = "Social relevance",
    "PA1_PROGRESS" = "Progress made",
    "PA1_FEASIBILITY" = "Future feasibility",
    "PA1_IMPORTANCE" = "Future importance")) +
  labs(x = "Components of Action PA1", y = "Respondents (%)") +
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

# Correlations

PA1cor <- bss %>%
  dplyr::select(AUDIENCE, PA1_RELEVANT_CITY, PA1_RELEVANT_REGION, PA1_RELEVANT_GROUPS, PA1_PROGRESS, PA1_IMPORTANCE) %>%
  mutate(across(.cols = PA1_RELEVANT_CITY:PA1_IMPORTANCE,
                .fns = ~ ifelse(is.na(.), NA, 
                                ifelse(. == "Very low", 0, 
                                       ifelse(. == "Low", 1, 
                                              ifelse(. == "Moderate", 2, 
                                                     ifelse(. == "High", 3, 
                                                            ifelse(. == "Very high", 4, 
                                                                   ifelse(. == "No", 0, 
                                                                          ifelse(. == "Yes", 1, NA)))))))))) %>%
  drop_na()
cor_PA1_public <- PA1cor %>%
  filter(AUDIENCE == "Public") %>%
  select(-AUDIENCE)
cor_PA1_innetwork <- PA1cor %>%
  filter(AUDIENCE == "In-network")%>%
  select(-AUDIENCE)

corrplot2(
  data = cor_PA1_public,
  method = "kendall",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75)

corrplot2(
  data = cor_PA1_innetwork,
  method = "kendall",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75)

#### *-- CC1 ####
CC1 <- bss %>%
  select(AUDIENCE, CC1_ACHIEVEMENT:CC1_IMPORTANCE) %>%
  pivot_longer(cols = -AUDIENCE, names_to = "CC1_ASPECT", values_to = "value") %>%
  drop_na(value) %>%
  group_by(AUDIENCE, CC1_ASPECT, value) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         score = ifelse(value == "Very low", 0, 
                        ifelse(value == "Low", 1, 
                               ifelse(value == "Moderate", 2, 
                                      ifelse(value == "High", 3, 
                                             ifelse(value == "Very high", 4, 
                                                    ifelse(value == "No", 0, 
                                                           ifelse(value == "Yes", 1, NA)))))))) %>%
  mutate(ACTION = sub("_.*", "", CC1_ASPECT),
         ASPECT = sub("^[^_]*_", "", CC1_ASPECT)) %>%
  mutate(CC1_ASPECT = factor(CC1_ASPECT, levels = c("CC1_IMPORTANCE","CC1_FEASIBILITY","CC1_PROGRESS","CC1_RELEVANT_GROUPS",
                                                    "CC1_RELEVANT_REGION","CC1_RELEVANT_CITY","CC1_ACHIEVEMENT")),
         value = factor(value, levels = c("Yes","Not sure","No","Very high", "High",
                                          "Moderate","Low","Very low", "Unsure")),
         AUDIENCE = factor(AUDIENCE, levels = c("Public", "In-network")))
plot_CC1 <- ggplot(CC1, aes(x=CC1_ASPECT, y=pct, fill=value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(values = c("Yes" = "#244276",
                               "No" = "#C3D7FA",
                               "Not sure" = "#D9D9D9",
                               "Very high" = "#244276",
                               "High" = "#3667B7",
                               "Moderate" = "#6896E5",
                               "Low" = "#9EC0FB",
                               "Very low" = "#C3D7FA",
                               "Unsure" = "#D9D9D9")) +
  scale_x_discrete(labels = c(
    "CC1_ACHIEVEMENT" = "Partner achievements",
    "CC1_RELEVANT_CITY" = "Local relevance",
    "CC1_RELEVANT_REGION" = "Regional relevance",
    "CC1_RELEVANT_GROUPS" = "Social relevance",
    "CC1_PROGRESS" = "Progress made",
    "CC1_FEASIBILITY" = "Future feasibility",
    "CC1_IMPORTANCE" = "Future importance")) +
  labs(x = "Components of Action CC1", y = "Respondents (%)") +
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

# Correlations

CC1cor <- bss %>%
  dplyr::select(AUDIENCE, CC1_RELEVANT_CITY, CC1_RELEVANT_REGION, CC1_RELEVANT_GROUPS, CC1_PROGRESS, CC1_IMPORTANCE) %>%
  mutate(across(.cols = CC1_RELEVANT_CITY:CC1_IMPORTANCE,
                .fns = ~ ifelse(is.na(.), NA, 
                                ifelse(. == "Very low", 0, 
                                       ifelse(. == "Low", 1, 
                                              ifelse(. == "Moderate", 2, 
                                                     ifelse(. == "High", 3, 
                                                            ifelse(. == "Very high", 4, 
                                                                   ifelse(. == "No", 0, 
                                                                          ifelse(. == "Yes", 1, NA)))))))))) %>%
  drop_na()
cor_CC1_public <- CC1cor %>%
  filter(AUDIENCE == "Public") %>%
  select(-AUDIENCE)
cor_CC1_innetwork <- CC1cor %>%
  filter(AUDIENCE == "In-network")%>%
  select(-AUDIENCE)

corrplot2(
  data = cor_CC1_public,
  method = "kendall",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75)

corrplot2(
  data = cor_CC1_innetwork,
  method = "kendall",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75)

#### *-- All (individual aspect comparisons) ####

dfs <- list(SW1,WW1,COC4,PH2,BH6,BH9,DR2,PE1,PA1,CC1)
common_cols <- Reduce(intersect, lapply(dfs, names))
result <- bind_rows(lapply(dfs, function(x) x[, common_cols])) %>%
  mutate(ACTION = factor(ACTION, levels = c("CC1","PA1","PE1","DR2","BH9","BH6","PH2","COC4","WW1","SW1")))



plot_ACHIEVEMENT <- ggplot(result %>%
                             filter(ASPECT == "ACHIEVEMENT"), 
                           aes(x=ACTION, y=pct, fill=value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(values = c("Yes" = "#244276",
                               "No" = "#C3D7FA",
                               "Not sure" = "#D9D9D9")) +
  labs(x = "Action", y = "Respondents (%)", fill = "ACHIEVEMENT") +
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

plot_FEASIBILITY <- ggplot(result %>%
                             filter(ASPECT == "FEASIBILITY"), 
                           aes(x=ACTION, y=pct, fill=value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(values = c("Very high" = "#244276",
                               "High" = "#3667B7",
                               "Moderate" = "#6896E5",
                               "Low" = "#9EC0FB",
                               "Very low" = "#C3D7FA",
                               "Unsure" = "#D9D9D9")) +
  labs(x = "Action", y = "Respondents (%)", fill = "FEASIBILITY") +
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

plot_PROGRESS <- ggplot(result %>%
                             filter(ASPECT == "PROGRESS"), 
                           aes(x=ACTION, y=pct, fill=value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(values = c("Very high" = "#244276",
                               "High" = "#3667B7",
                               "Moderate" = "#6896E5",
                               "Low" = "#9EC0FB",
                               "Very low" = "#C3D7FA",
                               "Unsure" = "#D9D9D9")) +
  labs(x = "Action", y = "Respondents (%)", fill = "PROGRESS") +
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

plot_RELEVANT_GROUPS <- ggplot(result %>%
                             filter(ASPECT == "RELEVANT_GROUPS"), 
                           aes(x=ACTION, y=pct, fill=value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(values = c("Very high" = "#244276",
                               "High" = "#3667B7",
                               "Moderate" = "#6896E5",
                               "Low" = "#9EC0FB",
                               "Very low" = "#C3D7FA",
                               "Unsure" = "#D9D9D9")) +
  labs(x = "Action", y = "Respondents (%)", fill = "RELEVANT_GROUPS") +
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

plot_RELEVANT_REGION <- ggplot(result %>%
                             filter(ASPECT == "RELEVANT_REGION"), 
                           aes(x=ACTION, y=pct, fill=value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(values = c("Very high" = "#244276",
                               "High" = "#3667B7",
                               "Moderate" = "#6896E5",
                               "Low" = "#9EC0FB",
                               "Very low" = "#C3D7FA",
                               "Unsure" = "#D9D9D9")) +
  labs(x = "Action", y = "Respondents (%)", fill = "RELEVANT_REGION") +
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

plot_RELEVANT_CITY <- ggplot(result %>%
                             filter(ASPECT == "RELEVANT_CITY"), 
                           aes(x=ACTION, y=pct, fill=value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(values = c("Very high" = "#244276",
                               "High" = "#3667B7",
                               "Moderate" = "#6896E5",
                               "Low" = "#9EC0FB",
                               "Very low" = "#C3D7FA",
                               "Unsure" = "#D9D9D9")) +
  labs(x = "Action", y = "Respondents (%)", fill = "RELEVANT_CITY") +
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

plot_IMPORTANCE <- ggplot(result %>%
                             filter(ASPECT == "IMPORTANCE"), 
                           aes(x=ACTION, y=pct, fill=value)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_col(color = "white") +
  scale_fill_manual(values = c("Very high" = "#244276",
                               "High" = "#3667B7",
                               "Moderate" = "#6896E5",
                               "Low" = "#9EC0FB",
                               "Very low" = "#C3D7FA",
                               "Unsure" = "#D9D9D9")) +
  labs(x = "Action", y = "Respondents (%)", fill = "IMPORTANCE") +
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


#### *-- Action Priorities ####

# 5 Priority Categories
ACTIONS <- bss %>%
  select(AUDIENCE, SW1_ACHIEVEMENT:CC1_IMPORTANCE) %>%
  mutate(across(.cols = SW1_ACHIEVEMENT:CC1_IMPORTANCE,
                .fns = ~ ifelse(is.na(.), NA, 
                                ifelse(. == "Very low", 0, 
                                       ifelse(. == "Low", 1, 
                                              ifelse(. == "Moderate", 2, 
                                                     ifelse(. == "High", 3, 
                                                            ifelse(. == "Very high", 4, 
                                                                   ifelse(. == "No", 0, 
                                                                          ifelse(. == "Yes", 1, NA)))))))))) %>%
  pivot_longer(cols = -AUDIENCE, names_to = "ASPECT", values_to = "score") %>%
  drop_na(score) %>%
  mutate(ACTION = sub("_.*", "", ASPECT),
         ASPECT = sub("^[^_]*_", "", ASPECT)) %>%
  group_by(AUDIENCE, ACTION, ASPECT) %>%
  summarise(avg = mean(score)) %>%
  mutate(PRIORITY5 = ifelse(ASPECT == "ACHIEVEMENT" & avg <= 0.2, "Very high",
                            ifelse(ASPECT == "ACHIEVEMENT" & avg > 0.2 & avg <= 0.4, "High",
                                   ifelse(ASPECT == "ACHIEVEMENT" & avg > 0.4 & avg <= 0.6, "Moderate",
                                          ifelse(ASPECT == "ACHIEVEMENT" & avg > 0.6 & avg <= 0.8, "Low",
                                                 ifelse(ASPECT == "ACHIEVEMENT" & avg > 0.8, "Very low",
                                                        ifelse(ASPECT == "PROGRESS" & avg <= 0.8, "Very high",
                                                               ifelse(ASPECT == "PROGRESS" & avg > 0.8 & avg <= 1.6, "High",
                                                                      ifelse(ASPECT == "PROGRESS" & avg > 1.6 & avg <= 2.4, "Moderate",
                                                                             ifelse(ASPECT == "PROGRESS" & avg > 2.4 & avg <= 3.2, "Low",
                                                                                    ifelse(ASPECT == "PROGRESS" & avg > 3.2, "Very low",
                                                                                           ifelse(avg <= 0.8, "Very low",
                                                                                                  ifelse(avg > 0.8 & avg <= 1.6, "Low",
                                                                                                         ifelse(avg > 1.6 & avg <= 2.4, "Moderate",
                                                                                                                ifelse(avg > 2.4 & avg <= 3.2, "High",
                                                                                                                       ifelse(avg > 3.2, "Very high", NA))))))))))))))),
         ASPECT = factor(ASPECT, levels = c("IMPORTANCE","FEASIBILITY","PROGRESS","RELEVANT_GROUPS",
                                            "RELEVANT_REGION","RELEVANT_CITY","ACHIEVEMENT")),
         ACTION = factor(ACTION, levels = c("SW1","WW1","COC4","PH2","BH6","BH9","DR2","PE1","PA1","CC1")),
         AUDIENCE = factor(AUDIENCE, levels = c("Public", "In-network"))) %>%
  mutate(PRIORITY5_SCALE = ifelse(ASPECT == "ACHIEVEMENT", ((avg*4)-4)*(-1),
                                  ifelse(ASPECT == "PROGRESS", (avg-4)*(-1), avg)))
ACTIONS2 <- ACTIONS %>%
  group_by(AUDIENCE, ACTION) %>%
  summarise(PRIORITY5_SCALE = mean(PRIORITY5_SCALE)) %>%
  mutate(ASPECT = "OVERALL",
         PRIORITY5 = ifelse(PRIORITY5_SCALE <= 0.8, "Very low",
                            ifelse(PRIORITY5_SCALE > 0.8 & PRIORITY5_SCALE <= 1.6, "Low",
                                   ifelse(PRIORITY5_SCALE > 1.6 & PRIORITY5_SCALE <= 2.4, "Moderate",
                                          ifelse(PRIORITY5_SCALE > 2.4 & PRIORITY5_SCALE <= 3.2, "High",
                                                 ifelse(PRIORITY5_SCALE > 3.2, "Very high", NA))))))
ACTIONS3 <- ACTIONS2 %>%
  mutate(ASPECT = " ",
         PRIORITY5 = "NA",
         PRIORITY5_SCALE = NA)
ACTIONS <- rbind(ACTIONS, ACTIONS2, ACTIONS3) %>%
  mutate(ASPECT = factor(ASPECT, levels = c("OVERALL"," ", "IMPORTANCE","FEASIBILITY","PROGRESS","RELEVANT_GROUPS",
                                            "RELEVANT_REGION","RELEVANT_CITY","ACHIEVEMENT")),
         ACTION = factor(ACTION, levels = c("SW1","WW1","COC4","PH2","BH6","BH9","DR2","PE1","PA1","CC1")),
         AUDIENCE = factor(AUDIENCE, levels = c("Public", "In-network")),
         PRIORITY5 = factor(PRIORITY5, levels = c("Very high","High","Moderate","Low","Very low","NA")))
matrix_plot <- ggplot(ACTIONS, aes(x = ACTION, y = ASPECT, fill = PRIORITY5)) +
  geom_tile(color = "white") +                     # white grid lines
  facet_wrap(~AUDIENCE) +                          # separate plot per audience
  scale_fill_manual(values = c(                     # assign colors to PRIORITY5 values
    "Very high" = "#244276",
    "High" = "#456AAB",
    "Moderate" = "#6896E5",
    "Low" = "#8AAFED",
    "Very low" = "#C3D7FA",
    "NA" = "white")) +
  scale_y_discrete(labels = c(
    "ACHIEVEMENT" = "Partner achievements",
    "RELEVANT_CITY" = "Local relevance",
    "RELEVANT_REGION" = "Regional relevance",
    "RELEVANT_GROUPS" = "Social relevance",
    "PROGRESS" = "Progress made",
    "FEASIBILITY" = "Future feasibility",
    "IMPORTANCE" = "Future importance",
    " " = " ",
    "OVERALL" = "Overall Priority")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "transparent"),
    strip.text = element_text(face = "bold"))


#### *---- Drivers of Importance ####

# Lump Manatee and Sarasota County residents together given small Sarasota sample size
bss <- bss %>%
  mutate(COUNTY = ifelse(is.na(COUNTY), NA, 
                         ifelse(COUNTY == "Sarasota", "Manatee*",
                                ifelse(COUNTY == "Manatee", "Manatee*", COUNTY)))) %>%
  mutate(COUNTY = factor(COUNTY, levels = c("Hillsborough","Manatee*","Pasco","Pinellas","Polk")))


# function to rank using CI overlap
rank_with_ci <- function(est, se, z = 1.96) {
  lower <- est - z * se
  upper <- est + z * se
  n <- length(est)
  ranks <- numeric(n)
  
  for (i in seq_len(n)) {
    # count how many are clearly higher (no CI overlap and higher estimate)
    greater <- sum(
      (est > est[i]) & 
        (lower > upper[i])  # strictly above i's CI
    )
    ranks[i] <- greater + 1
  }
  
  ranks
}


SW1_modeldata <- bss %>%
  select(AUDIENCE, COUNTY, SW1_RELEVANT_CITY:SW1_PROGRESS, SW1_IMPORTANCE) %>%
  mutate(across(.cols = SW1_RELEVANT_CITY:SW1_IMPORTANCE,
                .fns = ~ ifelse(is.na(.), NA, 
                                ifelse(. == "Very low", 0, 
                                       ifelse(. == "Low", 1, 
                                              ifelse(. == "Moderate", 2, 
                                                     ifelse(. == "High", 3, 
                                                            ifelse(. == "Very high", 4, 
                                                                   ifelse(. == "No", 0, 
                                                                          ifelse(. == "Yes", 1, NA)))))))))) %>%
  drop_na()
SW1_modeldata_public <- SW1_modeldata %>%
  filter(AUDIENCE == "Public")
SW1_modeldata_innetwork <- SW1_modeldata %>%
  filter(AUDIENCE == "In-network")

# public model
SW1_model <- lm(SW1_IMPORTANCE^2 ~ SW1_RELEVANT_CITY + SW1_RELEVANT_REGION + SW1_RELEVANT_GROUPS + SW1_PROGRESS + 
                  COUNTY, data = SW1_modeldata_public)
summary(SW1_model)
# adj R2 = 0.4268  
par(mfrow = c(2,2))
plot(SW1_model)
# square transformation makes it a little better

SW1_drivers <- tidy(SW1_model) %>%
  filter(term != "(Intercept)") %>%
  mutate(ACTION = "SW1",
         ASPECT = ifelse(grepl("SW1", term), sub("^[^_]*_", "", term), sub("COUNTY", "", term)))
SW1_ranks <- tidy(SW1_model) %>%
  slice_head(n = 5) %>%
  # separate intercept
  mutate(is_intercept = term == "(Intercept)") %>%
  # compute ranks only for non-intercept rows
  mutate(rank = if_else(
    is_intercept,
    NA_real_,
    rank_with_ci(
      estimate[!is_intercept],
      std.error[!is_intercept]
    )[match(row_number(), which(!is_intercept))]
  ),
  ) %>%
  mutate(ACTION = sub("_.*", "", term),
         ASPECT = sub("^[^_]*_", "", term)) %>%
  select(-is_intercept) %>%
  filter(term != "(Intercept)")


WW1_modeldata <- bss %>%
  select(AUDIENCE, COUNTY, WW1_RELEVANT_CITY:WW1_PROGRESS, WW1_IMPORTANCE) %>%
  mutate(across(.cols = WW1_RELEVANT_CITY:WW1_IMPORTANCE,
                .fns = ~ ifelse(is.na(.), NA, 
                                ifelse(. == "Very low", 0, 
                                       ifelse(. == "Low", 1, 
                                              ifelse(. == "Moderate", 2, 
                                                     ifelse(. == "High", 3, 
                                                            ifelse(. == "Very high", 4, 
                                                                   ifelse(. == "No", 0, 
                                                                          ifelse(. == "Yes", 1, NA)))))))))) %>%
  drop_na()
WW1_modeldata_public <- WW1_modeldata %>%
  filter(AUDIENCE == "Public")
WW1_modeldata_innetwork <- WW1_modeldata %>%
  filter(AUDIENCE == "In-network")

# public model
WW1_model <- lm(WW1_IMPORTANCE^2 ~ WW1_RELEVANT_CITY + WW1_RELEVANT_REGION + WW1_RELEVANT_GROUPS + WW1_PROGRESS +
                  COUNTY, data = WW1_modeldata_public)
summary(WW1_model)
# adj R2 = 0.4268  
par(mfrow = c(2,2))
plot(WW1_model)
# square transformation improves normalcy of residuals

WW1_drivers <- tidy(WW1_model) %>%
  filter(term != "(Intercept)") %>%
  mutate(ACTION = "WW1",
         ASPECT = ifelse(grepl("WW1", term), sub("^[^_]*_", "", term), sub("COUNTY", "", term)))
WW1_ranks <- tidy(WW1_model) %>%
  slice_head(n = 5) %>%
  # separate intercept
  mutate(is_intercept = term == "(Intercept)") %>%
  # compute ranks only for non-intercept rows
  mutate(rank = if_else(
    is_intercept,
    NA_real_,
    rank_with_ci(
      estimate[!is_intercept],
      std.error[!is_intercept]
    )[match(row_number(), which(!is_intercept))]
  ),
  ) %>%
  mutate(ACTION = sub("_.*", "", term),
         ASPECT = sub("^[^_]*_", "", term)) %>%
  select(-is_intercept) %>%
  filter(term != "(Intercept)")


COC4_modeldata <- bss %>%
  select(AUDIENCE, COUNTY, COC4_RELEVANT_CITY:COC4_PROGRESS, COC4_IMPORTANCE) %>%
  mutate(across(.cols = COC4_RELEVANT_CITY:COC4_IMPORTANCE,
                .fns = ~ ifelse(is.na(.), NA, 
                                ifelse(. == "Very low", 0, 
                                       ifelse(. == "Low", 1, 
                                              ifelse(. == "Moderate", 2, 
                                                     ifelse(. == "High", 3, 
                                                            ifelse(. == "Very high", 4, 
                                                                   ifelse(. == "No", 0, 
                                                                          ifelse(. == "Yes", 1, NA)))))))))) %>%
  drop_na()
COC4_modeldata_public <- COC4_modeldata %>%
  filter(AUDIENCE == "Public")
COC4_modeldata_innetwork <- COC4_modeldata %>%
  filter(AUDIENCE == "In-network")

# public model
COC4_model <- lm(COC4_IMPORTANCE^2 ~ COC4_RELEVANT_CITY + COC4_RELEVANT_REGION + COC4_RELEVANT_GROUPS + COC4_PROGRESS +
                   COUNTY, data = COC4_modeldata_public)
summary(COC4_model)
# adj R2 = 0.4268  
par(mfrow = c(2,2))
plot(COC4_model)
# square transformation improves normalcy of residuals

COC4_drivers <- tidy(COC4_model) %>%
  filter(term != "(Intercept)") %>%
  mutate(ACTION = "COC4",
         ASPECT = ifelse(grepl("COC4", term), sub("^[^_]*_", "", term), sub("COUNTY", "", term)))
COC4_ranks <- tidy(COC4_model) %>%
  slice_head(n = 5) %>%
  # separate intercept
  mutate(is_intercept = term == "(Intercept)") %>%
  # compute ranks only for non-intercept rows
  mutate(rank = if_else(
    is_intercept,
    NA_real_,
    rank_with_ci(
      estimate[!is_intercept],
      std.error[!is_intercept]
    )[match(row_number(), which(!is_intercept))]
  ),
  ) %>%
  mutate(ACTION = sub("_.*", "", term),
         ASPECT = sub("^[^_]*_", "", term)) %>%
  select(-is_intercept) %>%
  filter(term != "(Intercept)")


PH2_modeldata <- bss %>%
  select(AUDIENCE, COUNTY, PH2_RELEVANT_CITY:PH2_PROGRESS, PH2_IMPORTANCE) %>%
  mutate(across(.cols = PH2_RELEVANT_CITY:PH2_IMPORTANCE,
                .fns = ~ ifelse(is.na(.), NA, 
                                ifelse(. == "Very low", 0, 
                                       ifelse(. == "Low", 1, 
                                              ifelse(. == "Moderate", 2, 
                                                     ifelse(. == "High", 3, 
                                                            ifelse(. == "Very high", 4, 
                                                                   ifelse(. == "No", 0, 
                                                                          ifelse(. == "Yes", 1, NA)))))))))) %>%
  drop_na()
PH2_modeldata_public <- PH2_modeldata %>%
  filter(AUDIENCE == "Public")
PH2_modeldata_innetwork <- PH2_modeldata %>%
  filter(AUDIENCE == "In-network")

# public model
PH2_model <- lm(PH2_IMPORTANCE^2 ~ PH2_RELEVANT_CITY + PH2_RELEVANT_REGION + PH2_RELEVANT_GROUPS + PH2_PROGRESS +
                  COUNTY, data = PH2_modeldata_public)
summary(PH2_model)
# adj R2 = 0.4268  
par(mfrow = c(2,2))
plot(PH2_model)
# square transformation improves normalcy of residuals

PH2_drivers <- tidy(PH2_model) %>%
  filter(term != "(Intercept)") %>%
  mutate(ACTION = "PH2",
         ASPECT = ifelse(grepl("PH2", term), sub("^[^_]*_", "", term), sub("COUNTY", "", term)))
PH2_ranks <- tidy(PH2_model) %>%
  slice_head(n = 5) %>%
  # separate intercept
  mutate(is_intercept = term == "(Intercept)") %>%
  # compute ranks only for non-intercept rows
  mutate(rank = if_else(
    is_intercept,
    NA_real_,
    rank_with_ci(
      estimate[!is_intercept],
      std.error[!is_intercept]
    )[match(row_number(), which(!is_intercept))]
  ),
  ) %>%
  mutate(ACTION = sub("_.*", "", term),
         ASPECT = sub("^[^_]*_", "", term)) %>%
  select(-is_intercept) %>%
  filter(term != "(Intercept)")


BH6_modeldata <- bss %>%
  select(AUDIENCE, COUNTY, BH6_RELEVANT_CITY:BH6_PROGRESS, BH6_IMPORTANCE) %>%
  mutate(across(.cols = BH6_RELEVANT_CITY:BH6_IMPORTANCE,
                .fns = ~ ifelse(is.na(.), NA, 
                                ifelse(. == "Very low", 0, 
                                       ifelse(. == "Low", 1, 
                                              ifelse(. == "Moderate", 2, 
                                                     ifelse(. == "High", 3, 
                                                            ifelse(. == "Very high", 4, 
                                                                   ifelse(. == "No", 0, 
                                                                          ifelse(. == "Yes", 1, NA)))))))))) %>%
  drop_na()
BH6_modeldata_public <- BH6_modeldata %>%
  filter(AUDIENCE == "Public")
BH6_modeldata_innetwork <- BH6_modeldata %>%
  filter(AUDIENCE == "In-network")

# public model
BH6_model <- lm(BH6_IMPORTANCE^2 ~ BH6_RELEVANT_CITY + BH6_RELEVANT_REGION + BH6_RELEVANT_GROUPS + BH6_PROGRESS +
                  COUNTY, data = BH6_modeldata_public)
summary(BH6_model)
# adj R2 = 0.4268  
par(mfrow = c(2,2))
plot(BH6_model)
# square transformation improves normalcy of residuals

BH6_drivers <- tidy(BH6_model) %>%
  filter(term != "(Intercept)") %>%
  mutate(ACTION = "BH6",
         ASPECT = ifelse(grepl("BH6", term), sub("^[^_]*_", "", term), sub("COUNTY", "", term)))
BH6_ranks <- tidy(BH6_model) %>%
  slice_head(n = 5) %>%
  # separate intercept
  mutate(is_intercept = term == "(Intercept)") %>%
  # compute ranks only for non-intercept rows
  mutate(rank = if_else(
    is_intercept,
    NA_real_,
    rank_with_ci(
      estimate[!is_intercept],
      std.error[!is_intercept]
    )[match(row_number(), which(!is_intercept))]
  ),
  ) %>%
  mutate(ACTION = sub("_.*", "", term),
         ASPECT = sub("^[^_]*_", "", term)) %>%
  select(-is_intercept) %>%
  filter(term != "(Intercept)")


BH9_modeldata <- bss %>%
  select(AUDIENCE, COUNTY, BH9_RELEVANT_CITY:BH9_PROGRESS, BH9_IMPORTANCE) %>%
  mutate(across(.cols = BH9_RELEVANT_CITY:BH9_IMPORTANCE,
                .fns = ~ ifelse(is.na(.), NA, 
                                ifelse(. == "Very low", 0, 
                                       ifelse(. == "Low", 1, 
                                              ifelse(. == "Moderate", 2, 
                                                     ifelse(. == "High", 3, 
                                                            ifelse(. == "Very high", 4, 
                                                                   ifelse(. == "No", 0, 
                                                                          ifelse(. == "Yes", 1, NA)))))))))) %>%
  drop_na()
BH9_modeldata_public <- BH9_modeldata %>%
  filter(AUDIENCE == "Public")
BH9_modeldata_innetwork <- BH9_modeldata %>%
  filter(AUDIENCE == "In-network")

# public model
BH9_model <- lm(BH9_IMPORTANCE^2 ~ BH9_RELEVANT_CITY + BH9_RELEVANT_REGION + BH9_RELEVANT_GROUPS + BH9_PROGRESS +
                  COUNTY, data = BH9_modeldata_public)
summary(BH9_model)
# adj R2 = 0.4268  
par(mfrow = c(2,2))
plot(BH9_model)
# square transformation improves normalcy of residuals A BIT, still not ideal though

BH9_drivers <- tidy(BH9_model) %>%
  filter(term != "(Intercept)") %>%
  mutate(ACTION = "BH9",
         ASPECT = ifelse(grepl("BH9", term), sub("^[^_]*_", "", term), sub("COUNTY", "", term)))
BH9_ranks <- tidy(BH9_model) %>%
  slice_head(n = 5) %>%
  # separate intercept
  mutate(is_intercept = term == "(Intercept)") %>%
  # compute ranks only for non-intercept rows
  mutate(rank = if_else(
    is_intercept,
    NA_real_,
    rank_with_ci(
      estimate[!is_intercept],
      std.error[!is_intercept]
    )[match(row_number(), which(!is_intercept))]
  ),
  ) %>%
  mutate(ACTION = sub("_.*", "", term),
         ASPECT = sub("^[^_]*_", "", term)) %>%
  select(-is_intercept) %>%
  filter(term != "(Intercept)")


DR2_modeldata <- bss %>%
  select(AUDIENCE, COUNTY, DR2_RELEVANT_CITY:DR2_PROGRESS, DR2_IMPORTANCE) %>%
  mutate(across(.cols = DR2_RELEVANT_CITY:DR2_IMPORTANCE,
                .fns = ~ ifelse(is.na(.), NA, 
                                ifelse(. == "Very low", 0, 
                                       ifelse(. == "Low", 1, 
                                              ifelse(. == "Moderate", 2, 
                                                     ifelse(. == "High", 3, 
                                                            ifelse(. == "Very high", 4, 
                                                                   ifelse(. == "No", 0, 
                                                                          ifelse(. == "Yes", 1, NA)))))))))) %>%
  drop_na()
DR2_modeldata_public <- DR2_modeldata %>%
  filter(AUDIENCE == "Public")
DR2_modeldata_innetwork <- DR2_modeldata %>%
  filter(AUDIENCE == "In-network")

# public model
DR2_model <- lm(DR2_IMPORTANCE^2 ~ DR2_RELEVANT_CITY + DR2_RELEVANT_REGION + DR2_RELEVANT_GROUPS + DR2_PROGRESS +
                  COUNTY, data = DR2_modeldata_public)
summary(DR2_model)
# adj R2 = 0.4268  
par(mfrow = c(2,2))
plot(DR2_model)
# square transformation improves normalcy of residuals

DR2_drivers <- tidy(DR2_model) %>%
  filter(term != "(Intercept)") %>%
  mutate(ACTION = "DR2",
         ASPECT = ifelse(grepl("DR2", term), sub("^[^_]*_", "", term), sub("COUNTY", "", term)))
DR2_ranks <- tidy(DR2_model) %>%
  slice_head(n = 5) %>%
  # separate intercept
  mutate(is_intercept = term == "(Intercept)") %>%
  # compute ranks only for non-intercept rows
  mutate(rank = if_else(
    is_intercept,
    NA_real_,
    rank_with_ci(
      estimate[!is_intercept],
      std.error[!is_intercept]
    )[match(row_number(), which(!is_intercept))]
  ),
  ) %>%
  mutate(ACTION = sub("_.*", "", term),
         ASPECT = sub("^[^_]*_", "", term)) %>%
  select(-is_intercept) %>%
  filter(term != "(Intercept)")


PE1_modeldata <- bss %>%
  select(AUDIENCE, COUNTY, PE1_RELEVANT_CITY:PE1_PROGRESS, PE1_IMPORTANCE) %>%
  mutate(across(.cols = PE1_RELEVANT_CITY:PE1_IMPORTANCE,
                .fns = ~ ifelse(is.na(.), NA, 
                                ifelse(. == "Very low", 0, 
                                       ifelse(. == "Low", 1, 
                                              ifelse(. == "Moderate", 2, 
                                                     ifelse(. == "High", 3, 
                                                            ifelse(. == "Very high", 4, 
                                                                   ifelse(. == "No", 0, 
                                                                          ifelse(. == "Yes", 1, NA)))))))))) %>%
  drop_na()
PE1_modeldata_public <- PE1_modeldata %>%
  filter(AUDIENCE == "Public")
PE1_modeldata_innetwork <- PE1_modeldata %>%
  filter(AUDIENCE == "In-network")

# public model
PE1_model <- lm(PE1_IMPORTANCE^2 ~ PE1_RELEVANT_CITY + PE1_RELEVANT_REGION + PE1_RELEVANT_GROUPS + PE1_PROGRESS +
                  COUNTY, data = PE1_modeldata_public)
summary(PE1_model)
# adj R2 = 0.4268  
par(mfrow = c(2,2))
plot(PE1_model)
# square transformation improves normalcy of residuals

PE1_drivers <- tidy(PE1_model) %>%
  filter(term != "(Intercept)") %>%
  mutate(ACTION = "PE1",
         ASPECT = ifelse(grepl("PE1", term), sub("^[^_]*_", "", term), sub("COUNTY", "", term)))
PE1_ranks <- tidy(PE1_model) %>%
  slice_head(n = 5) %>%
  # separate intercept
  mutate(is_intercept = term == "(Intercept)") %>%
  # compute ranks only for non-intercept rows
  mutate(rank = if_else(
    is_intercept,
    NA_real_,
    rank_with_ci(
      estimate[!is_intercept],
      std.error[!is_intercept]
    )[match(row_number(), which(!is_intercept))]
  ),
  ) %>%
  mutate(ACTION = sub("_.*", "", term),
         ASPECT = sub("^[^_]*_", "", term)) %>%
  select(-is_intercept) %>%
  filter(term != "(Intercept)")


PA1_modeldata <- bss %>%
  select(AUDIENCE, COUNTY, PA1_RELEVANT_CITY:PA1_PROGRESS, PA1_IMPORTANCE) %>%
  mutate(across(.cols = PA1_RELEVANT_CITY:PA1_IMPORTANCE,
                .fns = ~ ifelse(is.na(.), NA, 
                                ifelse(. == "Very low", 0, 
                                       ifelse(. == "Low", 1, 
                                              ifelse(. == "Moderate", 2, 
                                                     ifelse(. == "High", 3, 
                                                            ifelse(. == "Very high", 4, 
                                                                   ifelse(. == "No", 0, 
                                                                          ifelse(. == "Yes", 1, NA)))))))))) %>%
  drop_na()
PA1_modeldata_public <- PA1_modeldata %>%
  filter(AUDIENCE == "Public")
PA1_modeldata_innetwork <- PA1_modeldata %>%
  filter(AUDIENCE == "In-network")

# public model
PA1_model <- lm(PA1_IMPORTANCE^2 ~ PA1_RELEVANT_CITY + PA1_RELEVANT_REGION + PA1_RELEVANT_GROUPS + PA1_PROGRESS +
                  COUNTY, data = PA1_modeldata_public)
summary(PA1_model)
# adj R2 = 0.4268  
par(mfrow = c(2,2))
plot(PA1_model)
# square transformation improves normalcy of residuals

PA1_drivers <- tidy(PA1_model) %>%
  filter(term != "(Intercept)") %>%
  mutate(ACTION = "PA1",
         ASPECT = ifelse(grepl("PA1", term), sub("^[^_]*_", "", term), sub("COUNTY", "", term)))
PA1_ranks <- tidy(PA1_model) %>%
  slice_head(n = 5) %>%
  # separate intercept
  mutate(is_intercept = term == "(Intercept)") %>%
  # compute ranks only for non-intercept rows
  mutate(rank = if_else(
    is_intercept,
    NA_real_,
    rank_with_ci(
      estimate[!is_intercept],
      std.error[!is_intercept]
    )[match(row_number(), which(!is_intercept))]
  ),
  ) %>%
  mutate(ACTION = sub("_.*", "", term),
         ASPECT = sub("^[^_]*_", "", term)) %>%
  select(-is_intercept) %>%
  filter(term != "(Intercept)")


CC1_modeldata <- bss %>%
  select(AUDIENCE, COUNTY, CC1_RELEVANT_CITY:CC1_PROGRESS, CC1_IMPORTANCE) %>%
  mutate(across(.cols = CC1_RELEVANT_CITY:CC1_IMPORTANCE,
                .fns = ~ ifelse(is.na(.), NA, 
                                ifelse(. == "Very low", 0, 
                                       ifelse(. == "Low", 1, 
                                              ifelse(. == "Moderate", 2, 
                                                     ifelse(. == "High", 3, 
                                                            ifelse(. == "Very high", 4, 
                                                                   ifelse(. == "No", 0, 
                                                                          ifelse(. == "Yes", 1, NA)))))))))) %>%
  drop_na()
CC1_modeldata_public <- CC1_modeldata %>%
  filter(AUDIENCE == "Public")
CC1_modeldata_innetwork <- CC1_modeldata %>%
  filter(AUDIENCE == "In-network")

# public model
CC1_model <- lm(CC1_IMPORTANCE^2 ~ CC1_RELEVANT_CITY + CC1_RELEVANT_REGION + CC1_RELEVANT_GROUPS + CC1_PROGRESS +
                  COUNTY, data = CC1_modeldata_public)
summary(CC1_model)
# adj R2 = 0.4268  
par(mfrow = c(2,2))
plot(CC1_model)
# square transformation improves normalcy of residuals

CC1_drivers <- tidy(CC1_model) %>%
  filter(term != "(Intercept)") %>%
  mutate(ACTION = "CC1",
         ASPECT = ifelse(grepl("CC1", term), sub("^[^_]*_", "", term), sub("COUNTY", "", term)))
CC1_ranks <- tidy(CC1_model) %>%
  slice_head(n = 5) %>%
  # separate intercept
  mutate(is_intercept = term == "(Intercept)") %>%
  # compute ranks only for non-intercept rows
  mutate(rank = if_else(
    is_intercept,
    NA_real_,
    rank_with_ci(
      estimate[!is_intercept],
      std.error[!is_intercept]
    )[match(row_number(), which(!is_intercept))]
  ),
  ) %>%
  mutate(ACTION = sub("_.*", "", term),
         ASPECT = sub("^[^_]*_", "", term)) %>%
  select(-is_intercept) %>%
  filter(term != "(Intercept)")

ACTION_drivers <- rbind(SW1_drivers,WW1_drivers,COC4_drivers,PH2_drivers,BH6_drivers,
                        BH9_drivers,DR2_drivers,PE1_drivers,PA1_drivers,CC1_drivers)

ACTION_ranks <- rbind(SW1_ranks,WW1_ranks,COC4_ranks,PH2_ranks,BH6_ranks,
                      BH9_ranks,DR2_ranks,PE1_ranks,PA1_ranks,CC1_ranks) %>%
  group_by(ASPECT) %>%
  summarise(mean_rank = mean(rank),
            sd_rank = sd(rank))


# PLOTS

ACTION_drivers <- ACTION_drivers %>%
  mutate(sig = p.value <= 0.05,
         ci_low = estimate - 1.96 * std.error,
         ci_high = estimate + 1.96 * std.error) %>%
  mutate(ACTION = factor(ACTION, levels = c("SW1","WW1","COC4","PH2","BH6",
                                            "BH9","DR2","PA1","PE1","CC1")))


by_eval <- ACTION_drivers %>%
  filter(grepl("RELEVANT", ASPECT) | ASPECT == "PROGRESS") %>%
  mutate(ASPECT = factor(ASPECT, levels = c("PROGRESS","RELEVANT_GROUPS",
                                            "RELEVANT_REGION","RELEVANT_CITY")))
by_county <- ACTION_drivers %>%
  filter(!(grepl("RELEVANT", ASPECT) | ASPECT == "PROGRESS")) %>%
  mutate(ASPECT = factor(ASPECT, levels = c("Polk","Pinellas",
                                            "Pasco","Manatee*")))


plot_by_eval <- ggplot(by_eval, aes(x = estimate, y = ASPECT, color = sig)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.2) +
  scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red"),
                     labels = c("TRUE" = "p < 0.05", "FALSE" = "p > 0.05")) +
  facet_wrap(~ ACTION, ncol = 5, nrow = 2) +
  labs(x = "Model Coefficient", y = "Predictor", color = "Significance") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  scale_y_discrete(labels = c(
    "RELEVANT_CITY" = "Local relevance",
    "RELEVANT_REGION" = "Regional relevance",
    "RELEVANT_GROUPS" = "Social relevance",
    "PROGRESS" = "Progress made")) +
  theme_minimal() +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = 'transparent', color = NA),
        legend.box.background = element_rect(fill = 'transparent', color = NA),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        panel.grid  = element_blank())

plot_by_county <- ggplot(by_county, aes(x = estimate, y = ASPECT, color = sig)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.2) +
  scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red"),
                     labels = c("TRUE" = "p < 0.05", "FALSE" = "p > 0.05")) +
  facet_wrap(~ ACTION, ncol = 5, nrow = 2) +
  labs(x = "Model Coefficient", y = "Predictor (ref: Hillsborough)", color = "Significance") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  #xlim(-2.5,2.5) +
  theme_minimal() +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = 'transparent', color = NA),
        legend.box.background = element_rect(fill = 'transparent', color = NA),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        panel.grid  = element_blank())



#### *---- Maps ####

library(sf)
library(RColorBrewer)
library(patchwork)
library(ggspatial)


zipmap <- st_read("spatial_data/tbep_zipcodes.shp") 

zipbound <- zipmap |>
  st_union() |>
  st_make_valid()


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
          fill = "white",
          color = "black",
          size = 0.1) +
  # Overlay (zipmap_merged with max_diff)
  geom_sf(data = zipmap_merged,
          aes(fill = max_diff),
          color = "black",
          size = 0.1,
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
final_plot <- wrap_plots(maps, ncol = 4) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "right")
#final_plot


