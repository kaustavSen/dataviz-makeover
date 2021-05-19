library(tidyverse)
library(ggtext)
library(here)

# Source: https://www.goldmansachs.com/insights/pages/gs-research/womenomics-europe-moving-ahead/report.pdf
# Plot on page 4

data <- tribble(
  ~age_band, ~country, ~female_participation,
  "15-19", "Europe", 20,
  "15-19", "US", 36,
  "20-24", "Europe", 58,
  "20-24", "US", 70,
  "25-29", "Europe", 77,
  "25-29", "US", 78,
  "30-34", "Europe", 79,
  "30-34", "US", 76,
  "35-39", "Europe", 81,
  "35-39", "US", 75,
  "40-44", "Europe", 83,
  "40-44", "US", 76,
  "45-49", "Europe", 83,
  "45-49", "US", 77,
  "50-54", "Europe", 79,
  "50-54", "US", 74,
  "55-59", "Europe", 70,
  "55-59", "US", 67,
  "60-64", "Europe", 41,
  "60-64", "US", 52,
)

data <- data %>% 
  group_by(age_band) %>% 
  mutate(
    alpha = if_else(female_participation == max(female_participation), 1, 0.6)
  ) %>% 
  map_df(rev)

ggplot() +
  geom_text(aes(x = 0, y = 1:10, label = unique(data$age_band)),
            family = "Roboto", fontface = "bold", size = 4) +
  annotate("text", x = 0, y = 10.5, label = "Age Group",
           family = "Roboto", fontface = "bold", size = 4) +
  geom_segment(
    data = filter(data, country == "US"),
    aes(
      x = 6,
      y = 1:10,
      xend = 6 + female_participation,
      yend = 1:10,
      alpha = alpha
    ),
    lineend = "round",
    size = 9,
    color = "#3d5a80"
  ) +
  geom_text(
    data = filter(data, country == "US", age_band != "15-19"),
    aes(
      x = 4.5 + female_participation,
      y = 1:9,
      label = female_participation
    ),
    size = 4,
    color = "white",
    family = "JetBrains Mono",
    fontface = "bold"
  ) +
  geom_text(
    data = filter(data, country == "US", age_band == "15-19"),
    aes(
      x = 4.5 + female_participation,
      y = 10,
      label = paste0(female_participation, "%")
    ),
    size = 4,
    color = "white",
    family = "JetBrains Mono",
    fontface = "bold"
  ) +
  geom_segment(
    data = filter(data, country == "Europe"),
    aes(
      x = -6,
      y = 1:10,
      xend = -6 - female_participation,
      yend = 1:10,
      alpha = alpha
    ),
    lineend = "round",
    size = 9,
    color = "#ee6c4d"
  ) +
  geom_text(
    data = filter(data, country == "Europe", age_band != "15-19"),
    aes(
      x = -4.5 - female_participation,
      y = 1:9,
      label = female_participation
    ),
    size = 4,
    color = "white",
    family = "JetBrains Mono",
    fontface = "bold"
  ) +
  geom_text(
    data = filter(data, country == "Europe", age_band == "15-19"),
    aes(
      x = -4.5 - female_participation,
      y = 10,
      label = paste0(female_participation, "%")
    ),
    size = 4,
    color = "white",
    family = "JetBrains Mono",
    fontface = "bold"
  ) +
  labs(
    title = "Higher female participation in <span style='color: #ee6c4d'>Europe</span> compared to <span style='color: #3d5a80'>US</span> at every age band from 30 to 59",
    subtitle = "Level in percentage points of female participation for each age band in 2019, UK is included in the Europe figures"
  ) +
  scale_alpha_identity() +
  theme_void(base_size = 12) +
  theme(
    plot.margin = margin(15, 20, 10, 20),
    plot.title.position = "plot",
    plot.title = element_markdown(family = "Oswald", face = "bold", 
                                  size = rel(1.4), color = "grey60"),
    plot.subtitle = element_markdown(family = "Oswald", 
                                  size = rel(1.1), color = "grey30")
  ) +
  ggsave(here("goldmansachs-plot", "plot.png"), width = 15, height = 7.5, dpi = 320)
