library(tidyverse)
library(readxl)
library(patchwork)

# Data cleaning -----------------------------------------------------------

data_raw <- read_excel(
  path = "efi-plot/Abb_B1-5_2018/Abb_B1-5_2018.xlsx",
  sheet = 1,
  range = "A6:AA9"
)

data_clean <- data_raw %>% 
  rename(title = "...1") %>% 
  pivot_longer(cols = -title, names_to = "year", values_to = "value") %>% 
  mutate(year = as.numeric(year))


# Helper functions --------------------------------------------------------

format_y_labels_percent <- function(x) {
  if_else(x == 0.25, "25%", paste0(round(x * 100), " "))
}

format_y_labels_numeric <- function(x) {
  if_else(x == 250, "250k", paste0(x, " "))
}

format_y_labels_numeric <- function(x) {
  if_else(x == 250, "250k", paste0(x, " "))
}

format_y_labels_currency <- function(x) {
  if_else(x == 1200, "$1.2tr", paste0(round(x/1000, 2), "  "))
}

# Chart 1: Patent Intensity -----------------------------------------------

data_chart_1 <- data_clean %>% 
  filter(title == "Patentintensität") %>% 
  mutate(
    value_lag = lag(value),
    fill = if_else(value >= value_lag, "#3b8ea5", "#f49e4c"),
    fill = if_else(year == 1990, "#f49e4c", fill)
  )

plot_1 <- ggplot(data_chart_1, aes(year, value, fill = fill)) +
  geom_col(alpha = 0.95) +
  annotate("text", x = 1997.5, y = 0.17, label = "Increasing trend ->", family = "Inter", size = 4, hjust = 1, fontface = "bold", color = "#3b8ea5") +
  annotate("text", x = 2006.5, y = 0.212, label = "<- Decreasing trend", family = "Inter", size = 4, hjust = 0, fontface = "bold", color = "#f49e4c") +
  scale_x_continuous(breaks = seq(1990, 2015, 5), labels = c("1990", "'95", "2000", "'05", "'10", "'15"),
                     expand = expansion(mult = 0.02)) +
  scale_y_continuous(breaks = seq(0, 0.25, 0.05), limits = c(0, 0.25), 
                     labels = format_y_labels_percent, expand = expansion(mult = c(0.02, 0))) +
  scale_fill_identity() +
  labs(subtitle = "Patent intensity") +
  coord_cartesian(clip = "off") +
  ggthemes::theme_fivethirtyeight(base_size = 14, base_family = "JetBrains Mono") +
  theme(
      plot.title.position = "plot",
      plot.subtitle = element_text(size = rel(1.2), hjust = 0.5, family = "Source Serif Pro", face = "bold.italic", color = "grey40")
  )   

# Chart 2: Transnational Patents ------------------------------------------

data_chart_2 <- data_clean %>% 
  filter(title == "Anzahl transnationaler Patente in Tsd.") %>% 
  mutate(
    value_lag = lag(value),
    fill = if_else(value >= value_lag, "#3b8ea5", "#f49e4c"),
    fill = if_else(year == 1990, "#f49e4c", fill)
  )

plot_2 <- ggplot(data_chart_2, aes(year, value, fill = fill)) +
  geom_col(alpha = 0.95) +
  scale_x_continuous(breaks = seq(1990, 2015, 5), labels = c("1990", "'95", "2000", "'05", "'10", "'15"),
                     expand = expansion(mult = 0.02)) +
  scale_y_continuous(breaks = seq(0, 250, 50), limits = c(0, 250), 
                     labels = format_y_labels_numeric, expand = expansion(mult = c(0.02, 0))) +
  scale_fill_identity() +
  labs(subtitle = "Number of transnational patents") +
  coord_cartesian(clip = "off") +
  ggthemes::theme_fivethirtyeight(base_size = 14, base_family = "JetBrains Mono") +
  theme(
      plot.title.position = "plot",
      plot.subtitle = element_text(size = rel(1.2), hjust = 0.5, family = "Source Serif Pro", face = "bold.italic", color = "grey40")
  ) 

# Chart 3: Gross domestic expenditure on R&D ------------------------------

data_chart_3 <- data_clean %>% 
  filter(title == "Bruttoinlandsaufwendungen für FuE (GERD) in Mrd. US-Dollar") %>% 
  mutate(
    value_lag = lag(value),
    fill = if_else(value >= value_lag, "#3b8ea5", "#f49e4c"),
    fill = if_else(year == 1990, "#f49e4c", fill)
  )

plot_3 <- ggplot(data_chart_3, aes(year, value, fill = fill)) +
  geom_col(alpha = 0.95) +
  scale_x_continuous(breaks = seq(1990, 2015, 5), labels = c("1990", "'95", "2000", "'05", "'10", "'15"),
                     expand = expansion(mult = 0.02)) +
  scale_y_continuous(breaks = seq(0, 1200, 300), limits = c(0, 1200), 
                     labels = format_y_labels_currency, expand = expansion(mult = c(0.02, 0))) +
  scale_fill_identity() +
  labs(subtitle = "Gross domestic expenditure on R&D") +
  coord_cartesian(clip = "off") +
  ggthemes::theme_fivethirtyeight(base_size = 14, base_family = "JetBrains Mono") +
  theme(
    plot.title.position = "plot",
    plot.subtitle = element_text(size = rel(1.2), hjust = 0.5, family = "Source Serif Pro", face = "bold.italic", color = "grey40")
  ) 


# Combine plots -----------------------------------------------------------

plot <- plot_2 + plot_1 + plot_3 + plot_annotation(title = "Development of patent intensity in the OECD") &
  theme(
    plot.margin = margin(15, 20, 10, 20),
    plot.background = element_rect(fill = "#F0F0F0", color = "#F0F0F0"),
    plot.title.position = "plot",
    plot.title = element_text(size = rel(2.5), hjust = 0.5, family = "Source Serif Pro", face = "bold", color = "black", margin = margin(b = 10))
  )
ggsave("efi-plot/plot.png", plot, width = 25, height = 8, dpi = 150)
