library(tidyverse)
library(here)

# Source: https://www.allianz.com/content/dam/onemarketing/azcom/Allianz_com/economic-research/publications/specials/en/2021/may/2021_05_14_InsuranceGrowth.pdf
# Pie chart on page 9

data <- tribble(
  ~country, ~perc_pc_ins,
  "Australia", 2,
  "Brazil", 1,
  "China", 11,
  "France", 5,
  "Germany", 5,
  "Italy", 2,
  "Japan", 5,
  "Spain", 2,
  "UK", 3,
  "USA", 41,
  "Others", 23
)

data_cleaned <- data %>%
  mutate(
    country = fct_reorder(country, perc_pc_ins),
    country = fct_relevel(country, "Others", after = 0)
  ) %>% 
  arrange(country)

data_cleaned %>% 
  filter(country != "Others") %>% 
  ggplot(aes(perc_pc_ins, country)) +
  geom_col() +
  theme_minimal()

rects_labels <- tibble(
  xmin = rep(-15, 10),
  xmax = rep(-4, 10),
  ymin = seq(1, 20, 2),
  ymax = seq(2, 20, 2)
)

rects_joining <- tibble(
  group = 1:10
) %>% 
  rowwise() %>% 
  mutate(
    x = list(c(-4, 0, 0, -4)),
    y = list(
      c(
        (group - 1) * 2 + 1,
        (group - 1) * 2 + 1.1,
        (group - 1) * 2 + 1.9,
        (group - 1) * 2 + 2
      )
    )
  ) %>% 
  unnest(cols = c(x, y))

rects_country <-
  data_cleaned %>% 
  filter(country != "Others") %>% 
  mutate(
    xmin = rep(0, 10),
    xmax = perc_pc_ins,
    ymin = seq(1.1, 20.1, 2),
    ymax = seq(1.9, 20.9, 2)
  )

ggplot() +
  # Horizontal rectangles for labels
  geom_rect(
    data = rects_labels,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "grey40"
  ) +
  # Joining paths
  geom_polygon(
    data = rects_joining,
    aes(x = x, y = y, group = group),
    fill = "grey70",
    alpha = 0.9
  ) +
  # Plot the country-wise data
  geom_rect(
    data = rects_country,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "grey40"
  ) +
  # Country Names
  geom_text(
    data = rects_country,
    aes(x = -14, y = ymin + 0.4, label = country),
    family = "Roboto",
    fontface = "bold",
    color = "white",
    size = 7,
    hjust = 0
  ) +
  labs(
    title = "10 Countries account for 75% of the global P&C insurance market"
  ) +
  scale_x_continuous(limits = c(NA, 50)) +
  theme_void(base_size = 12) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.title.position = "plot",
    plot.title = element_text(family = "Oswald", size = rel(2.5), face = "bold")
  ) +
  ggsave(here("allianz-plot", "plot.png"), width = 14, height = 12, dpi = 320)
