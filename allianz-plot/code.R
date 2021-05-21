library(tidyverse)
library(patchwork)
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
    ymax = seq(1.9, 20.9, 2),
    label_x = case_when(
      perc_pc_ins == max(perc_pc_ins) ~ xmax - 1.2,
      perc_pc_ins >= 5 ~ xmax - 0.1,
      TRUE ~ xmax + 1
    ),
    hjust = case_when(
      perc_pc_ins >= 5 ~ 1,
      TRUE ~ 0
    ),
    label_y = (ymin + ymax) / 2,
    label_color = if_else(perc_pc_ins >= 5, "white", "grey40"),
    label = if_else(perc_pc_ins == max(perc_pc_ins), 
                    paste0(perc_pc_ins, "%"), paste0(perc_pc_ins, " "))
  )

bar_chart <- 
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
  # Number labels
  geom_text(
    data = rects_country,
    aes(
      x = label_x,
      y = label_y,
      label = label,
      color = label_color,
      hjust = hjust
    ),
    family = "JetBrains Mono",
    fontface = "bold",
    size = 6
  ) +
  labs(
    title = "10 Countries account for more than 75% of the global P&C insurance market"
  ) +
  scale_x_continuous(limits = c(NA, 50)) +
  scale_color_identity() +
  theme_void(base_size = 12) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.title.position = "plot",
    plot.title = element_text(family = "Oswald", size = rel(2.5), 
                              face = "bold", hjust = 0.5)
  ) 

moon_chart <- 
  ggplot(tibble(values = c(0.77, 0.23))) +
  gggibbous::geom_moon(aes(x = 0.5, y = 0.5, ratio = values),
                       fill = "grey40", color = "grey40",
                       right = FALSE, show.legend = FALSE, size = 130) +
  gggibbous::geom_moon(aes(x = 0.5, y = 0.5, ratio = values),
                       fill = NA, color = "grey40",
                       right = TRUE, show.legend = FALSE, size = 130) +
  annotate("text", x = 0.49, y = 0.5,
           label = "Top 10\n77%", family = "JetBrains Mono",
           size = 8, color = "white", fontface = "bold", lineheight = 1) +
  annotate("text", x = 0.53, y = 0.5,
           label = "Others\n23%", family = "JetBrains Mono",
           size = 5.5, color = "black", fontface = "bold", lineheight = 0.9) +
  scale_x_continuous(limits = c(0.45, 0.55)) +
  scale_y_continuous(limits = c(0.45, 0.55)) +
  coord_cartesian(clip = "off") +
  theme_void()

bar_chart + 
  inset_element(moon_chart, 0.5, 0.5, 0.9, 0.5) +
  ggsave(here("allianz-plot", "plot.png"), 
         width = 14, height = 12, dpi = 320)