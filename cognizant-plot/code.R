library(tidyverse)
library(tsibble)
library(patchwork)

data_revenue <- tibble(
  quarter = c("Q1 2019", "Q2 2019", "Q3 2019", "Q4 2019", "Q1 2020", "Q2 2020", "Q3 2020", "Q4 2020", "Q1 2021"),
  revenue = c(4110, 4141, 4248, 4284, 4225, 4000, 4243, 4184, 4401),
  eps = c(0.91, 0.94, 1.08, 1.07, 0.96, 0.82, 0.97, 0.67, 0.97)
) %>% 
  mutate(quarter = yearquarter(quarter))

p1 <- ggplot(data_revenue, aes(quarter, revenue)) +
  geom_col(width = 70, fill = "#1142A0", alpha = 0.5) +
  geom_text(aes(label = revenue), nudge_y = -110, color = "white", 
            fontface = "bold", family = "Roboto Condensed") +
  scale_x_yearquarter(date_break = "3 month") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.1))) +
  labs(
    title = "Revenue and EPS",
    subtitle = "Revenue in $ millions"
  ) +
  theme_void(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", hjust = 0.5, size = rel(1.8)),
    plot.subtitle = element_text(face = "italic", hjust = 0.5, size = rel(0.9), color = "grey60"),
    axis.text.x = element_text(size = rel(0.9))
  )

p2 <- ggplot(data_revenue, aes(1:9, eps)) +
  geom_line(color = "#0693FC") +
  geom_point(size = 4, color = "#0693FC") +
  geom_point(size = 6, color = "#0693FC", shape = 1) +
  geom_text(aes(label = paste0("$", eps)), nudge_y = -0.25, family = "Roboto Condensed", color = "grey20") +
  scale_x_continuous(limits = c(0.5, 9.5)) +
  scale_y_continuous(limits = c(0.25, 1.25), expand = expansion(mult = 0)) +
  labs(x = "", y = "") +
  coord_cartesian(clip = "off") +
  theme_void(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "grey97", color = "grey97")
  )

p1 / p2 & plot_layout(heights = c(0.85, 0.15))
ggsave(here::here("cognizant-plot", "plot.png"), width = 10, height = 7, dpi = 150)