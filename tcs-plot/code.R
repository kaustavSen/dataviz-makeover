library(tidyverse)
library(ggtext)
library(systemfonts)

font <- system_fonts() %>% filter(family == "Font Awesome 5 Free")
register_font(
  name = "Font Awesome",
  plain = font$path[1]
)

males <- c(53, 60, 121)
females <- c(60, 58, 118)
males_scaled <- 5  * males / 121 
females_scaled <- 5  * females / 121 
males_label <- c("53", "60", "121 hours")
females_label <- c("60", "58", "118 hours")

ragg::agg_png(here::here("tcs-plot", "plot.png"), width = 8, height = 0.7 * 8, res = 150, units = "in")
ggplot() +
  geom_segment(aes(x = 0.4, xend = males_scaled + 0.4, y = 1:3, yend = 1:3), lineend = "round", size = 10, color = "#017EC5") +
  geom_segment(aes(x = -0.4, xend = -females_scaled - 0.4, y = 1:3, yend = 1:3), lineend = "round", size = 10, color = "#EE5389") +
  geom_text(aes(x = 0, y = 1:3, label = "clock"),
            family = "Font Awesome", size = 15, color = "#3d405b") +
  geom_text(aes(x = 0, y = 1:3 + 0.3, label = c("Senior", "Middle", "Junior")),
            family = "Poppins", size = 6, fontface = "bold") +
  geom_text(aes(x = males_scaled + 0.5, y = 1:3, label = males_label),
            family = "JetBrains Mono", color = "white", size = 4, hjust = 1, fontface = "bold") +
  geom_text(aes(x = -females_scaled - 0.5, y = 1:3, label = females_label),
            family = "JetBrains Mono", color = "white", size = 4, hjust = 0, fontface = "bold") +
  scale_y_continuous(limits = c(0.5, 3.5)) +
  scale_x_continuous(limits = c(-6, 6)) +
  labs(
    title = "Talent Development",
    subtitle = "Average learning hours per employees split by <span style='color: #017EC5'>**Males**</span> and <span style='color: #EE5389'>**Females**</span>"
  )+
  theme_void(base_family = "Poppins", base_size = 16) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = rel(1.8), hjust = 0.5, face = "bold"),
    plot.subtitle = element_markdown(size = rel(0.8), hjust = 0.5)
  )
dev.off()
