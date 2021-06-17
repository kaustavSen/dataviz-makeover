library(tidyverse)
library(ggtext)
library(here)

data <- read_csv(here("misc", "data", "engagement_factors.csv"))

data <- data %>% 
  group_by(type) %>% 
  mutate(factors = fct_reorder(factors, percent)) %>% 
  ungroup() %>% 
  mutate(percent_scaled = percent / max(percent) * 3.5)

ggplot() +
  geom_text(data = filter(data, type == "positive"),
            aes(label = factors, x = 0, y = 8:1, 
                size = c(rep(6, 3), rep(5, 5)),
                color = c(rep("#0081a7", 3), rep("#00afb9", 5))), 
            hjust = 0, family = "Source Sans Pro",
            fontface = c(rep("bold", 3), rep("plain", 5))) +
  geom_segment(data = filter(data, type == "positive"),
               aes(x = 0, xend = percent_scaled, 
                   y = 8:1 - 0.5, yend = 8:1 - 0.5), 
               size = 6, color = c(rep("#0081a7", 3), rep("#00afb9", 5))) +
  geom_text(data = filter(data, type == "positive"),
            aes(label = percent, x = percent_scaled, y = 8:1 - 0.5), 
            nudge_x = -0.05, color = "white", size = 4,
            hjust = 1, family = "Source Sans Pro",
            fontface = "bold") +
  geom_text(data = filter(data, type == "negative"),
            aes(label = factors, x = 4, y = 8:1, 
                size = c(rep(6, 3), rep(5, 5)),
                color = c(rep("#f07167", 3), rep("#fed9b7", 5))), 
            hjust = 0, family = "Source Sans Pro",
            fontface = c(rep("bold", 3), rep("plain", 5))) +
  geom_segment(data = filter(data, type == "negative"),
               aes(x = 4, xend = 4 + percent_scaled, 
                   y = 8:1 - 0.5, yend = 8:1 - 0.5), 
               size = 6, color = c(rep("#f07167", 3), rep("#fed9b7", 5))) +
  geom_text(data = filter(data, type == "negative"),
            aes(label = percent, x = 4 + percent_scaled, y = 8:1 - 0.5), 
            nudge_x = -0.05, color = "white", size = 4,
            hjust = 1, family = "Source Sans Pro",
            fontface = "bold") +
  labs(
    title = "<span style='color: #0081a7'>**Positive**</span> and <span style='color: #f07167'>**Negative**</span> factors influencing engagement with business",
    subtitle = "A % of Spain respondents not in business"
  ) +
  scale_x_continuous(limits = c(0, 7)) +
  scale_size_identity() +
  scale_color_identity() +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    plot.title.position = "plot",
    plot.title = element_markdown(family = "Source Serif Pro", size = 22, 
                                  hjust = 0.5),
    plot.subtitle = element_markdown(family = "Source Serif Pro", size = 18,
                                     hjust = 0.5, face = "italic", color = "grey60",
                                     margin = margin(t = 5, b = 10)),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    plot.background = element_rect(fill = "grey98", color = "grey98")
  ) +
  ggsave(here("misc", "plots", "engagement_factors.png"), width = 10, height = 6, dpi = 150)
