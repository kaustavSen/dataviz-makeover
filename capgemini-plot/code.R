library(tidyverse)
library(scales)
library(ggtext)

data <- tibble(
  users = rep(c("Voice Assistant users", "Non Users"), each = 6),
  time = rep(rep(c("Today", "Three years from now"), each = 3), 2),
  type = rep(c("Physical Stores", "Website & Apps", "Voice Assistants"), 4),
  prop = c(0.59, 0.38, 0.03, 0.45, 0.37, 0.18, 0.71, 0.29, 0, 0.62, 0.31, 0.07)
) %>% 
  mutate(
    time = str_wrap(time, 15),
    time = fct_rev(factor(time)),
    type = factor(type, c("Physical Stores", "Website & Apps", "Voice Assistants")),
    users = factor(users, c("Voice Assistant users", "Non Users"))
  )

colors <- c("#33B9E8", "#F26B3A", "#BAD432")

ggplot(data, aes(time, prop, color = type)) +
  geom_line(aes(group = type), size = 1.2) +
  geom_point(size = 18) +
  geom_point(shape = 1, size = 21, stroke = 1.5) +
  geom_text(aes(label = percent(prop, accuracy = 1)),
            size = 5, family = "Poppins", color = "white", fontface = "bold") +
  scale_x_discrete(expand = expansion(mult = 0.1)) +
  scale_y_continuous(limits = c(0, 0.8),
                     breaks = seq(0, 0.8, 0.2), 
                     labels = percent_format(accuracy = 1),
                     sec.axis = dup_axis()) +
  scale_color_manual(values = colors) +
  coord_cartesian(clip = "off") +
  facet_wrap(~users) +
  labs(
    title = "Change in spending mix between <span style='color: #33B9E8'>physical stores</span>, <span style='color: #F26B3A'>websites & apps</span>, and <span style='color: #BAD432'>voice assistants</span>,<br>today and three years from now",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 18, base_family = "Poppins") +
  theme(
    plot.title.position = "plot",
    plot.title = element_markdown(size = rel(1.3), hjust = 0.5, 
                                  face = "bold", lineheight = 1.4),
    legend.position = "none",
    plot.margin = margin(t = 10, r = 30, b = 10, l = 20),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = NA, color = "black", size = 0.5),
    panel.spacing = unit(20, "mm"),
    axis.text.x = element_text(size = rel(1.1), vjust = 0.5),
    strip.text.x = element_text(size = rel(1.2), face = "bold", color = "white", 
                                margin = margin(t = 15, b = 15)),
    strip.background.x = element_rect(fill = "#7F6692", color = "grey98", size = 4)
  )
ggsave(here::here("capgemini-plot", "plot.png"), width = 16, height = 10, dpi = 150)
