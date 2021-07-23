library(tidyverse)
library(lubridate)
library(systemfonts)
library(waffle)
library(ggtext)
library(patchwork)

font <- system_fonts() %>% filter(family == "Font Awesome 5 Free")
register_font(
  name = "Font Awesome",
  plain = font$path[1]
)

icon <- png::readPNG("reddit-plot/icon.png", native = TRUE)

data <- read_csv("reddit-plot/data.csv")

plot_data <-
  data %>% 
  mutate(
    year = year(date),
    month = month(date)
  ) %>% 
  group_by(year) %>% 
  filter(subscribers == max(subscribers, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    day = 1, 
    new_subscribers = if_else(year == 2012, 0, subscribers - lag(subscribers)),
    new_subscribers = round(new_subscribers/1000, 0),
    subscribers = round(subscribers/1000, 0)
  ) %>% 
  pivot_longer(cols = c("subscribers", "new_subscribers"))

p <- 
  ggplot(plot_data, aes(color = name, values = value, label = "user")) +
  geom_pictogram(size = 3, n_rows = 10, flip = TRUE, family = "Font Awesome") +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 10), expand = c(0, 0), 
                     label = function(x) paste0(x, "k"), position = "right") +
  scale_color_manual(values = c("#96F2C5", "#91DCF2")) +
  labs(
    title = "<span style='color: #91DCF2'>Existing</span> and <span style='color: #96F2C5'>New</span> user growth over time",
    subtitle = "r/stopdrinking"
  ) +
  facet_wrap(~year, nrow = 1, strip.position = "bottom") +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 16, base_family = "Roboto Condensed") +
  theme(
    plot.margin = margin(20, 20, 20, 20),
    plot.title.position = "plot",
    plot.title = element_markdown(family = "Bungee", hjust = 0.5, size = rel(1.8), color = "white"),
    plot.subtitle = element_text(hjust = 0.5, size = rel(1.5), face = "italic", family = "DM Serif Display", color = "white"),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(face = "bold", color = "white", size = rel(1.1)),
    axis.line.x = element_line(color = "white", size = 0.6),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = "white", face = "bold"),
    axis.ticks.y = element_line(color = "white"),
    panel.spacing.x = unit(1, "line")
  )

ragg::agg_png("reddit-plot/plot.png", width = 15, height = 8, res = 150, units = "in")
print(
  p +
  inset_element(icon, 0, 0.85, 0.15, 1, align_to = "full") & 
  theme(
    plot.background = element_rect(fill = "grey30", color = "grey30")
  )
)
dev.off()

