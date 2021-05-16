library(tidyverse)
library(lubridate)
library(waffle)
library(ggtext)
library(here)

data <- read_csv(here("realtor-plot-1", "data.csv"))

data_cleaned <- 
  data %>% 
  mutate(
    date = mdy(month),
    month_name = month(date, label = TRUE, abbr = FALSE),
    icon = "home",
    cnt_trans = round(cnt_trans / 10000, 0)
  ) 

ggplot(data_cleaned) +
  geom_waffle(aes(values = cnt_trans, fill = icon), 
              flip = TRUE, show.legend = FALSE, 
              color = "white", size = 0.6) +
  facet_wrap(~month_name, ncol = 2, strip.position = "left") +
  scale_fill_manual(values = "#81b29a") +
  coord_equal() +
  theme_void() +
  theme(
    strip.text.y.left = element_text(angle = 0, vjust = 0.6, hjust = 1)
  )

p <- 
  ggplot(data_cleaned) +
  geom_pictogram(aes(values = cnt_trans, color = icon, label = icon), 
              size = 6, flip = TRUE, show.legend = FALSE, family = "Font Awesome 5 Free") +
  facet_wrap(~month_name, ncol = 2, strip.position = "left") +
  scale_color_manual(values = "#2a9d8f") +
  labs(
    title = "Number of completed home purchases in 2020",
    caption = "<span style='font-family: \"Font Awesome 5 Free\"; color: #2a9d8f'>home</span> = 10,000 purchases"
  ) +
  theme_void(base_family = "Roboto", base_size = 12) +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(size = rel(1.8), face = "bold"),
    plot.caption = element_markdown(size = rel(1), hjust = 1, color = "grey30"),
    plot.margin = margin(10, 20, 10, 20),
    panel.spacing = unit(0.5, "lines"),
    strip.text.y.left = element_text(vjust = 0.5, hjust = 1, size = rel(1.4), face = "bold")
  )

ragg::agg_png(here("realtor-plot-1", "plot.png"), height = 12, width = 8, units = "in", res = 320)
print(p)
dev.off()
