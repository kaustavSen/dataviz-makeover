library(tidyverse)
library(lubridate)
library(waffle)
library(ggtext)
library(systemfonts)
library(glue)
library(here)

# Original article: https://www.realtor.com/news/trends/housing-market-was-rocked-by-covid-19-where-we-go-from-here/

data <- read_csv(here("realtor-plot-1", "data.csv"))

data_cleaned <- 
  data %>% 
  mutate(
    date = mdy(month),
    month_name = month(date, label = TRUE, abbr = FALSE),
    icon = "home",
    cnt_trans = round(cnt_trans / 10000, 0),
    month_name = glue("{month_name}<br><span style='font-family: JetBrains Mono; color:grey; font-size:15px; font-face: bold'>{cnt_trans * 10}k</span>")
  ) 

font <- system_fonts() %>% filter(family == "Font Awesome 5 Free")
register_font(
  name = "Font Awesome",
  plain = font$path[1]
)

p <- 
  ggplot(data_cleaned) +
  geom_pictogram(aes(values = cnt_trans, color = icon, label = icon), 
              size = 6, flip = TRUE, show.legend = FALSE, family = "Font Awesome") +
  facet_wrap(~month_name, ncol = 2, strip.position = "left") +
  scale_color_manual(values = "#2a9d8f") +
  labs(
    title = "Number of completed home purchases in 2020",
    caption = "<span style='font-family: \"Font Awesome\"; color: #2a9d8f'>home</span> = 10,000 purchases"
  ) +
  theme_void(base_family = "Roboto", base_size = 12) +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(family = "Oswald", hjust = 0.5, 
                              size = rel(1.8), face = "bold", margin = margin(b = 10)),
    plot.caption = element_markdown(size = rel(1), hjust = 1, color = "grey30"),
    plot.margin = margin(10, 20, 10, 20),
    panel.spacing = unit(0.5, "lines"),
    strip.text.y.left = element_markdown(vjust = 0.5, hjust = 1, size = rel(1.4), 
                                         face = "bold", lineheight = 1.2)
  )

ragg::agg_png(here("realtor-plot-1", "plot.png"), 
              height = 12, width = 10, units = "in", res = 320)
print(p)
dev.off()
