library(tidyverse)
library(ggthemes)
library(ggtext)
library(scales)

data <- read_csv("ycombinator-plot/data.csv")

subtitle <- "% of companies funded each year which have their main focus in the emerging markets of Africa, Latin America & Middle East"

data %>% 
  mutate(percent = parse_number(percent) / 100) %>% 
  ggplot(aes(year, percent)) +
  geom_step(color = "#B40F20", size = 1.05) +
  scale_x_continuous(labels = number_format(accuracy = 1, big.mark = "")) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.3),
                     expand = expansion(mult = 0), breaks = seq(0, 0.3, 0.1)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Y Combinator's <span style='color: #B40F20'>emerging</span> markets focus",
    subtitle = str_wrap(subtitle)
  ) +
  theme_fivethirtyeight(base_family = "Inter") +
  theme(
    plot.title.position = "plot",
    plot.title = element_markdown(family = "Poppins", hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(hjust = 0.5, lineheight = 1.2, color = "grey60",
                                     size = rel(0.9), margin = margin(b = 10))
  )
ggsave("ycombinator-plot/plot.png", width = 8, height = 6)
