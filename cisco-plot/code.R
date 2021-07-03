library(tidyverse)
library(lubridate)
library(ggrepel)
library(wesanderson)

data <- 
  read_csv("cisco-plot/data.csv") %>% 
  mutate(date = mdy(date))

# Date labels
dates <- ymd(20150701) %m+% months(seq(0, 60, 12))

# Label position
label_df <- tibble(x = ymd(20200915), y = c(267, 188, 142), index = unique(data$index))

p <- 
  ggplot(data, aes(date, value, color = index)) +
  geom_line(size = 0.6) +
  geom_text(
    data = label_df,
    aes(x = x, y = y, label = index), hjust = 1, size = 3.5, family = "Roboto Condensed"
  ) +
  scale_x_date(breaks = dates, date_labels = "%b\n%Y") +
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50), labels = scales::dollar_format()) +
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  labs(
    title = "Comparison of 5-year cumulative total return",
    subtitle = str_wrap("among Cisco Systems, Inc., the S&P 500 Index, and the S&P Information Technology Index", 50),
    x = "",
    y = ""
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "Roboto", base_size = 16) +
  theme(
    legend.position = "none",
    axis.text = element_text(family = "Roboto Condensed"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 0.5),
    plot.title.position = "plot",
    plot.title = element_text(family = "Roboto Condensed", face = "bold"),
    plot.subtitle = element_text(color = "grey60", size = rel(0.8))
  )
ggsave("cisco-plot/plot.png", p, width = 8, height = 6, dpi = 150)

