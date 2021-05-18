library(tidyverse)
library(lubridate)
library(here)

# Original viz: https://www.blackrock.com/corporate/insights/blackrock-investment-institute/macro-insights/april-19-2021

data <- read_csv(here("blackrock-plot", "data.csv"))

data_cleaned <- data %>% 
  mutate(
    Date = dmy(Date),
    Rate = parse_number(Rate)
  )

df_rate_smooth <- as_tibble(
  spline(y = as.numeric(data_cleaned$Rate),
         x = data_cleaned$Date,
         n = 500)
) %>% 
  mutate(x = as_date(x))

df_labels <- tibble(
  Date = c(ymd(20210310), ymd(20210915)),
  Rate = c(22.5, 11),
  label = c("Likely impact of American Rescue Plan stimulas payments",
            "Projected reversion to pre-COVID saving rate")
) %>% 
  mutate(label = str_wrap(label, width = 30))

ggplot(data_cleaned, aes(Date, Rate)) +
  # Based on actual past data
  geom_line(data = filter(df_rate_smooth, x <= ymd(20201201)), 
            aes(x, y), color = "#f15032", size = 1.05) +
  # Based on estimated future projection
  geom_line(data = filter(df_rate_smooth, x > ymd(20201201)), 
            aes(x, y), color = "#f15032", size = 1.05, alpha = 0.4) +
  geom_point(shape = 21, size = 2.5, stroke = 1,
             color = "white", fill = "#eb7a1c") +
  annotate("label", x = ymd(20190901), y = 38, 
           label = "U.S personal saving as a % of disposable income",
           family = "Roboto", fontface = "bold", size = 5.5, 
           hjust = 0, label.size = 0) +
  geom_text(
    data = df_labels,
    aes(label = label),
    family = "Roboto", size = 3, 
    hjust = 0, lineheight = 0.9
  ) +
  scale_x_date(name = "", 
               date_labels = "%B\n%Y", 
               date_breaks = "6 months",  
               expand = c(0, 0),
               limits = c(ymd(20190901), ymd(20220301))) +
  scale_y_continuous(name = "",
                     limits = c(0, 40),
                     labels = paste0(seq(0, 40, 10), 
                                     c(rep(" ", 4), "%"))) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "Roboto", base_size = 12) +
  theme(
    plot.margin = margin(10, 30, 10, 20),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text("JetBrains Mono")
  ) +
  ggsave(here("blackrock-plot", "plot.png"), height = 6, width = 10, units = "in", dpi = 320)

