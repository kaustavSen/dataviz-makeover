library(tidyverse)
library(lubridate)
library(slider)
library(scales)
library(colorspace)
library(ggsci)
library(ggtext)

covid_data <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

countries <- c("France", "Italy", "Spain", "Germany", "United Kingdom")

covid_data_long <- 
  covid_data %>% 
  pivot_longer(cols = -c("Province/State", "Country/Region", "Lat", "Long"),
               names_to = "date", values_to = "cases") %>% 
  select(-`Province/State`, -Lat, -Long) %>% 
  rename(country = `Country/Region`) %>% 
  mutate(date = mdy(date))

covid_data_europe <-
  covid_data_long %>% 
  filter(country %in% countries) %>% 
  group_by(country, date) %>% 
  summarise(cases = sum(cases), .groups = "keep") %>% 
  mutate(
    two_week_avg = slide_index_dbl(.x = cases, .i = date, .f = mean, .before = 13)
  )

country_labels <-
  covid_data_europe %>% 
  ungroup() %>% 
  filter(date == max(date)) %>% 
  arrange(desc(country)) %>% 
  mutate(date = date %m+% weeks(1),
         count_final = paste0(round(cases / 1e6, 2), "M"),
         label = glue::glue("**{country}**<br /><span style='color: grey; font-size: 14px'>{count_final}</sapn>"),
         mid_point = two_week_avg / 2,
         lag = cumsum(two_week_avg) - two_week_avg,
         y = mid_point + lag) 
  
plot <- 
  covid_data_europe %>% 
  ggplot(aes(date, two_week_avg)) +
  geom_area(aes(fill = country, color = after_scale(darken(fill))), alpha = 0.8, size = 0.9) +
  geom_richtext(
    data = country_labels,
    aes(y = y, label = label, color = country),
    label.color = NA, fill = NA, family = "Roboto Condensed", size = 4.5,
    hjust = 0, lineheight = 1.1
  ) +
  annotate("richtext", x = ymd(20200701), y = 18e6, label = "COVID-19 Total Cases<br /><span style='color: grey; font-size: 30px'>*Zone Europe*</span>", 
           fontface = "bold", family = "Source Serif Pro", lineheight = 0.1, size = 12, label.color = NA, fill = NA) +
  scale_fill_futurama() +
  scale_color_futurama() +
  scale_x_date(name = "", date_breaks = "3 months", date_labels = "%B %Y", expand = expansion(mult = 0)) +
  scale_y_continuous(name = "", labels = number_format(scale = 1/1e6, suffix = "M", accuracy = 1), expand = expansion(mult = 0)) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 18) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 0.4),
    plot.margin = margin(10, 100, 10, 20)
  )
ggsave("misc/plots/covid.png", plot, width = 14, height = 8, dpi = 150)
