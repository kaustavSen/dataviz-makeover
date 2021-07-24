library(tidyverse)
library(lubridate)
library(ggh4x)
library(colorspace)
library(geofacet)
library(patchwork)

data <- 
  read_tsv("misc/data/economic_sentiment_indicator.tsv") %>% 
  janitor::clean_names()

country_codes <- 
  countrycode::codelist %>% 
  select(country_name = iso.name.en, country_code = eurostat)

exclude_countries <- c("AL", "EA19", "EU27_2020", "ME", "MK", "RS", "TR")

date_breaks <- ymd(20200701, 20201101, 20210301, 20210601)

data_longer <- 
  data %>% 
  mutate(across(-indic_s_adj_geo_time, as.numeric)) %>% 
  pivot_longer(cols = -indic_s_adj_geo_time, names_to = "date", values_to = "esi")

plot_data <- 
  data_longer %>% 
  drop_na() %>% 
  separate(indic_s_adj_geo_time, into = c("indic", "continent", "country_code"), sep = ",") %>% 
  select(-indic, -continent) %>% 
  mutate(date = str_remove(date, "x") %>% 
                str_replace("m", "-") %>% 
                paste0("-01"),
         date = ymd(date)) %>% 
  left_join(country_codes) %>% 
  filter(! country_code %in% exclude_countries)

plot_theme <- list(
  theme_minimal(base_size = 18, base_family = "Roboto Condensed"),
  theme(
    plot.margin = margin(20, 20, 20, 20),
    plot.background = element_rect(fill = "grey98", color = "grey98"),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "line")
  )
)

plot <- 
  plot_data %>% 
  mutate(ymin = 100) %>% 
  ggplot(aes(x = date)) +
  stat_difference(aes(ymin = ymin, ymax = esi, color = after_scale(lighten(fill))), 
                  linetype = "solid", size = 1, alpha = 0.7) +
  geom_line(aes(y = ymin), color = "grey60", size = 1, linetype = "dashed") +
  scale_x_date(name = "", limits = range(date_breaks), breaks = date_breaks, date_labels = "%b\n%y") +
  scale_y_continuous(name = "") +
  scale_fill_manual(values = c("#3d5a80", "#ee6c4d")) +
  labs(title = "Chart: Wirtschaftsstimmung an der Schwelle zur Euphorie") +
  facet_geo(~country_code, grid = "eu_grid1", label = "name") +
  theme_minimal(base_size = 18, base_family = "Roboto Condensed") +
  theme(
    plot.margin = margin(20, 20, 20, 20),
    plot.background = element_rect(fill = "grey98", color = "grey98"),
    plot.title.position = "plot",
    plot.title = element_text(size = rel(1.8), hjust = 0.5, family = "Staatliches", margin = margin(b = 20)),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "line")
  )
ggsave("misc/plots/esi_plot.png", plot, width = 19, height = 15, dpi = 150)
