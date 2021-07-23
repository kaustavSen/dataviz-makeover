library(tidyverse)
library(janitor)
library(lubridate)
library(ggforce)
library(ggh4x)
library(colorspace)

data <- 
  read_tsv("misc/data/economic_sentiment_indicator.tsv") %>% 
  clean_names()

country_codes <- 
  countrycode::codelist %>% 
  select(country_name = iso.name.en, country_code = eurostat)

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
  left_join(country_codes)
  # filter(is.na(country_name)) %>%
  # count(country_name)

plot_data %>% 
  # count(country_name)
  mutate(ymin = 100) %>% 
  filter(country_name == "Croatia") %>% 
  ggplot(aes(x = date)) +
  stat_difference(aes(ymin = ymin, ymax = esi, color = after_scale(lighten(fill))), 
                  linetype = "solid", size = 1, alpha = 0.8) +
  geom_line(aes(y = ymin), color = "grey60", size = 1, linetype = "dashed") +
  scale_fill_manual(values = c("#3d5a80", "#ee6c4d")) +
  # scale_color_manual(values = c("#3d5a80", "#ee6c4d")) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )
