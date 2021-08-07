library(tidyverse)
library(readxl)
library(ggthemes)
library(wesanderson)
library(patchwork)

data <- read_excel(
  path = 'cb-insights-plot/CB-Insights_Venture-Trends-Q2-2021.xlsx',
  sheet = 'Global Trends Charts',
  range = 'C111:N116'
) %>% 
  rename('region' = '...1')

x_labels <- c("2011", seq(12, 20), "21HY")

plot_data <- data %>% 
  pivot_longer(cols = -region, names_to = 'year', values_to = 'funding') %>% 
  mutate(
    year = if_else(year == '2021 YTD', '2021', year),
    year = as.numeric(year),
    region = fct_reorder(region, -funding)
  ) 

plot_main <- 
  ggplot(plot_data, aes(year, funding)) +
  geom_area(aes(fill = region), alpha = 0.8) +
  scale_x_continuous(breaks = seq(2011, 2021), labels = x_labels, expand = expansion(mult = 0)) +
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50), expand = expansion(mult = 0)) +
  scale_fill_manual(values = wes_palette('Darjeeling1')) +
  labs(
    title = "Global: Annual funding by region",
    subtitle = 'Figures in USD Billions'
  ) +
  theme_fivethirtyeight(base_family = 'Roboto Condensed', base_size = 16) +
  coord_cartesian(clip = 'off') +
  theme(
    legend.position = 'none',
    plot.subtitle = element_text(color = 'grey60', size = rel(0.9), face = 'bold'),
    axis.line = element_line(size = 0.4)
  )

plot_sidebar <- 
  ggplot(plot_data, aes(year, funding)) +
  geom_col(aes(fill = region), alpha = 0.9) +
  scale_x_continuous(breaks = seq(2011, 2021), labels = x_labels) +
  scale_y_continuous(position = 'right') +
  scale_fill_manual(values = wes_palette('Darjeeling1')) +
  facet_wrap(~region, ncol = 1, scales = 'free_y') +
  theme_fivethirtyeight(base_family = 'Roboto Condensed') +
  theme(
    legend.position = 'none',
    strip.text = element_text(face = 'bold')
  )

plot_combined <- plot_main + plot_sidebar + plot_layout(widths = c(0.75, 0.25)) &
  theme(plot.background = element_rect(fill = '#F0F0F0', color = '#F0F0F0'))

ggsave('cb-insights-plot/plot.png', plot_combined, width = 16, height = 9, dpi = 150)
