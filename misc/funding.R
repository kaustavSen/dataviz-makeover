# Load packages
library(tidyverse)
library(wesanderson)
library(ggtext)
library(scales)
library(patchwork)

# Read-in data
data <- read_csv(here::here("misc", "data", "funding.csv"))

# Set theme
theme_set(theme_minimal(base_size = 21, base_family = "Poppins"))

theme_update(legend.position = "none",
             plot.title.position = "plot",
             plot.title = element_text(face = "bold", hjust = 0.5),
             strip.text = element_markdown(size = rel(1)),
             panel.grid.minor = element_blank(),
             plot.background = element_rect(fill = "grey98", color = NA),
             axis.line = element_line(color = "grey40", size = 0.5),
             panel.spacing = unit(10, "mm"))

# Plot for total funding
p1 <- 
  ggplot(data, aes(year, total_funding)) +
  geom_col(alpha = 0.8, fill = "#5BBCD6") +
  scale_x_continuous(name = "", breaks = seq(2011, 2020, 3)) +
  scale_y_continuous(name = "", expand = expansion(mult = c(0, 0.1)),
                     breaks = seq(0, 1250, 250), 
                     label = comma_format(suffix = "M")) +
  labs(
    title = "Trend in yearly total funding"
  ) +
  coord_cartesian(clip = "off") 

# Plot for number of companies receiving funding
p2 <- data %>% 
  pivot_longer(cols = pre_seed_and_seed:series_c_higher) %>% 
  mutate(
    fund_name = case_when(
      name == "pre_seed_and_seed" ~ "<span style='color: #FF0000'>**Pre-seed and seed**</span>",
      name == "series_a_b" ~ "<span style='color: #00A08A'>**Series A and B**</span>",
      name == "series_c_higher" ~ "<span style='color: #F2AD00'>**Series C and higher**</span>"
    ),
    fund_name = factor(fund_name, levels = c("<span style='color: #FF0000'>**Pre-seed and seed**</span>",
                                             "<span style='color: #00A08A'>**Series A and B**</span>",
                                             "<span style='color: #F2AD00'>**Series C and higher**</span>"))
  ) %>% 
  ggplot(aes(year, value)) +
  geom_col(aes(fill = name), alpha = 0.8) +
  scale_x_continuous(name = "", breaks = seq(2011, 2020, 3)) +
  scale_y_continuous(name = "", expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 3)) +
  labs(
    title = "Number of companies"
  ) +
  facet_wrap(~fund_name, ncol = 1) +
  coord_cartesian(clip = "off")

# Combine and save plot
ggsave(plot = p1 + p2 + 
        plot_layout(ncol = 2, widths = c(3, 1)),
       filename = here::here("misc", "plots", "funding.png"), 
       width = 18, height = 12, dpi = 150)