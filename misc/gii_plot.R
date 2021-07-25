library(tidyverse)
library(pdftools)
library(patchwork)

pdf_link <- "misc/data/gii-full-report-2020.pdf"
pdf_pages <- pdf_text(pdf_link)

get_gdp_per_capita <- function(pdf_page) {
  pdf_page %>% 
    str_split("\n") %>% 
    pluck(1, 4) %>% 
    str_replace_all("[A-Za-z]", "") %>% 
    str_squish() %>% 
    str_split(" ") %>% 
    pluck(1, 5) %>% 
    parse_number()
}

pages <- seq(from = 215, to = 345) + 49
gdp_per_cap <- map_dbl(pages, ~get_gdp_per_capita(pdf_pages[.x]))

data_venture_cap <- read_csv("misc/data/venture_cap_deals.csv")
world_map <- read_csv("misc/data/worldtilegrid.csv")

plot_data <- 
  data_venture_cap %>% 
  bind_cols(gdp_per_cap = gdp_per_cap) %>% 
  filter(venture_cap_score != 0) %>% 
  mutate(
    Country = case_when(
      Country == "Czech Republic (the)" ~ "Czech Republic",
      Country == "Hong Kong, China" ~ "China",
      Country == "Netherlands (the)" ~ "Netherlands",
      Country == "Republic of Korea (the)" ~ "South Korea",
      Country == "Russian Federation (the)" ~ "Russian Federation",
      Country == "Trinidad and Tobago" ~ "Trinidad & Tobago",
      Country == "United Arab Emirates (the)" ~ "United Arab Emirates",
      Country == "United Kingdom (the)" ~ "Great Britain and Northern Ireland",
      Country == "United States of America (the)" ~ "United States of America",
      TRUE ~ Country
    )
  )

plot_text <- plot_data %>% 
  left_join(world_map, by = c("Country" = "name")) %>% 
  group_by(region) %>% 
  filter(venture_cap_score == max(venture_cap_score) | Country == "Qatar")

p <- 
  plot_data %>% 
  left_join(world_map, by = c("Country" = "name")) %>% 
  ggplot(aes(gdp_per_cap, venture_cap_score, color = region)) +
  geom_point(size = 4, alpha = 0.6) +
  ggrepel::geom_text_repel(data = plot_text, aes(label = Country), 
                           nudge_x = 0.4, nudge_y = 0.6, size = 4.5, family = "Roboto Condensed") +
  geom_segment(aes(x = 2000, xend = 2000, y = 90, yend = 98), color = "grey60",
               arrow = arrow(length = unit(2, "mm"), type = "closed")) +
  geom_segment(aes(x = 2000, xend = 2000, y = 2, yend = 10), color = "grey60",
               arrow = arrow(end = "first", length = unit(2, "mm"), type = "closed")) +
  annotate("text", x = 2650, y = 94, label = "High venture cap\npenetration", size = 3, family = "Roboto Condensed", color = "grey60", fontface = "bold", lineheight = 0.9) +
  annotate("text", x = 2650, y = 6, label = "Low venture cap\npenetration", size = 3, family = "Roboto Condensed", color = "grey60", fontface = "bold", lineheight = 0.9) +
  scale_x_continuous(trans = "log2", breaks = 2 ^ seq(12, 16, 2), labels = scales::comma_format()) +
  ggsci::scale_color_futurama() +
  labs(
    title = "Venture Capital Penetration in selected economies, 2016-18",
    x = "GDP per capita (USD)",
    y = "Venture Capital Score"
  ) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    plot.background = element_rect(fill = "grey95", color = "grey95"),
    plot.title.position = "plot",
    plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5, color = "grey30"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    legend.position = "none",
    axis.title = element_text(size = rel(0.7), hjust = 1, face = "bold", color = "grey40")
  )

p_legend <- 
  world_map %>% 
  filter(region != "Antarctica") %>% 
  ggplot() +
  geom_rect(aes(xmin = x, ymin = y, xmax = x + 1, ymax = y + 1, fill = region), size = 0.5, color = "white") +
  scale_y_reverse() +
  ggsci::scale_fill_futurama() +
  labs(
    title = "Region Legend"
  ) +
  theme_void(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    plot.background = element_rect(fill = "grey95", color = "grey95"),
    plot.title.position = "plot",
    plot.title = element_text(size = rel(0.7), face = "bold", hjust = 0.5, color = "grey70"),
    legend.position = "none",
  )

plot <- p + inset_element(p_legend, 0.3, 0.6, 0.52, 0.8, align_to = "full") & theme(plot.background = element_rect(fill = "grey95", color = "grey95"))
ggsave("misc/plots/gii_plot.png", plot, width = 8, height = 7, dpi = 150)
