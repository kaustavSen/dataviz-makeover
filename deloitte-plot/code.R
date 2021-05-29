library(tidyverse)
library(ggchicklet)
library(here)

data <- tibble(
  business_unit = c("Audit & assurance", "Consulting", "Tax & Legal", "Risk Advisory", "Financial Advisory"),
  year_2019_20 = c(35, 34, 14, 9, 8),
  year_2015_16 = c(48, 27, 13, 6, 6)
)

data_for_plot <- 
  data %>% 
  pivot_longer(cols = -business_unit, values_to = "revenue", names_to = "year") %>% 
  mutate(
    year = if_else(year == "year_2019_20", "2019-20", "2015-16"),
    business_unit = fct_reorder(business_unit, revenue) %>% fct_rev(),
    business_unit = str_wrap(business_unit, width = 10),
    revenue = revenue / 100
  ) %>% 
  group_by(year) %>% 
  arrange(year, business_unit) %>% 
  mutate(
    agg_rev = accumulate(revenue, sum),
    label_y = lag(agg_rev, default = 0) + revenue / 2,
    label = paste0(revenue * 100, "%")
  )

ggplot(data_for_plot) +
  geom_chicklet(aes(x = year, y = revenue, fill = business_unit), 
                width = 0.85, alpha = 0.8) +
  geom_text(aes(x = year, y = label_y, label = label), 
            family = "Roboto Condensed", size = 3, fontface = "bold", color = "white") +
  scale_y_continuous(expand = c(0, 0), labels = scales::label_percent()) +
  guides(fill = guide_legend(title = NULL, label.position = "top", 
                             label.vjust = 0, label.theme = element_text(family = "Roboto Condensed", size = 6, lineheight = 0.8))) +
  ghibli::scale_fill_ghibli_d(name = "PonyoMedium") +
  labs(
    title = "Revenue by business unit",
    x = "",
    y = ""
  ) +
  coord_flip(clip = "off") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.margin = margin(15, 25, 15, 25),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = rel(1.5), hjust = 0.5),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold"),
    legend.position = "bottom",
    legend.box.spacing = unit(0, "mm"),
    legend.spacing.y = unit(0, "mm"),
    legend.spacing.x = unit(5, "mm"),
    legend.key.height = unit(5, "mm"),
    legend.key.width = unit(15, "mm")
  ) +
  ggsave(here("deloitte-plot", "plot.png"), width = 9, height = 3, dpi = 320)
  