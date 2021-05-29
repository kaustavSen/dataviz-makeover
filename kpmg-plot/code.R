library(tidyverse)
library(waffle)
library(systemfonts)
library(ragg)
library(here)

font <- system_fonts() %>% filter(family == "Font Awesome 5 Free")
register_font(
  name = "Font Awesome",
  plain = font$path[1]
)

data <- tibble(
  consultation = c("Fraud, anti-bribery and Wwft", "Auditing standards", "Accounting Standards"),
  fy_2016_17 = c(113, 478, 193),
  fy_2015_16 = c(95, 315, 174)
)

data_for_plot <- 
  data %>% 
  pivot_longer(cols = -consultation, values_to = "num", names_to = "year") %>% 
  mutate(
    year = if_else(year == "fy_2016_17", "FY 2016/17", "FY 2015/16"),
    num = num / 10,
  ) %>% 
  group_by(year) %>% 
  mutate(tot = sum(num))

p <- 
  ggplot(data_for_plot, aes(values = num, label = consultation)) +
  geom_pictogram(aes(color = consultation), 
                 family = "Font Awesome", 
                 size = 10, flip = TRUE,
                 n_rows = 5, make_proportional = FALSE) +
  scale_y_continuous(
    limits = c(0, 16),
    breaks = seq(4, 16, 4),
    labels = seq(4, 16, 4) * 50,
    sec.axis = dup_axis()
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Fraud, anti-bribery and Wwft" = "#335c67", 
               "Auditing standards" = "#e09f3e", 
               "Accounting Standards" = "#9e2a2b"),
    labels = c("Fraud, anti-bribery\nand Wwft",
               "Accounting\nStandards",
               "Auditing\nstandards")
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c("Fraud, anti-bribery and Wwft" = "briefcase", 
               "Auditing standards" = "briefcase", 
               "Accounting Standards" = "briefcase"),
    labels = c("Fraud, anti-bribery\nand Wwft",
               "Accounting\nStandards",
               "Auditing\nstandards")
  ) +
  labs(
    title = "Consulting Projects Completed",
    caption = "<span style='font-family: \"Font Awesome\"; color: #6b705c'>briefcase</span> = 5 projects"
  ) +
  guides(fill = guide_legend(label.vjust = 0.5)) +
  facet_wrap(~year) +
  coord_cartesian(clip = "off", expand = FALSE) +
  theme_minimal(base_size = 16, base_family = "Roboto Condensed") +
  theme(
    plot.margin = margin(15, 25, 15, 25),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(margin = margin(b = 15), face = "bold", size = rel(1.6), hjust = 0.5),
    plot.caption = ggtext::element_markdown(size = rel(1.1)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing.x = unit(15, "mm"),
    strip.text.x = element_text(margin = margin(b = 30), face = "bold", size = rel(1.2), color = "grey60"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = "JetBrains Mono", size = rel(1.1)),
    legend.position = "bottom",
    legend.box.spacing = unit(0, "mm"),
    legend.text = element_text(margin = margin(r = 30))
  )

agg_png(filename = here("kpmg-plot", "plot.png"), width = 8, height = 10, res = 320, units = "in")
print(p)
dev.off()
