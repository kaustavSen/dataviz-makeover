library(tidyverse)
library(ggtext)
library(glue)

data <- 
  read_csv("pwc-plot/data.csv") %>% 
  mutate(
    across(.cols = -city, .fns = ~parse_number(.x) / 100),
    city = if_else(city == "Average", "**Average**", city),
    city = fct_reorder(city, increase)
  ) %>% 
  pivot_longer(cols = -city, values_to = "percent", names_to = "category") %>% 
  mutate(category = factor(category, levels = c("reduce", "same", "increase"))) %>% 
  group_by(city) %>% 
  mutate(
    cum_percent = cumsum(percent),
    label_x = coalesce(lag(cum_percent) + percent/2, percent / 2),
    label_x = ifelse(category == "reduce", 1.02, label_x),
    label_color = ifelse(category == "reduce", "black", "white"),
    label = ifelse(percent == 0, NA, glue("{percent * 100}%"))
  ) 

legend_labels <- str_pad(c("Reduce Workforce", "Remain the same", "Increase Workforce"), width = 25, side = "right")

p <- 
  data %>% 
  ggplot(aes(percent, city, fill = category)) +
  geom_col() +
  geom_text(aes(x = label_x, label = label, color = label_color), family = "JetBrains Mono", size = 3.5) +
  scale_fill_manual(values = c("#CD681F", "#636366", "#FBB133"), labels = legend_labels) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.1))) +
  scale_color_identity() +
  guides(fill = guide_legend(title = NULL, reverse = TRUE, label.hjust = 0)) +
  labs(
    title = str_wrap("Over the next 12 months, what expectations do you have about the number of employees in your company?", width = 80),
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 12, base_family = "Roboto") +
  theme(
    plot.margin = margin(t = 20, r = 10, b = 30, l = 10),
    plot.title = element_text(family = "Roboto Condensed", face = "bold"),
    legend.direction = "horizontal",
    legend.position = c(0.4, -0.04),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_markdown()
  )
ggsave("pwc-plot/plot.png", p, width = 8, height = 8, dpi = 150)
