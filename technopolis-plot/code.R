library(tidyverse)
library(ggstream)
library(pals)
library(patchwork)

theme_set(ggthemes::theme_fivethirtyeight(base_size = 14, base_family = "Poppins"))

data <- read_csv("technopolis-plot/data.csv")

plot_data <- data %>% 
  pivot_longer(cols = -year, names_to = "company", values_to = "funding") 

p1 <- ggplot(plot_data, aes(year, funding, fill = company)) +
  geom_stream(alpha = 0.85) +
  annotate("text", y = 150, x = 2014.1, label = "Offentlig finansiering (miljoner kronor)", 
           family = "Roboto Condensed", size = 4, fontface = "bold", color = "grey", hjust = 0) +
  scale_x_continuous(expand = expansion(mult = 0.02)) +
  scale_y_continuous(labels = function(x) abs(x)) +
  scale_fill_manual(values = ocean.curl(6)) +
  theme(
    legend.position = "none",
  )

p2 <- ggplot(plot_data, aes(year, funding, fill = company)) +
  geom_col(alpha = 0.85) +
  scale_fill_manual(values = ocean.curl(6)) +
  facet_wrap(~company) +
  theme(
    legend.position = "none",
    panel.spacing = unit(1.5, "line"),
    strip.text = element_text(family = "Roboto Condensed", face = "bold")
  )

p <- p1 + p2 + plot_layout(ncol = 1, heights = c(0.4, 0.6)) + 
plot_annotation(
  title = str_wrap("Offentlig finansiering per program och år till projekt från utlysningar 2014–2019 (staplar, vänsteraxel) och sammanlagd offentlig finansiering respektive medfinansiering per år för alla sex program (linjer, höger axel)", width = 110)
) &
  theme(plot.title.position = "plot",
        plot.title = element_text(size = rel(1.2), lineheight = 1.1))

ggsave("technopolis-plot/plot.png", p, width = 14, height = 9, dpi = 150)
