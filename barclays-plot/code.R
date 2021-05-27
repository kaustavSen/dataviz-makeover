library(tidyverse)
library(ggtext)
library(here)

# Source: https://www.investmentbank.barclays.com/content/dam/barclaysmicrosites/ibpublic/documents/our-insights/10_ESG_Themes_2021/Barclays_Investment_Bank_Sustainability_Matters_10_ESG_Themes_for_2021.pdf
# Bar chart on page 6

data <- tibble(
 question = rep(c("<b style='color: grey'>Positive Screening</b>", 
                  "<b style='color: grey'>Negative Screening</b>", 
                  "<b style='color: grey'>ESG Securities</b>", 
                  "<b>Corporate engagement</b>", 
                  "<b style='color: grey'>Index eligibility</b>"), 2),
 perc_response = c(68, 67, 36, 55, 18, 65, 60, 39, 40, 17),
 highlight = rep(c(0, 0, 0, Inf, 0), 2),
 area = rep(c("Continental Europe", "USA"), each = 5)
)

data <- 
  data %>% 
  mutate(question = fct_reorder(question, perc_response))

p <- ggplot(data) +
  geom_col(aes(x = highlight, y = question), fill = "grey70", alpha = 0.3, width = 0.8) +
  geom_point(aes(x = perc_response, y = question, fill = area), 
             shape = 21, size = 12, color = "white", 
             alpha = 0.9, stroke = 1.2, show.legend = FALSE) +
  annotate("text", x = 68, y = 5.35, label = "Continental\nEurope", family = "Roboto", 
           fontface = "bold", size = 3, color = "#3d405b", hjust = 0, lineheight = 0.8) +
  annotate("text", x = 65, y = 5.35, label = "USA", family = "Roboto", 
           fontface = "bold", size = 3, color = "#81b29a", hjust = 1) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 80), breaks = seq(0, 80, 20), labels = c("0%", "20", "40", "60", "80")) +
  scale_fill_manual(values = c("Continental Europe" = "#3d405b", "USA" = "#81b29a")) +
  labs(title = "How do you choose which securities can be bought by your ESG funds?",
       x = "Percentage of responses", y = "") +
  coord_cartesian(clip = "off") +
  ggthemes::theme_clean(base_family = "Roboto") +
  theme(
    plot.margin = margin(10, 20, 10, 20),
    panel.background = element_rect(colour = "white"),
    plot.title.position = "plot",
    plot.title = element_text(family = "Oswald", size = rel(1.6), margin = margin(b = 15)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", size = 0.5, linetype = "solid"),
    panel.grid.major.y = element_line(color = "grey90", size = 0.5, linetype = "solid"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(family = "JetBrains Mono", size = rel(1.2)),
    axis.text.y = element_markdown(size = rel(1.2)), 
    axis.title.x = element_text(hjust = 0.99, vjust = 18, size = rel(0.9), face = "bold")
  )

ggsave(here("barclays-plot", "plot.png"), p, width = 9, height = 6, dpi = 320)
