library(tidyverse)

data <- tibble(
  desc = c("Read Data", "Read and interpret data", "Communicate internally", "Make data-driven decisions", "Communicate Externally"),
  prop = c(75, 65, 63, 63, 46)
)

ggplot(data, aes(prop, desc)) +
  geom_col(alpha = 0.9, fill = "#C6CDF7", width = 0.6) +
  geom_text(aes(label = paste0(prop, "%")), nudge_x = -0.75, hjust = 1, size = 4,
            family = "Poppins", fontface = "bold", color = "white") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.01)), 
                     limits = c(0, 100), labels = paste0(seq(0, 100, 25), "%")) +
  labs(
    title = "How employees use data?",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 16, base_family = "Poppins") +
  theme(
    plot.margin = margin(5, 15, 5, 10),
    plot.title = element_text(face = "bold"),
    axis.line = element_line(size = 0.5),
    panel.grid.minor = element_blank()
  )

ggsave(here::here("accenture-plot", "plot.png"), width = 10, height = 6, dpi = 150)
