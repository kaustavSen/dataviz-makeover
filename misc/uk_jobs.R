library(tidyverse)
library(ggrepel)
library(scales)

uk_jobs <- read_csv("misc/data/uk_jobs.csv")

uk_jobs <- 
  uk_jobs %>% 
  # Convert to percentages
  mutate(across(where(is.numeric), ~ .x / 100))

uk_jobs_outliers <-
  uk_jobs %>%
    filter(occupation %in% c("Management / exec / consulting", 
                             "Marketing / advertising / PR",
                             "Charity / voluntary",
                             "HR & recruitment"))

quadrants <- tribble(
  ~xmin, ~xmax, ~ymin, ~ymax, ~name, ~fill, ~x, ~y,
  -Inf, 0, -Inf, 0, "Low Projection\nLow Growth", "#EBCC2A", -0.1, -0.1,
  -Inf, 0, 0, Inf, "Low Projection\nHigh Growth", "#E1AF00", -0.1, 0.18,
  0, Inf, -Inf, 0, "High Projection\nLow Growth", "#78B7C5", 0.1, -0.1,
  0, Inf, 0, Inf, "High Projection\nHigh Growth", "#3B9AB2", 0.1, 0.18
)

plot <- 
  ggplot(uk_jobs, aes(growth, job_posting)) +
  geom_rect(data = quadrants, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), alpha = 0.4, show.legend = FALSE, inherit.aes = FALSE) +
  geom_text(data = quadrants, aes(x = x, y = y, label = name, color = fill), inherit.aes = FALSE, size = 9, alpha = 0.5, family = "Staatliches", lineheight = 0.9) +
  geom_point(size = 2, color = "#0081a7") +
  geom_smooth(method = "lm", se = FALSE, size = 0.8, color = "#00afb9") +
  annotate("text", x = -0.13, y = 0.17, label = "Trend Line", size = 3.5,
           family = "Roboto Condensed", fontface = "bold", color = "#00afb9") +
  geom_text_repel(data = uk_jobs_outliers, aes(label = occupation), 
                  family = "Roboto Condensed", size = 3.5, color = "#0081a7", direction = "y") +
  geom_point(data = uk_jobs_outliers, shape = 1, size = 4, color = "#0081a7") +
  scale_x_continuous(name = "Employment Projections", limits = c(-.15, NA), label = percent_format(accuracy = 1)) +
  scale_y_continuous(name = "Growth in online job postings", limits = c(-.15, NA), label = percent_format(accuracy = 1)) +
  scale_fill_identity() +
  scale_color_identity() +
  labs(
    title = str_wrap("United Kingdom: The association between the growth of online job postings during the pandemic and medium to long-term employment projections by occupation", 90)
  ) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    plot.background = element_rect(fill = "grey98", color = "grey98"),
    plot.title.position = "plot",
    plot.title = element_text(family = "Roboto", face = "bold", hjust = 0.5, size = rel(1)),
    plot.margin = margin(20, 20, 20, 20),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 0.5),
    axis.title.x = element_text(hjust = 1, size = rel(0.7), color = "grey60", face = "bold"),
    axis.title.y = element_text(hjust = 1, size = rel(0.7), color = "grey60", face = "bold")
  )
ggsave("misc/plots/uk_jobs.png", plot, width = 9, height = 6, dpi = 150)
