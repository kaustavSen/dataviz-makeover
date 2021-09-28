library(tidyverse)
library(gt)
library(ggforce)

data <- tribble(
  ~industry, ~`2020`, ~`2025`, ~ggplot,
  "Wealth management", 0, 2.6, NA,
  "Consumer lending", 1.4, 15.7, NA,
  "Insurance", 5, 70.7, NA,
  "Payments", 16.1, 140.8, NA,
)

plot_bars <- function(data) {
  bar_length <- data[["2025"]]
  df <- tibble(x = c(0, bar_length, bar_length, 0),
               y = c(0, 0, 1, 1))
  
  ggplot(df, aes(x, y)) +
    geom_shape(radius = unit(0.8, "cm"), fill = "grey40") +
    scale_x_continuous(limits = c(0, 141)) +
    theme_void() +
    coord_cartesian(clip = "off")
  
}

plots <- data %>% 
  select(-ggplot) %>% 
  arrange(desc(`2025`)) %>% 
  group_by(industry) %>% 
  nest() %>% 
  mutate(plot = map(data, plot_bars))

table <-
data %>% 
  arrange(desc(`2025`)) %>% 
  gt() %>% 
  text_transform(
    locations = cells_body(columns = ggplot),
    fn = function(x) {
      map(plots$plot, ggplot_image, height = 20, aspect_ratio = 6)
    }
  ) %>% 
  tab_header(
    title = "FORECAST: Embedded Finance Market Value",
    subtitle = "Generated revenues, USD billions"
  ) %>% 
  tab_source_note(
    source_note = "Source: Lightyear Capital, 2020"
  ) %>% 
  opt_align_table_header(align = "left") %>% 
  cols_label(
    industry = "",
    ggplot = ""
  ) %>% 
  tab_style(
    style = cell_text(
      font = google_font("Roboto Condensed"),
      color = "#D4381F",
      weight = "bold"
    ),
    locations = cells_title("title")
  ) %>% 
  tab_style(
    style = cell_text(
      font = google_font("Roboto Condensed"),
      color = "grey",
      weight = "bold"
    ),
    locations = list(cells_title("subtitle"), cells_column_labels(), cells_source_notes())
  ) %>% 
  tab_style(
    style = cell_text(
      font = google_font("Karla")
    ),
    locations = cells_body(columns = "industry")
  ) %>% 
  tab_style(
    style = cell_text(
      font = google_font("Inconsolata")
    ),
    locations = cells_body(columns = -industry)
  ) %>% 
  cols_width(ggplot ~ px(100))

gtsave(table, "BI_Table.png", expand = 5, path = "business-insider")
