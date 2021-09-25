library(tidyverse)
library(gt)

data <- tribble(
  ~industry, ~`2020`, ~`2025`,
  "Wealth management", 0, 2.6,
  "Consumer lending", 1.4, 15.7,
  "Insurance", 5, 70.7,
  "Payments", 16.1, 140.8
)

table <- 
  gt(data) %>% 
  tab_header(
    title = "FORECAST: Embedded Finance Market Value",
    subtitle = "Generated revenues, USD billions"
  ) %>% 
  tab_source_note(
    source_note = "Source: Lightyear Capital, 2020"
  ) %>% 
  opt_align_table_header(align = "left") %>% 
  cols_label(
    industry = ""
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
  )

gtsave(table, "BI_Table.png", expand = 5, path = "business-insider")
