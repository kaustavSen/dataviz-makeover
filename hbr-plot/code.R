library(tidyverse)
library(magick)
library(ggtext)

data <- read_csv("hbr-plot/data.csv")

icons <- list.files(path = "hbr-plot/icons/orig")
icons_path <- list.files(path = "hbr-plot/icons/orig", full.names = TRUE)

resize_image <- function(path, name) {
  image_read(path) %>% 
    image_scale("x50") %>% 
    image_write(paste0("hbr-plot/icons/", name))
}

# Save the re-sized images
walk2(icons_path, icons, resize_image)

labels <- c(
  "NYT" = "<img src='hbr-plot/icons/nyt.png'>",
  "The Economist" = "<img src='hbr-plot/icons/economist.png'>",
  "HBP" = "<img src='hbr-plot/icons/hbp.png'>",
  "Forbes" = "<img src='hbr-plot/icons/forbes.png'>",
  "Fortune" = "<img src='hbr-plot/icons/fortune.png'>",
  "Axios" = "<img src='hbr-plot/icons/axios.png'>"
  
)

plot_data <- data %>% 
  mutate(company = fct_reorder(company, rev_per_employee))

ggplot(plot_data, aes(rev_per_employee, company)) +
  geom_col(fill = "#1697a6", width = 0.7) +
  geom_col(data = filter(plot_data, company == "HBP"), fill = "#e76f51", width = 0.7) +
  annotate("text", y = 6.2, x = 570, label = "REVENUE PER EMPLOYEE", family = "Roboto Condensed",
           fontface = "bold", size = 3.5, color = "white", hjust = 1) +
  annotate("text", y = 5.95, x = 570, label = "$582k", family = "JetBrains Mono",
           fontface = "bold", size = 5.5, color = "white", hjust = 1) +
  geom_text(
    data = filter(plot_data, company != "HBP"),
    aes(x = rev_per_employee - 8, label = paste0(rev_per_employee, "k")),
    family = "JetBrains Mono", fontface = "bold", size = 5.5, 
    color = "white", hjust = 1
  ) +
  scale_x_continuous(name = "", expand = expansion(mult = 0)) +
  scale_y_discrete(
    name = "",
    labels = labels
  ) +
  labs(
    title = str_wrap("<span style='color: #e76f51'>HBP's</span> leaner staff gives it much better leverage than other orgnaizations", 90) %>% 
      str_replace_all("\\n", "<br />")
  ) +
  theme_minimal() +
  theme(
    plot.margin = margin(20, 20, 20, 20),
    plot.title.position = "plot",
    plot.title = element_markdown(family = "IBM Plex Sans", face = "bold", 
                                  size = 22, color = "grey30", lineheight = 1.1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_markdown(),
    axis.line.y = element_line(size = 0.8, color = "black")
  )
ggsave("hbr-plot/plot.png", width = 9, height = 7, dpi = 300)
