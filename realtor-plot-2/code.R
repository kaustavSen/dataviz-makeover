library(tidyverse)
library(lubridate)
library(ggforce)
library(here)

# Original article: https://www.realtor.com/news/trends/housing-market-was-rocked-by-covid-19-where-we-go-from-here/

data <- read_csv(here("realtor-plot-2", "data.csv"))

data_cleaned <- 
  data %>% 
  rename(rate = `US 30-Year`) %>% 
  mutate(
    date = mdy(Week),
    month = month(date),
    year = year(date),
    description = case_when(
      year == 2020 & month == 3 ~ "Federal Reserve cuts interest rates in early March to starve off an economic disaster",
      year == 2020 & month == 7 ~ "Interest rates fall below 3% for the first time since the 1970s",
      year == 2021 & month == 3 ~ "Rates expected to continue rising through 2021, eventually hitting 3.4% by end of the year"
    )
  )

df_rate_smooth <- as_tibble(
  spline(y = as.numeric(data_cleaned$rate),
         x = data_cleaned$date,
         n = 500)
) %>% 
  mutate(x = as_date(x))

p <- 
  ggplot(data_cleaned, aes(date, rate)) +
  geom_line(data = df_rate_smooth,
            aes(x, y), color = "grey70") +
  geom_point(shape = 21, size = 2.5, stroke = 1,
             color = "white", fill = "#264653") +
  geom_mark_circle(aes(filter = month %in% c(3, 7), 
                       description = description),
                   label.family = "Roboto", label.fontsize = 9,
                   fill = "grey80", alpha = 0.3, con.cap = unit(0, "mm"),
                   expand = unit(1.5, "mm")) +
  annotate("label", x = ymd(20200302), y = 3.8, 
           label = "U.S. weekly average 30-year fixed-rate mortgage rates",
           family = "Roboto", fontface = "bold", size = 4, hjust = 0, label.size = 0) +
  scale_y_continuous(name = "", 
                     limits = c(2, 3.8), 
                     breaks = seq(2.2, 3.8, 0.4),
                     labels = paste0(format(seq(2.2, 3.8, 0.4), nsmall = 1), c(rep(" ", 4), "%"))
                     ) +
  scale_x_date(name = "", 
               date_labels = "%B\n%Y", 
               date_breaks = "3 months",  
               expand = c(0, 0),
               limits = c(ymd(20200301), ymd(20210331))) +
  theme_minimal(base_family = "Roboto") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_text("JetBrains Mono") 
  )

ragg::agg_png(here("realtor-plot-2", "plot.png"), height = 6, width = 10, units = "in", res = 320)
print(p)
dev.off()
