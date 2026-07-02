library(ggplot2); 
library(dplyr)
library(countrycode)
library(tidyr)
library(tidyverse)
library(openxlsx)
library(readxl)
library(writexl)
library(corrplot)
library(PerformanceAnalytics)


df <- read_excel("Final_Dataset.xlsx", sheet="Continents")

df <- df %>% 
  mutate(Year = as.numeric(Year)) %>% 
  filter(is.finite(Year))

df %>%
  group_by(Year) %>%
  summarise(Global_Sales = sum(Sales_in_Millions, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = Global_Sales)) +
  geom_line(linewidth = 1.2, color = "maroon") +
  geom_point() +
  geom_rect(aes(xmin = 2020, xmax = 2022, ymin = -Inf, ymax = Inf),
            fill = "green", alpha = 0.02, inherit.aes = FALSE)  +
  scale_x_continuous(breaks = seq(min(df$Year), max(df$Year), 1)) +
  labs(
    title = "Global Video Game Sales Over Time",
    subtitle = "COVID effects indicated by dashed lines",
    x = "Year",
    y = "Sales (Millions USD)"
  ) +
  theme_dark()
