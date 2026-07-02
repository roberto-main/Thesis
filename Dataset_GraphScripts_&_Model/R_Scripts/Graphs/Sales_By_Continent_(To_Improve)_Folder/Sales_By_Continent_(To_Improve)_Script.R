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

view(df)
df$Year <- as.numeric(df$Year)


df %>%
  group_by(Continent, Year) %>%
  summarise(Global_Sales = sum(Sales_in_Millions, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = Year, y = Global_Sales, color = Continent, group = Continent)) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "white") +
  scale_x_continuous(breaks = seq(min(df$Year), max(df$Year), 1)) +
  labs(
    title = "Video Game Sales by Continent",
    subtitle = "COVID shock indicated by dashed line",
    x = "Year",
    y = "Sales (Millions USD)",
    color = "Continent"
  ) +
  theme_dark()