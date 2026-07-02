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


df <- read_excel("Final_Dataset.xlsx", sheet="Countries")

df$Year <- as.numeric(df$Year)
df <- df[!is.na(df$Country),]

df$Year <- as.numeric(df$Year)



df %>%
  
  ggplot(aes(x = Unemployment, y = Sales_in_Millions)) +
  
  geom_point(alpha = 0.6) +
  
  geom_smooth(method = "lm", color = "maroon") +
  
  labs(
    
    title = "Unemployment and Video Game Sales",
    
    x = "Unemployment Rate",
    
    y = "Sales (Millions USD)"
    
  ) +
  
  theme_dark()
