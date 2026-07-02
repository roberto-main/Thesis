library(dplyr)
library(tidyr)
library(tidyverse)
library(openxlsx)
library(readxl)
library(writexl)
library(corrplot)

df <- read_excel("Final_Dataset.xlsx", sheet='Macro_Regions')

#df <- read_excel("C:\\Users\\rober\\Desktop\\Final_Dataset.xlsx", sheet='Continents')
#Vedere la differenza tra Continents, Regions e Countries con questo grafico
#df <- read_excel("C:\\Users\\rober\\Desktop\\Final_Dataset.xlsx", sheet='Regions')
#df <- read_excel("C:\\Users\\rober\\Desktop\\Final_Dataset.xlsx", sheet='Countries')


df_wide <- df %>%
  dplyr::select(Continent, Year, Gini) %>%
  pivot_wider(names_from = Continent, values_from = Gini)

x <- df_wide$Year

y <- as.matrix(df_wide[, -1])

df$Region <- as.factor(df$Continent)

levels(df$Region) <- c("Africa", "Americas", "Americas", 
  "Asia", "Asia", "Asia", 
  "Asia", "Europe", "Europe", 
  "Europe", "Europe")

df %>%
  ggplot(aes(x = Year, y = Gini, color = Continent, group = Continent)) +
  geom_line(size = 1.5) +        # linee più spesse
  geom_point(size = 2) +         # punti visibili
  labs(title = "GINI Index by Continent",
       x = "Year",
       y = "GINI Index") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_blank()
  ) +
  facet_wrap(~Region, ncol = 4)
