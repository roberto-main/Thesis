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


df <- read_excel("Final_Dataset.xlsx", sheet='Countries')

df$Year <- as.numeric(df$Year)
df <- df[!is.na(df$Country),]

continent_region_map <- c(
  #AMERICAS
  "Canada" = "Americas - North America",
  "United States" = "Americas - North America",
  "Mexico" = "Americas - Latin America",
  "Chile" = "Americas - Latin America",
  "Colombia" = "Americas - Latin America",
  
  #AFRICA
  "Nigeria" = "Africa",
  "South Africa" = "Africa",
  "Egypt" = "Africa",
  
  #ASIA
  "China" = "Asia - East Asia",
  "Hong Kong" = "Asia - East Asia",
  "Japan" = "Asia - East Asia",
  "Republic of Korea (South Korea)" = "Asia - East Asia",
  
  "India" = "Asia - South Asia",
  "Pakistan" = "Asia - South Asia",
  
  "Indonesia" = "Asia - Southeast Asia",
  "Malaysia" = "Asia - Southeast Asia",
  "Philippines" = "Asia - Southeast Asia",
  "Singapore" = "Asia - Southeast Asia",
  "Thailand" = "Asia - Southeast Asia",
  "Vietnam" = "Asia - Southeast Asia",
  
  "Iran" = "Asia - Middle East",
  "Israel" = "Asia - Middle East",
  "Saudi Arabia" = "Asia - Middle East",
  "United Arab Emirates" = "Asia - Middle East",
  "Turkey" = "Asia - Middle East",
  
  #EUROPE
  "Denmark" = "Europe - Northern Europe",
  "Finland" = "Europe - Northern Europe",
  "Norway" = "Europe - Northern Europe",
  "Sweden" = "Europe - Northern Europe",
  
  "France" = "Europe - Western Europe",
  "Germany" = "Europe - Western Europe",
  "Netherlands" = "Europe - Western Europe",
  "Switzerland" = "Europe - Western Europe",
  
  "Italy" = "Europe - Southern Europe",
  "Spain" = "Europe - Southern Europe",
  
  "Poland" = "Europe - Eastern Europe",
  "Russian Federation" = "Europe - Eastern Europe",
  
  "Ireland" = "Europe - Western Europe",
  "United Kingdom" = "Europe - Western Europe"
)


df <- df %>% mutate(Continent = continent_region_map[Country])
df$Region <- as.factor(df$Continent)
levels(df$Region) <- c("Africa", "Americas", "Americas", 
                       "Asia", "Asia", "Asia", 
                       "Asia", "Europe", "Europe", 
                       "Europe", "Europe")

df$Continent <- as.factor(df$Continent)
levels(df$Continent) <- c("AF", "AM - South", "AM - North", 
                          "AS - East", "AS - Middle East", "AS - South", 
                          "AS - Southeast", "EU - East", "EU - North", 
                          "EU - South", "EU - West")

my_colors <- c(
  "AF" = "#E41A1C",
  "AM - North" = "#D95F02", "AM - South" = "#E6AB02",
  "AS - East" = "#4169E1", "AS - Middle East" = "#008080", "AS - South" = "#00FFFF", "AS - Southeast" = "#AFEEEE",
  "EU - East" = "#4B0082", "EU - North" = "#9932CC", "EU - South" = "#EE82EE", "EU - West" = "#D8BFD8"
)

df_sub <- df
df_sub$Sales_in_Millions <- log(df_sub$Sales_in_Millions)


df %>% #Con sales in millions
  ggplot(aes(x = Depression_Rates, y = Sales_in_Millions)) +
  # Aggiungiamo i punti colorati per Continent
  geom_point(aes(color = Continent), alpha = 0.6, size = 2) +
  # Linea di regressione globale
  geom_smooth(method = "lm", color = "maroon", se = FALSE) + 
  labs(
    title = "Depression Rates and Video Game Sales",
    subtitle = "Analysis of lockdown's impact on sales",
    x = "Depression Rates",
    y = "Sales (Millions USD)",
    color = "Colors"
  ) +
  scale_color_manual(values = my_colors) +
  theme_dark() +
  theme(
    legend.title = element_text(size = 9, face = "bold"), 
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    legend.position = "right"
  )

df_sub %>% #Con log sales
  ggplot(aes(x = Depression_Rates, y = Sales_in_Millions)) +
  geom_point(aes(color = Continent), alpha = 0.6, size = 2) +
  # Linea di regressione globale
  geom_smooth(method = "lm", color = "maroon", se = FALSE) + 
  labs(
    title = "Depression Rates and Log Video Game Sales",
    subtitle = "Analysis of lockdown's impact on sales",
    x = "Depression Rates",
    y = "Log Sales (Millions USD)",
    color = "Colors"
  ) +
  scale_color_manual(values = my_colors) +
  theme_dark() +
  theme(
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    legend.position = "right"
  )

ggsave(filename = "LOG_Colored_Depression_Rates_&_VideoGames_Sales_Graph.png",
       width = 10, height = 6, units = "in", dpi = 300)
