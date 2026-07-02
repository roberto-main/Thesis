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

# Mappatura continenti/regioni
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

# Creazione variabili Continent e Region
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
  #AMERICAS
  "AM - North" = "#D95F02",
  "AM - South" = "#E6AB02",
  
  #ASIA
  "AS - East" = "#4169E1",
  "AS - Middle East" = "#008080",
  "AS - South" = "#00FFFF",
  "AS - Southeast" = "#AFEEEE",
  
  #EUROPE
  "EU - East" = "#4B0082",
  "EU - North" = "#9932CC",
  "EU - South" = "#EE82EE",
  "EU - West" = "#D8BFD8"
)
df_sub <- df
df_sub$Sales_in_Millions <- log(df_sub$Sales_in_Millions)

df %>% #Con sales in millions
  ggplot(aes(x = Unemployment, y = Sales_in_Millions)) +
  geom_point(aes(color = Continent), alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", color = "maroon", se = FALSE) + 
  labs(
    title = "Unemployment and Video Game Sales",
    x = "Unemployment Rate",
    y = "Sales (Millions USD)",
    color = "Colors"
  ) +
  scale_color_manual(values = my_colors) +
  theme_dark() +
  theme(
    # Riduce la dimensione del titolo della legenda
    legend.title = element_text(size = 9, face = "bold"), 
    
    # Riduce la dimensione delle etichette (AF, EU, ecc.)
    legend.text = element_text(size = 8),
    
    # Opzionale: riduce la dimensione dei quadratini colorati (le chiavi)
    legend.key.size = unit(0.4, "cm") 
  )

df_sub %>% #Con log sales
  ggplot(aes(x = Unemployment, y = Sales_in_Millions)) +
  geom_point(aes(color = Continent), alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", color = "maroon", se = FALSE) + 
  labs(
    title = "Unemployment and Video Game Sales",
    x = "Unemployment Rate",
    y = "Log of Sales (Millions USD)",
    color = "Colors"
  ) +
  scale_color_manual(values = my_colors) +
  theme_dark() +
  theme(
    # Riduce la dimensione del titolo della legenda
    legend.title = element_text(size = 9, face = "bold"), 
    
    # Riduce la dimensione delle etichette (AF, EU, ecc.)
    legend.text = element_text(size = 8),
    
    # Opzionale: riduce la dimensione dei quadratini colorati (le chiavi)
    legend.key.size = unit(0.4, "cm") 
  )


ggsave(filename = "LOG_Colored_Unemployement_&_Sales_Graph.png",
       width = 7, height = 4, units = "in", dpi = 600)
