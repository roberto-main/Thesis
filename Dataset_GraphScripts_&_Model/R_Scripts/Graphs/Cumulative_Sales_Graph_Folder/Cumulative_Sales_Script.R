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

df <- df[df$Year != 2014 & df$Year != 2025, ]

my_colors <- c(
  "AF" = "#E41A1C",
  "AM - North" = "#D95F02", "AM - South" = "#E6AB02",
  "AS - East" = "#4169E1", "AS - Middle East" = "#008080", "AS - South" = "#00FFFF", "AS - Southeast" = "#AFEEEE",
  "EU - East" = "#4B0082", "EU - North" = "#9932CC", "EU - South" = "#EE82EE", "EU - West" = "#D8BFD8"
)

str(df$Sales_Variations)
summary(df$Sales_Variations)
df$Year <- as.numeric(df$Year)


df <- df %>%
  group_by(Continent, Country) %>%
  arrange(Year) %>%
  mutate(
    Sales_Var_anno = coalesce(Sales_Variations, 0),
    Sales_Index = 100 * cumprod(1 + Sales_Var_anno)
  )


library(dplyr)
library(ggplot2)
library(stringr)


# CREAZIONE MACRO REGIONI


df <- df %>%
  mutate(
    Macro_Region = case_when(
      
      grepl("^AM", Continent)
      ~ "America",
      
      grepl("^EU", Continent)
      ~ "Europe",
      
      grepl("^AS", Continent)
      ~ "Asia",
      
      grepl("^AF", Continent)
      ~ "Africa",
      
    )
  )

df %>%
  filter(is.na(Macro_Region)) %>%
  distinct(Country, Continent)

# DATASET GRAFICO

unique(df$Continent)

df_plot <- df %>%
  group_by(Macro_Region, Country, Continent, Year) %>%
  summarise(
    Sales_Index_mean = mean(Sales_Index, na.rm = TRUE),
    .groups = "drop"
  )

# GRAFICO FINALE

ggplot(
  df_plot,
  aes(
    x = Year,
    y = Sales_Index_mean,
    color = Continent,
    group = Country
  )
) +
  
  geom_line(linewidth = 1.1, alpha = 0.6) +
  
  geom_point(size = 1.7, alpha = 0.6) +
  
  # Linee COVID
  geom_vline(
    xintercept = 2020,
    linetype = "dashed",
    color = "cyan"
  ) +
  
  geom_vline(
    xintercept = 2022,
    linetype = "dashed",
    color = "cyan"
  ) +
  
  facet_wrap(~ Macro_Region, scales = "free_y") +
  
  scale_x_continuous(
    breaks = seq(
      min(df$Year, na.rm = TRUE),
      max(df$Year, na.rm = TRUE),
      1
    )
  ) +
  
  labs(
    title = "Cumulative Sales Index by Macro Region",
    subtitle = "Base 100 in the first year",
    x = "Year",
    y = "Sales Index"
  ) +
  
  theme_dark() +
  
  scale_color_manual(values = my_colors) + 
  
  theme(
    strip.text = element_text(
      face = "bold",
      size = 12
    ),
    
    legend.position = "bottom"
  )

str(df)

#Nigeria GNI Variations Graph:
nigeria <- df[df$Country == "Nigeria", ]

nigeria$GNI_growth <- nigeria$GNI_per_Capita / lag(nigeria$GNI_per_Capita) - 1
nigeria$GNI_growth[nigeria$Year == 2014] <- 0
print(nigeria)

ggplot(nigeria, aes(x = Year, y = GNI_growth)) +
  geom_line(color = "#E41A1C", alpha = 0.6, linewidth = 1.1) +
  geom_point(color = "#E41A1C", alpha = 0.6, size = 1.7) +
  labs(
    title = "Nigeria GNI Growth Rate",
    x = "Year",
    y = "Growth rate"
  ) +
  scale_x_continuous(breaks = unique(nigeria$Year)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = length(nigeria$Year))) +
  theme_minimal()


#ggsave("C:\\Users\\rober\\OneDrive - Università degli Studi di Trieste\\File di DI CREDICO GIOIA - Maijnelli\\Analysis\\Roberto_Updates\\R_Scripts\\Graphs\\Cumulative_Sales_Graph_Folder\\Cumulative_Sales_Graph.png", 
 #      width = 8, height = 8)

#NIGERIA GNI DROP BELOW
ggsave("Nigeria_GNI_Drop.png", 
      width = 8, height = 8)


library(dplyr)

df %>%
  filter(Country %in% c("Germany", "France", "Netherlands", "Switzerland", "Ireland", "United Kingdom")) %>%
  group_by(Country) %>%
  summarise(mean_sales_variation = mean(`Sales_Variations`, na.rm = TRUE))
