library(GGally)
library(reshape)
library(dplyr)
library(tidyr)
library(tidyverse)
library(openxlsx)
library(readxl)
library(writexl)
library(corrplot)

df <- read_excel("Final_Dataset.xlsx", sheet="Countries")

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

my_colors <- c(
  "AF" = "#E41A1C",
  "AM - North" = "#D95F02", "AM - South" = "#E6AB02",
  "AS - East" = "#4169E1", "AS - Middle East" = "#008080", "AS - South" = "#00FFFF", "AS - Southeast" = "#AFEEEE",
  "EU - East" = "#4B0082", "EU - North" = "#9932CC", "EU - South" = "#EE82EE", "EU - West" = "#D8BFD8"
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

ìdf <- df[df$Year < 2025,]
df <- df %>%
  dplyr::rename(GNI_per_Capita = `GNI per Capita`)
df_sub <- df[,c("Country", "Year", "Depression_Rates", "GNI_per_Capita", 
                "Internet_Users", "Sales_in_Millions", "Unemployment", "Continent", 
                "Region", "Stingency_Index0")]
df_sub$Sales_in_Millions <- log(df_sub$Sales_in_Millions)
colnames(df_sub) <- make.names(colnames(df_sub))

df_sub <- df_sub %>%
  filter(!is.na(Continent))

ggpairs(df_sub %>%
          select(
            Year,
            Depression_Rates,
            GNI_per_Capita,
            Internet_Users,
            Sales_in_Millions,
            Unemployment,
            Stingency_Index0,
            Continent
          ),
        mapping = aes(color = Continent),
        
        lower = list(continuous = wrap("points", alpha = 1, size = 2)),
        
        upper = list(continuous = "box_no_facet"),
        
        diag = list(
          continuous = function(data, mapping, ...) {
            ggplot(data, mapping) +
              geom_density(fill = "grey70", alpha = 0.5, color = "black")
          }
)
) +
  scale_color_manual(values = my_colors)


ggsave("Colored_Correlation_Chart_Detailed_Graph.png", 
       width = 16, height = 10)

ggplot(df_sub, aes(x = Continent, fill = Continent)) +
  geom_bar() +
  scale_fill_manual(values = my_colors) +
  theme_minimal()

ggsave("Colored_Correlation_Chart_Regions.png", 
       width = 16, height = 10)
