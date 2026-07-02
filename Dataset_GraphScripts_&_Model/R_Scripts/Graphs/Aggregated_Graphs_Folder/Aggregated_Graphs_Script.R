library(reshape)
library(dplyr)
library(tidyr)
library(tidyverse)
library(openxlsx)
library(readxl)
library(writexl)
library(corrplot)

# Lettura dati
df <- read_excel("Final_Dataset.xlsx", sheet="Countries")
str(df)
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

df <- df[df$Year < 2025,]
df <- df %>%
  dplyr::rename(GNI_per_Capita = `GNI per Capita`)
df_sub <- df[,c("Country", "Year", "Depression_Rates", "GNI_per_Capita", 
                "Internet_Users", "Sales_in_Millions", "Unemployment", "Continent", 
                "Region", "Stingency_Index0", "Sales_per_Capita")]
df_sub$Sales_in_Millions <- log(df_sub$Sales_in_Millions)
colnames(df_sub) <- make.names(colnames(df_sub))

df_melt <- df_sub %>%
  pivot_longer(
    cols = -c(Country, Continent, Region, Year),
    names_to = "variable",
    values_to = "value"
  )
df_melt <- df_melt[df_melt$Year > 2014,]

my_colors <- c(
  "AF" = "#E41A1C",
  # 🌎 AMERICAS (arancio → giallo scuro)
  "AM - North" = "#D95F02",
  "AM - South" = "#E6AB02",
  
  # 🌏 ASIA (blu → turchese)
  "AS - East" = "#4169E1",
  "AS - Middle East" = "#008080",
  "AS - South" = "#00FFFF",
  "AS - Southeast" = "#AFEEEE",
  
  # 🌍 EUROPE (viola → rosa)
  "EU - East" = "#4B0082",
  "EU - North" = "#9932CC",
  "EU - South" = "#EE82EE",
  "EU - West" = "#D8BFD8"
)
df_melt$variable <- as.factor(df_melt$variable)
levels(df_melt$variable) <- c("Depr", "GNI", "Internet", "Log_Sales", "Sales_Capita", 
                              "String","Unempl")
df_melt$variable <- relevel(df_melt$variable, "Log_Sales")
ggplot(df_melt, aes(x = Year, y = value, color = Continent, group = Country)) +
  geom_line(linewidth = .5, alpha = 0.6) +
  geom_point(size = .5) +
  facet_grid(variable ~ Region, scales = "free_y") +
  scale_x_continuous(breaks = unique(df_melt$Year)) +
  scale_color_manual(values = my_colors) +
  ylab("") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10),
    #panel.spacing = unit(1, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8) # Ruota anni per leggibilità
  )

# Controllo della consistenza del calcolo
df %>%
  mutate(check = Sales_in_Millions / Population_In_Millions) %>%
  select(Continent, Year, Sales_per_Capita, check) %>%
  head(20)


ggsave(filename = "Aggregated_Graphs_Graph2.png",
       width = 8, height = 8, units = "in", dpi = 600)

library(GGally)
df_sub$Stingency_Index01 <- df_sub$Stingency_Index0!=0
ggpairs(df_sub[,-c(1,2,9,10)])



