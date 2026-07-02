library(dplyr)
library(readxl)
library(writexl)

df <- read_excel("Final_Dataset", sheet = 1)


#MAPPA PAESE -> CONTINENTE
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

#AGGREGAZIONE 
# NOTA: la weighted_mean usa i pesi della riga originale (pre-somma), corretto
weighted_mean_safe <- function(x, w) {
  mask <- !is.na(x) & !is.na(w) & w > 0
  if (sum(mask) == 0) return(NA_real_)
  weighted.mean(x[mask], w[mask])
}

df_continent <- df %>%
  group_by(Continent, Year) %>%
  summarise(
    #Contatori di quanti paesi hanno contribuito (utili per valutare affidabilità)
    #N_countries_total        = n(),
    #N_countries_sales        = sum(!is.na(Sales_in_Millions)),
    #N_countries_gini         = sum(!is.na(Gini)),
    #N_countries_pop          = sum(!is.na(Population_In_Millions)),
    #N_countries_stringency   = sum(!is.na(Stringency_Index)),
    #N_countries_depression   = sum(!is.na(Depression_Rates)),
    #N_countries_gni         = sum(!is.na(GNI_per_Capita)),
    
    # Aggregazioni
    Gini                     = weighted_mean_safe(Gini, Population_In_Millions),
    Depression_Rates         = weighted_mean_safe(Depression_Rates, Population_In_Millions),
    GNI_per_Capita           = weighted_mean_safe(GNI_per_Capita, Population_In_Millions),
    Internet_Users           = weighted_mean_safe(Internet_Users, Population_In_Millions),
    Population_in_Millions   = sum(Population_In_Millions, na.rm = TRUE),
    Sales_in_Millions        = sum(Sales_in_Millions, na.rm = TRUE),
    Stringency_Index         = weighted_mean_safe(Stringency_Index, Population_In_Millions),
    Unemployment             = weighted_mean_safe(Unemployment, Population_In_Millions),
    .groups = "drop"
  )
df_continent <- subset(df_continent, Year != 2025)

write_xlsx(df_continent, "Countries_To_Regions.xlsx")
