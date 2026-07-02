library(dplyr)
library(readxl)
library(writexl)

df <- read_excel("Final_Dataset", sheet = 1)


#MAPPA PAESE -> CONTINENTE
continent_map <- c(
  "NGA" = "Africa", "ZAF" = "Africa", "EGY" = "Africa",
  "CAN" = "Americas", "USA" = "Americas",
  "CHL" = "Americas", "COL" = "Americas", "MEX" = "Americas",
  "CHN" = "Asia", "HKG" = "Asia", "JPN" = "Asia", "KOR" = "Asia",
  "IDN" = "Asia", "MYS" = "Asia", "PHL" = "Asia", "SGP" = "Asia", "THA" = "Asia", "VNM" = "Asia",
  "IND" = "Asia", "PAK" = "Asia",
  "IRN" = "Asia", "ISR" = "Asia", "SAU" = "Asia", "ARE" = "Asia", "TUR" = "Asia",
  "FRA" = "Europe", "DEU" = "Europe", "IRL" = "Europe", "ITA" = "Europe",
  "NLD" = "Europe", "ESP" = "Europe", "CHE" = "Europe", "GBR" = "Europe",
  "DNK" = "Europe", "FIN" = "Europe", "NOR" = "Europe", "SWE" = "Europe",
  "POL" = "Europe", "RUS" = "Europe"
)

df <- df %>% mutate(Continent = continent_map[Code])

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

write_xlsx(df_continent, "Countries_To_Continent.xlsx")
