library(reshape)
library(dplyr)
library(tidyr)
library(tidyverse)
library(openxlsx)
library(readxl)
library(writexl)
library(corrplot)
library(lme4)
library(mgcv)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)

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
df$Stringency_Index01 <- df$Stringency_Index0 !=0
df2 <- df #In modo da tenere la versione senza log sales
df$Sales_in_Millions <- log(df$Sales_in_Millions) # lavoriamo in scala log

colnames(df) <- make.names(colnames(df))
colnames(df2) <- make.names(colnames(df2))

final <- gam(
  log(Sales_in_Millions) ~
    s(Internet_Users) +
    te(Year, Depression_Rates, k=c(4,4)) +
    s(Unemployment) +
    GNI_per_Capita +
    s(Continent, bs="re"),
  family = gaussian,
  method = "REML",
  data = df2
)


re_continent <- data.frame(
  Continent = levels(df2$Continent),
  re_value  = coef(final)[grep("Continent", names(coef(final)))]
)

country_re <- df2 %>%
  select(Country, Continent) %>%
  distinct() %>%
  left_join(re_continent, by = "Continent")




world <- ne_countries(scale = "medium", returnclass = "sf")

country_re <- country_re %>%
  mutate(Country = recode(Country,
                          "Republic of Korea (South Korea)" = "Republic of Korea"))

world_data <- world %>%
  left_join(country_re, by = c("name_long" = "Country"))

ggplot(world_data) +
  geom_sf(aes(fill = re_value), color = "white", linewidth = 0.1) +
  scale_fill_gradient2(
    low = "steelblue", mid = "lightyellow", high = "firebrick",
    midpoint = 0, na.value = "grey85",
    name = "Random Effect"
  ) +
  theme_void()

ggsave(filename = "Random_Effect_Map.png",
       width = 7, height = 4, units = "in", dpi = 600)
