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
library(plotly)

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
    (GNI_per_Capita) +
    s(Continent, bs="re"),
  family = gaussian,
  method = "REML",
  data = df2
)

summary(final)
plot(final)


# Crea una griglia di valori per Year e Depression_Rates
year_seq <- seq(2015, max(df2$Year), length.out = 50)
dep_seq  <- seq(min(df2$Depression_Rates, na.rm = T), max(df2$Depression_Rates, na.rm = T), length.out = 50)

griglia <- expand.grid(
  Year             = year_seq,
  Depression_Rates = dep_seq
)

# Fissa le altre variabili ai loro valori medi/referenza
griglia$Internet_Users <- mean(df2$Internet_Users, na.rm = TRUE)
griglia$Unemployment   <- mean(df2$Unemployment,   na.rm = TRUE)
griglia$GNI_per_Capita <- mean(df2$GNI_per_Capita, na.rm = TRUE)
griglia$Continent      <- names(sort(table(df2$Continent), decreasing = TRUE))[1]  # continente più frequente

# Predici con il termine te() isolato (exclude tutti gli altri smooth)
pred <- predict(
  final,
  newdata  = griglia,
  type     = "terms",
  terms    = "te(Year,Depression_Rates)"
)

# pred è una matrice: prendi la colonna del termine te()
griglia$z <- as.numeric(pred[, "te(Year,Depression_Rates)"])

# Converti in matrice per la surface
Z_mat <- matrix(griglia$z, nrow = length(year_seq), ncol = length(dep_seq))

# Grafico 3D con Plotly
fig <- plot_ly(
  x = year_seq,
  y = dep_seq,
  z = Z_mat,
  type       = "surface",
  colorscale = "Viridis",
  showscale  = TRUE
) |>
  layout(
    title = "Bivariate Spline: te(Year, Depression_Rates)",
    scene = list(
      xaxis = list(title = "Year"),
      yaxis = list(title = "Depression Rates"),
      zaxis = list(title = "Effetto parziale su log(Sales)")
    )
  )

fig

