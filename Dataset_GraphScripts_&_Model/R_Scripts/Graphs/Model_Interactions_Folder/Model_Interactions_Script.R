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
library(gratia)
library(patchwork)

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


p <- recordPlot()
dev.copy(png, "s_Internet_Users_Simple.png",
width = 1200, height = 800, res = 150)
dev.off()

plots <- draw(final, residuals = TRUE, ci_level = 0.95)


p1 <- plots[[1]]
p2 <- plots[[2]]
p3 <- plots[[3]]
p4 <- plots[[4]]

p1 <- p1 + scale_y_continuous(n.breaks = 16)
p2 <- p2 + scale_y_continuous(n.breaks = 16)
p3 <- p3 + scale_y_continuous(n.breaks = 16)
p4 <- p4 + scale_y_continuous(n.breaks = 16)

combined <- (p1 | p2) / (p3 | p4) &
  theme_minimal(base_size = 13) &
  theme(plot.title = element_text(face = "bold"))

p1

ggsave("s_Internet_Users.png",
       plot = p1,
       width = 12,
       height = 8,
       dpi = 300,
       bg = "white")
