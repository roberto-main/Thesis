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
df$GNI_per_Capita <- df$GNI_per_Capita/1000
df$Stringency_Index01 <- df$Stringency_Index0 !=0
df$Sales_in_Millions <- log(df$Sales_in_Millions) # lavoriamo in scala log

colnames(df) <- make.names(colnames(df))

### Modello lineare con Sales_in_Millions sulla scala log
fit.lm <- lm(Sales_in_Millions ~ Depression_Rates, data = df)
summary(fit.lm)

plot(df$Depression_Rates, df$Sales_in_Millions)
abline(a = coef(fit.lm)[1], b = coef(fit.lm)[2])
par(mfrow= c(2,2))
plot(fit.lm)
par(mfrow= c(1,1))

## Multilevel continent
fit <- lmer(Sales_in_Millions ~ Depression_Rates + Stringency_Index0 + 
              Unemployment + 
              (1|Continent), data = df)
summary(fit)

## Multilevel country/continent
fit2 <- lmer(Sales_in_Millions ~ Depression_Rates *Stringency_Index01 + 
              GNI_per_Capita + Internet_Users + Year + 
              (1|Continent/Code), data = df)
summary(fit2)
par(mfrow= c(2,2))
plot(fit2)
par(mfrow= c(1,1))

ranef(fit2)
coefplot(fit2, intercept = F)
plot(ranef(fit, level = 1))



fit4_no_unemp2 <- lmer(
  Sales_in_Millions ~
    Depression_Rates +
    Stringency_Index01 +
    GNI_per_Capita +
    Internet_Users +
    factor(Year) +
    (1|Continent/Code),
  data = df
)
summary(fit4_no_unemp2)
par(mfrow= c(2,2))
plot(fit4_no_unemp2)
par(mfrow= c(1,1))

ranef(fit4_no_unemp2)
coefplot(fit4_no_unemp2)


fit.gam <- gamm(Sales_in_Millions ~
                 #s(Depression_Rates) +
                 Stringency_Index0 +
                 s(GNI_per_Capita, Internet_Users) +
                 # s(GNI_per_Capita) +
                 #s(Internet_Users) +
                 s(Year, Depression_Rates, k = 4),
                random = list(Continent = Continent ~ 1,
                              Code = Code ~ 1),
                groups = ~ Continent / Code,
               data = df)
summary(fit.gam$lme)
summary(fit.gam$gam)
plot(fit.gam$gam)
ranef(fit.gam$lme)

fit.gam1 <- gamm(Sales_in_Millions ~
                  Depression_Rates +
                  #Stringency_Index01 +
                  s(GNI_per_Capita, Internet_Users) +
                  # s(GNI_per_Capita) +
                  #s(Internet_Users) +
                  s(Year, k = 4),
                random = list(Continent = Continent ~ 1,
                              Code = Code ~ 1),
                groups = ~ Continent / Code,
                data = df)
summary(fit.gam1$lme)
summary(fit.gam1$gam)
plot(fit.gam1$gam)

AIC(fit.gam$lme)
AIC(fit.gam1$lme)
