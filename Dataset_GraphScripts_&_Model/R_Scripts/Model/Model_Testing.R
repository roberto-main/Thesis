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
                "Region", "Stingency_Index0")]
df_sub$Sales_in_Millions <- log(df_sub$Sales_in_Millions)
colnames(df_sub) <- make.names(colnames(df_sub))



library(lme4)

fit <- lm(Sales_in_Millions ~ Depression_Rates, data = df_sub)
summary(fit)

plot(df_sub$Depression_Rates, df_sub$Sales_in_Millions)
abline(a = coef(fit)[1], b = coef(fit)[2])
par(mfrow= c(2,2))
plot(fit)
par(mfrow= c(1,1))

df_sub$GNI_per_Capita <- df_sub$GNI_per_Capita/1000
fit <- lmer(Sales_in_Millions ~ Depression_Rates + Stingency_Index0 + 
              Unemployment + 
              (1|Continent), data = df_sub)
summary(fit)

df$GNI_per_Capita <- df$GNI_per_Capita/1000
df$Stingency_Index01 <- df$Stingency_Index0 !=0
df$Sales_in_Millions <- log(df$Sales_in_Millions)

fit <- lmer(Sales_in_Millions ~ Depression_Rates *Stingency_Index01 + 
              GNI_per_Capita + Internet_Users + Year + 
              (1|Continent/Code), data = df)
summary(fit)
par(mfrow= c(2,2))
plot(fit)
par(mfrow= c(1,1))

fit <- lmer(Sales_in_Millions ~ Depression_Rates *Stingency_Index01 + 
              GNI_per_Capita + Internet_Users + Unemployment + I(Unemployment^2) +
              (1|Continent/Code), data = df)
summary(fit)

ranef(fit)

library(car)
vif(fit)

#vif() calcola il Variance Inflation Factor, cioè misura la multicollinearità.
#In pratica controlla se alcune variabili esplicative e stanno dicendo quasi la stessa cosa.
#VIF	Significato
#1	perfetto
#1–5	ok
#>5	attenzione
#>10	problema serio
#In questo caso,  Stingency_Index01 = 16.824544 e Depression_Rates:Stingency_Index01 = 18.546884 sono le
#variabili problematiche

fit2 <- lmer(
  Sales_in_Millions ~
    Depression_Rates +
    Stingency_Index01 +
    GNI_per_Capita +
    Internet_Users +
    Unemployment +
    I(Unemployment^2) +
    (1|Continent/Code),
  
  data = df
)

vif(fit2)

df$GNI_z <- scale(df$GNI_per_Capita)
df$Internet_z <- scale(df$Internet_Users)
df$Depression_z <- scale(df$Depression_Rates)
df$Unemployment_z <- scale(df$Unemployment)
df$Gini_z <- scale(df$Gini)

fit3 <- lmer(
  Sales_in_Millions ~
    Depression_z +
    Stringency_Index01 +
    GNI_z +
    Internet_z +
    Unemployment_z +
    I(Unemployment_z^2) +
    (1|Continent/Code),
  
  data = df
)

vif(fit3)

fit4 <- lmer(
  Sales_in_Millions ~
    Depression_z +
    Stringency_Index01 +
    GNI_z +
    Internet_z +
    Unemployment_z +
    I(Unemployment_z^2) +
    factor(Year) +
    (1|Continent/Code),
  
  data = df
)

vif(fit4)

AIC(fit2, fit3, fit4)
BIC(fit2, fit3, fit4)
#FIT4 probabile modello migliore
summary(fit4)

fit1_test <- lmer(Sales_in_Millions ~ 
                    Depression_Rates *Stringency_Index01 + 
              GNI_per_Capita + 
                Internet_Users + 
                Unemployment + 
                I(Unemployment^2) +
                factor(Year) +
              (1|Continent/Code), 
              data = df)
summary(fit1_test)

fit5 <- lmer(
  Sales_in_Millions ~
    Depression_z +
    GNI_z +
    Internet_z +
    Unemployment_z +
    I(Unemployment_z^2) +
    factor(Year) +
    (1|Continent/Code),
  
  data = df
)

vif(fit5)
summary(fit5)

fit44 <- lmer(
  Sales_in_Millions ~
    Depression_z +
    Stringency_Index01 +
    GNI_z +
    Internet_z +
    Unemployment_z +
    I(Unemployment_z^2) +
    (1|Continent/Code),
  
  data = df
)
vif(fit44)
summary(fit4)
summary(fit44)


AIC(fit4, fit5, fit44)
BIC(fit4, fit5, fit44)
AIC(fit44)
BIC(fit44)

#PER RENDERE PIÙ CHIARE A PRIMO IMPATTO LE SPIEGAZIONI DEL FILE 'Note per la scelta del modello'
#RINOMINIAMO I MODELLI 5,4,44 IN mod_('a,b,c').
mod_a <- fit5
mod_b <- fit44
mod_c <- fit4
AIC(mod_a, mod_b, mod_c)
BIC(mod_a, mod_b, mod_c)
AIC(mod_a, mod_c)
BIC(mod_a, mod_c)
AIC(mod_b)
BIC(mod_b)
summary(mod_a)
summary(mod_c)


# Test se il termine quadratico è davvero necessario
fit4_no_quad <- lmer(
  Sales_in_Millions ~
    Depression_z +
    Stringency_Index01 +
    GNI_z +
    Internet_z +
    Unemployment_z +  # Rimuovi quadratico
    factor(Year) +
    (1|Continent/Code),
  data = df
)

fit4_no_unemp <- lmer(
  Sales_in_Millions ~
    Depression_z +
    Stringency_Index01 +
    GNI_z +
    Internet_z +
    factor(Year) +
    (1|Continent/Code),
  data = df
)

anova(fit4, fit4_no_quad, fit4_no_unemp)  # Likelihood ratio test
#Sembra che sia meglio rimuovere unemployment

fit4_improved <-  lmer(
  Sales_in_Millions ~
    # Interazioni COVID
    Stringency_Index01*Internet_z +
    Stringency_Index01*GNI_z +
    Stringency_Index01*Depression_z +
    #(Li ho provati anche singolarmente ma nessuno riesce a superare fit4_no_unemp)
    factor(Year) +
    (1|Continent/Code),
  
  data = df
)


anova(fit4_no_unemp, fit4_improved)


#DUNQUE fit4_no_unemp SEMBRA ESSERE IL MODELLO MIGLIORE
#Facciamo un ultimo test per vedere quanto escludere il Gini index sia una cattiva idea

df_model <- df %>%
  filter(Year <= 2022) #Per escludere tutti gli NA di Gini

fit4_no_unemp_ConGini <- lmer(
  Sales_in_Millions ~
    Depression_z +
    Stringency_Index01 +
    GNI_z +
    Internet_z +
    Gini_z +
    factor(Year) +
    (1|Continent/Code),
  data = df_model
)

fit4_no_unemp2 <- lmer(
  Sales_in_Millions ~
    Depression_z +
    Stringency_Index01 +
    GNI_z +
    Internet_z +
    factor(Year) +
    (1|Continent/Code),
  data = df_model
)

anova(fit4_no_unemp2, fit4_no_unemp_ConGini)

#Confermiamo dunque che fit4_no_unemp sembra essere il modello migliore.