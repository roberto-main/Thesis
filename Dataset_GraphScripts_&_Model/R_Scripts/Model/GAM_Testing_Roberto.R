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


fit.sgam <- gamm(Sales_in_Millions ~
                  #s(Depression_Rates) +
                  Stringency_Index0 +
                  s(GNI_per_Capita, Internet_Users) +
                  #s(GNI_per_Capita) +
                  #s(Internet_Users) +
                  s(Year, Depression_Rates, k = 4),
                random = list(Continent = Continent ~ 1,
                              Code = Code ~ 1),
                groups = ~ Continent / Code,
                data = df)

fit.tegam <- gamm(Sales_in_Millions ~
                  #te(Depression_Rates) +
                  Stringency_Index0 +
                  te(GNI_per_Capita, Internet_Users) +
                  #te(GNI_per_Capita) +
                  #te(Internet_Users) +
                  te(Year, Depression_Rates, k = c(4, 9)),
                random = list(Continent = Continent ~ 1,
                              Code = Code ~ 1),
                groups = ~ Continent / Code,
                data = df)
                
AIC(fit.sgam$lme, fit.tegam$lme)
                
summary(fit.gam$lme)
summary(fit.gam$gam)
plot(fit.gam$gam)
ranef(fit.gam$lme)

fit.sgam1 <- gamm(Sales_in_Millions ~
                   Depression_Rates +
                   #Stringency_Index01 +
                   s(GNI_per_Capita, Internet_Users) +
                   #s(GNI_per_Capita) +
                   #s(Internet_Users) +
                   s(Year, k = 4),
                 random = list(Continent = Continent ~ 1,
                               Code = Code ~ 1),
                 groups = ~ Continent / Code,
                 data = df)

fit.tegam1 <- gamm(Sales_in_Millions ~
                    Depression_Rates +
                    #Stringency_Index01 +
                    te(GNI_per_Capita, Internet_Users) +
                    #te(GNI_per_Capita) +
                    #te(Internet_Users) +
                    te(Year, k = 4),
                  random = list(Continent = Continent ~ 1,
                                Code = Code ~ 1),
                  groups = ~ Continent / Code,
                  data = df)
summary(fit.gam1$lme)
summary(fit.gam1$gam)
plot(fit.gam1$gam)
plot(fit.sgam1$gam)
plot(fit.tegam1$gam)

AIC(fit.gam$lme)
AIC(fit.sgam$lme, fit.tegam$lme)
AIC(fit.sgam1$lme, fit.tegam1$lme)


#Notiamo dunque come 'fit.tegam' sia il modello migliore finora.
#Continuiamo il testing con questo modello.

fit.tegam2 <- gamm(Sales_in_Millions ~
                    #(Depression_Rates) +
                    #Stringency_Index0 +
                    #te(GNI_per_Capita, Internet_Users) +
                    te(GNI_per_Capita) +
                    te(Internet_Users) +
                    te(Year, Depression_Rates, k = c(4, 4)),
                  random = list(Continent = Continent ~ 1,
                                Code = Code ~ 1),
                  groups = ~ Continent / Code,
                  data = df)

AIC(fit.tegam$lme, fit.tegam2$lme) #Meglio fit.tegam2
names(df)

fit.tegam3 <- gamm(Sales_in_Millions ~
                     #(Depression_Rates) +
                     Stringency_Index0 +
                     #te(GNI_per_Capita, Internet_Users) +
                     te(GNI_per_Capita) +
                     te(Internet_Users) +
                     te(Year, Depression_Rates, k = c(4, 4)),
                   random = list(Continent = Continent ~ 1,
                                 Code = Code ~ 1),
                   groups = ~ Continent / Code,
                   data = df)

AIC(fit.tegam2$lme, fit.tegam3$lme)

s(GNI_per_Capita, bs = "cr")   # stessa base di te()
te(GNI_per_Capita)              # cubic regression spline di default


fit.tegam4 <- gamm(Sales_in_Millions ~
                     #(Depression_Rates) +
                     Stringency_Index0 +
                     #te(GNI_per_Capita, Internet_Users) +
                     s(GNI_per_Capita, bs = "cr") +  # stessa base di te()
                     s(Internet_Users, bs = "cr") +  # stessa base di te()
                   te(Year, Depression_Rates, k = c(4, 4)),
                   random = list(Continent = Continent ~ 1,
                                 Code = Code ~ 1),
                   groups = ~ Continent / Code,
                   data = df)

AIC(fit.tegam3$lme, fit.tegam4$lme)


fit.tegam5 <- gamm(Sales_in_Millions ~
                     #(Depression_Rates) +
                     Stringency_Index0 +
                     #te(GNI_per_Capita, Internet_Users) +
                     GNI_per_Capita + 
                     Internet_Users + 
                     te(Year, Depression_Rates, k = c(4, 4)),
                   random = list(Continent = Continent ~ 1,
                                 Code = Code ~ 1),
                   groups = ~ Continent / Code,
                   data = df)

AIC(fit.tegam4$lme, fit.tegam5$lme)

#fit.tegam4 sembra essere il migliore finora
names(df)

fit.tegam6 <- gamm(Sales_in_Millions ~               
                          Stringency_Index0 +          
                          #s(GNI_per_Capita, bs = "cr", k = 6) +
                          s(Internet_Users, bs = "cr", k = 6) +
                          #s(Unemployment, bs = "cr", k = 6) +
                          te(GNI_per_Capita, Unemployment, k = c(4, 4)) +
                          #s(Gini, bs = "cr", k = 5) +
                          te(Year, Depression_Rates, k = c(4, 4)),
                        random = list(Continent = ~ 1, Code = ~ 1),
                        groups = ~ Continent / Code,
                        data = df)

fit.sgam6 <- gamm(Sales_in_Millions ~             
                     Stringency_Index0 +           
                     s(GNI_per_Capita, bs = "cr", k = 4) +
                     s(Internet_Users, bs = "cr", k = 6) +
                     #s(Unemployment, bs = "cr", k = 4) + 
                     #te(GNI_per_Capita, Unemployment) +
                     #s(Gini, bs = "cr", k = 5) +
                     s(Year, Depression_Rates, k = 4),
                   random = list(Continent = ~ 1, Code = ~ 1),
                   groups = ~ Continent / Code,
                   data = df)

fit.tegam7 <- gamm(Sales_in_Millions ~             
                       Stringency_Index0 +           
                       s(GNI_per_Capita, bs = "cr", k = 4) +
                       s(Internet_Users, bs = "cr", k = 6) +
                       te(Year, Depression_Rates, k = c(4, 4)),
                     random = list(Continent = ~ 1, Code = ~ 1),
                     groups = ~ Continent / Code,
                     data = df)

AIC(fit.sgam6$lme, fit.tegam7$lme, fit.tegam6$lme)

#Nonostante l'AIC migliore sia quello di 'sgam6', credo che sarebbe più corretto utilizzare 'tegam7'
#in quanto nonostante abbia un AIC (leggermente) più basso, con l'uso di te() per l'interazione tra
#Year e Depression_Rates valuta le due variabili sulla stessa scala.

plot(fit.sgam6$gam)
plot(fit.tegam7$gam)

#Anche dando un'occhiata al grafico con le Splines tra Year e Depression Rates, quello con 'te()'
#risulta più valido rispetto a quello con 's()'
summary(fit.tegam7$gam)


fit.tegam7_NO_Log <- gamm(Sales_in_Millions ~             
                     Stringency_Index0 +           
                     s(GNI_per_Capita, bs = "cr", k = 4) +
                     s(Internet_Users, bs = "cr", k = 6) +
                     te(Year, Depression_Rates, k = c(4, 4)),
                   random = list(Continent = ~ 1, Code = ~ 1),
                   groups = ~ Continent / Code,
                   data = df2)

summary(fit.tegam7_NO_Log$gam)

hist(df2$Sales_in_Millions)
boxplot(df2$Sales_in_Millions)

library(gamm4)
# Opzione 2 - fmamiglia Gamma, appropriata per dati positivi asimmetrici
modello <- gamm4(Sales_in_Millions ~ Stringency_Index0 + 
                 s(GNI_per_Capita, bs = "cr", k = 4) + 
                 s(Internet_Users, bs = "cr", k = 6) + 
                 t2(Year, Depression_Rates, k = c(4, 4)),
               family = Gamma(link = "log"),
               data = df2, method = "REML")

summary(modello)

modello2 <- gam(Sales_in_Millions ~ s(Stringency_Index0, bs = "cr", k = 4) + 
                  s(GNI_per_Capita, bs = "cr", k = 4) + 
                  s(Internet_Users, bs = "cr", k = 6) + 
                  te(Year, Depression_Rates, k = c(4, 4)),
                family = Gamma(link = "log"),
                data = df2, method = "REML")
summary(modello$lme)
AIC(modello)
AIC(modello, modello2)
AIC(fit.tegam7_NO_Log$lme, fit.tegam7$lme)

names(df2)
df2$Country <- as.factor(df2$Country)
df2$Code <- as.factor(df2$Code)

class(df2$Continent)
class(df2$Country)

modello_re <- gam(Sales_in_Millions ~ Stringency_Index0 + 
                    s(GNI_per_Capita, bs = "cr", k = 4) + 
                    s(Internet_Users, bs = "cr", k = 6) + 
                    te(Year, Depression_Rates, k = c(4, 4)) +
                    s(Continent, bs = "re") +
                    s(Country, bs = "re"),
                  family = Gamma(link = "log"),
                  data = df2, method = "REML")

summary(modello_re)

modello_re2 <- gam(Sales_in_Millions ~ Stringency_Index0 + 
                    s(GNI_per_Capita, bs = "cr", k = 4) + 
                    s(Internet_Users, bs = "cr", k = 6) + 
                    te(Year, Depression_Rates, k = c(4, 4)) +
                    s(Continent, bs = "re"),
                  family = Gamma(link = "log"),
                  data = df2, method = "REML")

summary(modello_re2)

AIC(modello_re, modello_re2)
modello_fe <- gam(Sales_in_Millions ~ Stringency_Index0 + 
                    Continent +
                    s(GNI_per_Capita, bs = "cr", k = 4) + 
                    s(Internet_Users, bs = "cr", k = 6) + 
                    te(Year, Depression_Rates, k = c(4, 4)),
                  family = Gamma(link = "log"),
                  data = df2, method = "REML")

summary(modello_fe)

modello_fe2 <- gam(Sales_in_Millions ~ Continent + 
                     s(GNI_per_Capita, bs = "cr", k = 4) + 
                     s(Internet_Users, bs = "cr", k = 6) + 
                     te(Year, Depression_Rates, k = c(4, 4)),
                   family = Gamma(link = "log"),
                   data = df2, method = "REML")

AIC(modello_fe, modello_fe2)
gam.check(modello_fe2)

modello_fe3 <- gam(log(Sales_in_Millions) ~ Continent + 
                     s(GNI_per_Capita, bs = "cr", k = 4) + 
                     s(Internet_Users, bs = "cr", k = 6) + 
                     te(Year, Depression_Rates, k = c(4, 4)),
                   family = gaussian,
                   data = df2, method = "REML")

gam.check(modello_fe3)
summary(modello_fe3)

modello_fe4 <- gam(log(Sales_in_Millions) ~ Continent + 
                     s(GNI_per_Capita, bs = "cr", k = 8) + 
                     s(Internet_Users, bs = "cr", k = 6) + 
                     te(Year, Depression_Rates, k = c(6, 6)),
                   family = gaussian,
                   data = df2, method = "REML")

gam.check(modello_fe4)
summary(modello_fe4)


modello_fe5 <- gam(log(Sales_in_Millions) ~ 
                     s(GNI_per_Capita, bs = "cr", k = 8) + 
                     s(Internet_Users, bs = "cr", k = 6) + 
                     te(Year, Depression_Rates, k = c(6, 6)),
                   family = gaussian,
                   data = df2, method = "REML")

summary(modello_fe5)
gam.check(modello_fe5)

ayo <- gam(
  Sales_in_Millions ~ Stringency_Index0 +
    s(GNI_per_Capita, bs="cr", k=4) +
    s(Internet_Users, bs="cr", k=6) +
    te(Year, Depression_Rates, k=c(4,4)) +
    s(Continent, bs="re") +
    s(Code, bs="re"),
  family = Gamma(link="log"),
  method = "REML",
  data = df2
)

summary(ayo)

ayo2 <- gam(
  Sales_in_Millions ~
    Stringency_Index0 +
    s(GNI_per_Capita, bs = "cr", k = 4) +
    s(Internet_Users, bs = "cr", k = 6) +
    te(Year, Depression_Rates, k = c(4,4)) +
    s(Continent, bs = "re"),
  family = Gamma(link = "log"),
  method = "REML",
  data = df2
)

summary(ayo2)

ayo3 <- gam(
  Sales_in_Millions ~
    Stringency_Index0 +
    Internet_Users +
    te(Year, Depression_Rates, k=c(4,4)) +
    s(Continent, bs="re"),
  family=Gamma(link="log"),
  method="REML",
  data=df2
)

summary(ayo3)

AIC(ayo3, modello_fe5)
ayo3_log <- gam(
  log(Sales_in_Millions) ~
    Stringency_Index0 +
    Internet_Users +
    te(Year, Depression_Rates, k=c(4,4)) +
    s(Continent, bs="re"),
  family = gaussian,
  method = "REML",
  data = df2
)

AIC(ayo3_log, modello_fe5)
summary(ayo3_log)

ayo4_log <- gam(
  log(Sales_in_Millions) ~
    Internet_Users +
    te(Year, Depression_Rates, k=c(4,4)) +
    s(Continent, bs="re"),
  family = gaussian,
  method = "REML",
  data = df2
)

fit.tegam7 <- gamm(Sales_in_Millions ~             
                     Stringency_Index0 +           
                     s(GNI_per_Capita, bs = "cr", k = 4) +
                     s(Internet_Users, bs = "cr", k = 6) +
                     te(Year, Depression_Rates, k = c(4, 4)),
                   random = list(Continent = ~ 1, Code = ~ 1),
                   groups = ~ Continent / Code,
                   data = df)

ayo4 <- gam(
  (Sales_in_Millions) ~
    Internet_Users +
    te(Year, Depression_Rates, k=c(4,4)) +
    s(Continent, bs="re"),
  family = gaussian,
  method = "REML",
  data = df2
)

AIC(ayo3_log, ayo4_log)
summary(ayo4_log)
gam.check(ayo4)
gam.check(ayo4_log)

summary(fit.tegam7$gam)
summary(ayo4_log)

plot(df2$Internet_Users, log(df2$Sales_in_Millions), 
     xlab="Internet Users", ylab="log(Sales)",
     col=my_colors[df2$Continent], pch=19)


ayo5_log <- gam(
  log(Sales_in_Millions) ~
    te(Year, Depression_Rates, k=c(4,4)) +
    s(Continent, bs="re"),
  family = gaussian,
  method = "REML",
  data = df2
)

summary(ayo5_log)
AIC(ayo4_log, ayo5_log)

ayo5b_log <- gam(
  log(Sales_in_Millions) ~
    s(Internet_Users, bs="cr", k=6) +
    te(Year, Depression_Rates, k=c(4,4)) +
    s(Unemployment, bs="cr", k=6) +
    s(GNI_per_Capita, bs="cr", k=6) +
    s(Continent, bs="re"),
  family = gaussian,
  method = "REML",
  data = df2
)

AIC(ayo4_log, ayo5b_log)
summary(ayo5b_log) #MODELLO MIGLIORE

ayo7_log <- gam(
  log(Sales_in_Millions) ~
    s(Internet_Users, bs="cr", k=6) +
    s(GNI_per_Capita, bs="cr", k=6) +
    s(Stringency_Index0, bs="cr", k=4) +
    te(Year, Depression_Rates, k=c(4,4)) +
    s(Unemployment, bs="cr", k=6) +
    s(Continent, bs="re"),
  family = gaussian,
  method = "REML",
  data = df2
)

AIC(ayo5b_log, ayo7_log) #IL MIGLIORE RIMANE ayo5b_log
summary(ayo7_log)

prova <- gam(
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


prova2 <- gam(
  log(Sales_in_Millions) ~
    s(Internet_Users, by = factor(Stringency_Index01)) +
    te(Year, Depression_Rates, k=c(4,4)) +
    s(Unemployment) +
    GNI_per_Capita +
    s(Continent, bs="re"),
  family = gaussian,
  method = "REML",
  data = df2
)
summary(prova2)
plot(prova2)

prova3 <- gam(
  log(Sales_in_Millions) ~
    s(Internet_Users, by = factor(Stringency_Index01)) +
    te(Year, Depression_Rates, k=c(4,4)) +
    s(Unemployment) +
    GNI_per_Capita +
    s(Continent, bs="re"),
  family = gaussian,
  method = "REML",
  data = df2
)
summary(prova3)
plot(prova3)


re_continent <- data.frame(
  Continent = levels(df2$Continent),
  re_value  = coef(prova)[grep("Continent", names(coef(prova)))]
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

