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
library(gridExtra)


df <- read_excel("Final_Dataset.xlsx", sheet="Countries")
df <- df[!is.na(df$Country),]

library(stargazer)
stargazer(df, type="latex", summary=TRUE)

df2$Log_Sales_in_Millions <- log(df2$Sales_in_Millions)

df_summary <- df2[, c("Log_Sales_in_Millions", "GNI_per_Capita", 
                      "Internet_Users", "Depression_Rates",
                      "Unemployment", "Stringency_Index0")]

stargazer(as.data.frame(df_summary), type="latex", summary=TRUE,
          title="General Data Insights",
          label="tab:descrittive")

stargazer(as.data.frame(df_summary), type="latex", summary=TRUE,
          summary.stat=c("n", "mean", "sd", "min", "median", "max"),
          title="General Data Insights",
          label="tab:descrittive")

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
df$Sales_in_Millions <- log(df$Sales_in_Millions)

colnames(df) <- make.names(colnames(df))
colnames(df2) <- make.names(colnames(df2))

my_colors <- c(
  "AF" = "#E41A1C",
  
  #AMERICAS
  "AM - North" = "#D95F02",
  "AM - South" = "#E6AB02",
  
  #ASIA
  "AS - East" = "#4169E1",
  "AS - Middle East" = "#008080",
  "AS - South" = "#00FFFF",
  "AS - Southeast" = "#AFEEEE",
  
  #EUROPE
  "EU - East" = "#4B0082",
  "EU - North" = "#9932CC",
  "EU - South" = "#EE82EE",
  "EU - West" = "#D8BFD8"
)

##Previous model/version, kept here as a backup
ayo4_log <- gam(
  log(Sales_in_Millions) ~
    Internet_Users +
    te(Year, Depression_Rates, k=c(4,4)) +
    s(Continent, bs="re"),
  family = gaussian,
  method = "REML",
  data = df2
)

ayo4 <- gam(
  (Sales_in_Millions) ~
    Internet_Users +
    te(Year, Depression_Rates, k=c(4,4)) +
    s(Continent, bs="re"),
  family = gaussian,
  method = "REML",
  data = df2
)

##Best model/version
ayo5b <- gam(
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

idx <- which(complete.cases(df2[, c("Internet_Users", "Year", 
                                    "Depression_Rates", "Continent",
                                    "Sales_in_Millions",
                                    "Unemployment",
                                    "GNI_per_Capita")]))

residuals_val <- residuals(ayo5b_log)
fitted_val    <- fitted(ayo5b_log)
continent_val <- df2$Continent[idx]

n            <- length(residuals_val)
qq_intercept <- mean(residuals_val)
qq_slope     <- sd(residuals_val)

diag_ayo5b_log <- data.frame(
  fitted_val    = fitted_val,
  residuals_val = residuals_val,
  sqrt_res      = sqrt(abs(residuals_val)),
  theoretical   = qnorm(ppoints(n))[rank(residuals_val)],
  Continent     = continent_val
)

qq_intercept <- mean(residuals_val)
qq_slope     <- sd(residuals_val)

# Grafici neri
p1 <- ggplot(diag_ayo5b_log, aes(x=fitted_val, y=residuals_val)) +
  geom_point() +
  labs(title="Residuals vs Fitted", x="Fitted", y="Residuals")

p2 <- ggplot(diag_ayo5b_log, aes(x=residuals_val)) +
  geom_histogram(bins=30) +
  labs(title="Histogram of Residuals", x="Residuals")

p3 <- ggplot(diag_ayo5b_log, aes(x=theoretical, y=residuals_val)) +
  geom_point() +
  geom_abline(intercept=qq_intercept, slope=qq_slope) +
  labs(title="Q-Q Plot", x="Theoretical Quantiles", y="Sample Quantiles")

p4 <- ggplot(diag_ayo5b_log, aes(x=fitted_val, y=sqrt_res)) +
  geom_point() +
  labs(title="Scale-Location", x="Fitted", y="sqrt|Residuals|")

# Grafici colorati
p1c <- ggplot(diag_ayo5b_log, aes(x=fitted_val, y=residuals_val, color=Continent)) +
  geom_point(alpha=0.6) +
  scale_color_manual(values=my_colors) +
  labs(title="Residuals vs Fitted", x="Fitted", y="Residuals")

p2c <- ggplot(diag_ayo5b_log, aes(x=residuals_val)) +
  geom_histogram(fill=my_colors["EU - West"], bins=30) +
  labs(title="Histogram of Residuals", x="Residuals")

p3c <- ggplot(diag_ayo5b_log, aes(x=theoretical, y=residuals_val, color=Continent)) +
  geom_point(alpha=0.6) +
  geom_abline(intercept=qq_intercept, slope=qq_slope, color="black") +
  scale_color_manual(values=my_colors) +
  labs(title="Q-Q Plot", x="Theoretical Quantiles", y="Sample Quantiles")

p4c <- ggplot(diag_ayo5b_log, aes(x=fitted_val, y=sqrt_res, color=Continent)) +
  geom_point(alpha=0.6) +
  scale_color_manual(values=my_colors) +
  labs(title="Scale-Location", x="Fitted", y="sqrt|Residuals|")

g  <- grid.arrange(p1,  p2,  p3,  p4,  ncol=2)
g2 <- grid.arrange(p1c, p2c, p3c, p4c, ncol=2)

ggsave("Log_Model_Graph.png", plot=g, width=12, height=10, dpi=300)
ggsave("Log_Model_Colored_Graph.png", plot=g2, width=14, height=10, dpi=300)

summary(ayo5b_log)
