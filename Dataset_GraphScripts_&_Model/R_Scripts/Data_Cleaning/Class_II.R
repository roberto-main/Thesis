library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

df <- read_excel("Dataset.xlsx")

country_order <- c(
  "Canada",
  "Chile",
  "China",
  "Colombia",
  "Denmark",
  "Egypt",
  "Finland",
  "France",
  "Germany",
  "Hong Kong",
  "India",
  "Indonesia",
  "Iran",
  "Ireland",
  "Israel",
  "Italy",
  "Japan",
  "Malaysia",
  "Mexico",
  "Netherlands",
  "Nigeria",
  "Norway",
  "Pakistan",
  "Philippines",
  "Poland",
  "Republic of Korea (South Korea)",
  "Russian Federation",
  "Saudi Arabia",
  "Singapore",
  "South Africa",
  "Spain",
  "Sweden",
  "Switzerland",
  "Thailand",
  "Turkey",
  "United Arab Emirates",
  "United Kingdom",
  "United States",
  "Vietnam"
)


years <- 2014:2024

df_complete <- expand_grid(
  Country = country_order,
  Year = years
) %>%
  left_join(df, by = c("Country", "Year"))

write_xlsx(df_complete, "Cleaned.xlsx")
