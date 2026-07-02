library(readxl)
library(dplyr)
library(writexl)

df <- read_excel("Dataset.xlsx", skip = 3)

df_filtered <- df %>%
  select(`Country Name`, `2014`:`2024`)

write_xlsx(df_filtered, "Cleaned.xlsx")
