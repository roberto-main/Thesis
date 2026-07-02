library(dplyr)
library(readr)
library(lubridate)
library(writexl)

df <- read_csv("Dataset.csv")

df <- df %>%
  mutate(
    Date = ymd(Date),
    Year = year(Date)
  )

media_stringency <- df %>%
  group_by(CountryName, Year) %>%
  summarise(
    Avg_Stringency = mean(StringencyIndex_Average, na.rm = TRUE)
  ) %>%
  ungroup()

print(media_stringency)

write_xlsx(media_stringency, "Cleaned.xlsx")
