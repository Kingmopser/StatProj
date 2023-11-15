library(tidyverse)
library(stringr)
#formats all hourly data into long format


Daten_longer <- Daten %>% rename("AT 20" = "A 20","AT 22" = "A 22") %>% rename("Notizen" = ...163)%>% 
  mutate(across(matches(".*[0-9]+.*"), as.numeric)) %>% # all columns to numeric
  mutate(across(matches(".*[0-9]+.*"), function(x) {ifelse(x < -50, NA, x)})) %>% # all negative values to NA
  pivot_longer(
    cols = matches(".*[0-9]+.*"), # columns to pivot
    names_to = c("bodypart", "hour"), # new column names, ATTENTION: bodypart contains things like clothing etc.
    names_pattern = "(.*\\D)(\\d{1,2})$", # pattern to separate the original column names
    values_to = "temperatur" # new column name for the values
  )
