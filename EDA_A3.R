# Load necessary library

library(tidyverse)
library(naniar)
library(visdat)
library(ggplot2)
library(corrplot)
library(GGally)
library(car) # use for VIF

clean_nbs <- function(file_path, value_name, save = TRUE) {
  
  df <- read_csv(file_path, skip = 3, na = c("", "NA"), show_col_types = FALSE) 
  select(-`2025`) |>
    rename(province = Region) |>
    
    # 🔴 Remove footer / invalid rows
    filter(
      !is.na(province),
      !grepl("Data Sources", province),
      province != ""
    ) |>
    
    mutate(across(-province, as.numeric)) |>
    
    pivot_longer(
      cols = -province,
      names_to = "year",
      values_to = value_name
    ) |>
    # nolint
    mutate(year = as.integer(year))
  
  if (save) {
    write_csv(df, paste0("output/", value_name, "_clean.csv"))
  }
  
  return(df)
}

# Apply to datasets

# Demographic
birth_rate <- clean_nbs("data/Birth_rate.csv", "birth_rate")
odr <- clean_nbs("data/Old_Dependency_Ratio(Sample_Survey)(%).csv", "odr")