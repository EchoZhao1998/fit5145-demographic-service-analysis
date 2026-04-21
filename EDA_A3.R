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
# Economic
grp <- clean_nbs("data/Per_Capita_Gross_Regional_Product(yuan:person).csv", "grp_per_capita")
# Healthcare
beds <- clean_nbs("data/Number_of_Beds_in_Health_Care_Institutions(10000_units).csv", "beds") 
medical <- clean_nbs("data/Number_of_Medical_Technical_Personnel(10000 persons).csv", "medical_staff")
hos <- clean_nbs("data/Outpatient_Services_of_Health_Institutions.csv", "hos") 
# Population (CRITICAL)
population <- clean_nbs("data/Resident_Population(year-end)(10000 persons).csv", "population")

# Join tables
library(purrr)

# Put all your cleaned dataframes into a list
list_of_dfs <- list(
  population, odr, beds, medical, hos, grp
)

# Join them all at once
df_raw <- list_of_dfs |> reduce(left_join, by = c("province", "year"))


# diagnose NA value
colSums(is.na(df))
miss_var_summary(df)


# dealing NA.
# As 2020 entire year is missing, may be due to Covid 19, it is systematic issue. 
# So I determine to drop all.
df_cleaned <- df_raw |>
  filter(year != 2020)

df_cleaned <- df_clean |> 
  mutate(
    beds_per_10k = beds/population * 10000,
    medical_per_10k = medical_staff / population * 10000,
    log_grp = log(grp_per_capita), # log GRP to make model more stable
  )

# Creating the lags
df_cleaned <- df_cleaned |> 
  group_by(province) |> 
  mutate(
    odr_lag1 = lag(odr, 1),
    odr_lag2 = lag(odr, 2),
    odr_lag3 = lag(odr, 3)
  ) |> 
  ungroup()

# Running the DLM
dlm_model <- lm(beds_per_10k ~ odr + odr_lag1 + odr_lag2 + odr_lag3 + log_grp, data = df_clean)
summary(dlm_model)




