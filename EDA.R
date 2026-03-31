library(tidyverse)

clean_nbs <- function(file_path, value_name, save = TRUE) {
  
  df <- read_csv(file_path, skip = 3, na = c("", "NA"), show_col_types = FALSE) %>%
    select(-`2025`) %>%
    rename(province = Region) %>%
    
    # 🔴 Remove footer / invalid rows
    filter(
      !is.na(province),
      !grepl("Data Sources", province),
      province != ""
    ) %>%
    
    mutate(across(-province, as.numeric)) %>%
    
    pivot_longer(
      cols = -province,
      names_to = "year",
      values_to = value_name
    ) %>%
    
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
cdr <- clean_nbs("data/Children_Dependency_Ratio (Sample_Survey)(%).csv","cdr")

# Economic
grp <- clean_nbs("data/Per_Capita_Gross_Regional_Product(yuan:person).csv", "grp_per_capita")


# Healthcare
beds <- clean_nbs("data/Number_of_Beds_in_Health_Care_Institutions(10000_units).csv", "beds")
medical <- clean_nbs("data/Number_of_Medical_Technical_Personnel(10000 persons).csv", "medical_staff")

# Population (CRITICAL)
population <- clean_nbs("data/Resident_Population(year-end)(10000 persons).csv", "population")


# Education
student_teacher_pri <- clean_nbs("data/Student-Teacher_Ratio_of_Primary_School.csv", "student_teacher_pri_ratio")
student_teacher_jun <- clean_nbs("data/Student-Teacher_Ratio_of_Junior_School.csv", "student_teacher_jun_ratio")


# join tables

df <- population |> 
  left_join(birth_rate, by = c("province", "year")) |> 
  left_join(odr, by = c("province", "year")) |> 
  left_join(cdr, by = c("province", "year")) |> 
  left_join(grp, by = c("province", "year")) |> 
  left_join(beds, by =c("province","year")) |> 
  left_join(medical, by = c("province", "year")) |> 
  left_join(student_teacher_pri, by = c("province", "year")) |> 
  left_join(student_teacher_jun, by = c("province", "year"))

# Workflow: Clean → Merge → Diagnose NA → Handle NA

## diagnose NA value

colSums(is.na(df))

library(naniar)

miss_var_summary(df)

library(visdat)
df |> vis_dat()
df |> vis_miss()

# visualize NA
library(ggplot2)

df %>%
  mutate(odr_missing = is.na(odr)) %>%
  ggplot(aes(x = year, fill = odr_missing)) +
  geom_bar(position = "stack") +
  labs(title = "Missing Pattern of ODR by Year")

df %>%
  mutate(cdr_missing = is.na(cdr)) %>%
  ggplot(aes(x = year, fill = cdr_missing)) +
  geom_bar(position = "stack") +
  labs(title = "Missing Pattern of CDR by Year")

df %>%
  mutate(odr_missing = is.na(odr)) %>%
  ggplot(aes(x = year, fill = odr_missing)) +
  geom_bar(position = "stack") +
  labs(title = "Missing Pattern of ODR by Year")
# dealing NA.
# As 2020 entire year is missing, may be due to Covid 19, it is systematic issue. 
# So I determine to drop all.

df_clean <- df |> 
  filter(year != 2020)

# Report: Observations for 2020 were excluded from the analysis due to 
# complete missing values in key demographic indicators (ODR and CDR), 
# ensuring consistency and reliability in subsequent analysis.

summary(df_clean)


# Normnalize beds and medical staff to better measuring healcare capacity
df_clean <- df_clean |> 
  mutate(
    beds_per_10k = beds/population * 10000,
    medical_per_10k = medical_staff / population * 10000
  )

# log GRP to make model more stable
df_clean <- df_clean %>%
  mutate(log_grp = log(grp_per_capita))

# healthcare regression model
h_model <- lm(
  beds_per_10k ~ odr + cdr + birth_rate + log_grp,
  data = df_clean
)

summary(h_model)

# education regression model
e_model_pri <- lm(
  student_teacher_pri_ratio ~ birth_rate + cdr + log_grp,
  data = df_clean
)

summary(e_model_pri)

e_model_jun <- lm(
  student_teacher_jun_ratio ~ birth_rate + cdr + log_grp,
  data = df_clean
)

summary(e_model_jun)

lm(beds_per_10k ~ odr + cdr + birth_rate + log_grp, data = df_clean)