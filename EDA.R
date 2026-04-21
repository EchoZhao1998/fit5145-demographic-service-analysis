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
  left_join(beds, by = c("province", "year")) |>
  left_join(medical, by = c("province", "year")) |>
  left_join(student_teacher_pri, by = c("province", "year")) |>
  left_join(student_teacher_jun, by = c("province", "year"))

# Workflow: Clean → Merge → Diagnose NA → Handle NA

## diagnose NA value

colSums(is.na(df))
miss_var_summary(df)
df |> vis_dat()
df |> vis_miss()

# visualize NA
df |>
  mutate(odr_missing = is.na(odr)) |>
  ggplot(aes(x = year, fill = odr_missing)) +
  geom_bar(position = "stack") +
  labs(title = "Missing Pattern of ODR by Year")

df |>
  mutate(cdr_missing = is.na(cdr)) |>
  ggplot(aes(x = year, fill = cdr_missing)) +
  geom_bar(position = "stack") +
  labs(title = "Missing Pattern of CDR by Year")

df |>
  mutate(odr_missing = is.na(odr)) |>
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


# Normalize beds and medical staff to better measuring healcare capacity
df_clean <- df_clean |> 
  mutate(
    beds_per_10k = beds/population * 10000,
    medical_per_10k = medical_staff / population * 10000,
    log_grp = log(grp_per_capita), # log GRP to make model more stable
  )


# compute vector to test lag effect
df_clean <- df_clean |> 
  arrange(province, year) |> 
  group_by(province) |> 
  mutate(
    birth_rate_lag1 = lag(birth_rate, 1),
    odr_lag1 = lag(odr, 1)
  ) |> 
  ungroup()


# Check skewness
ggplot(df_clean, aes(x = beds_per_10k)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "#feb24c") +

ggplot(df_clean, aes(x = odr)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "#feb24c") +
  geom_density(color = "blue", linewidth = 1)

ggplot(df_clean, aes(x = birth_rate)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "#feb24c") +
  geom_density(color = "blue", linewidth = 1)

# “Initial exploratory analysis was conducted to examine variable distributions 
# and detect potential outliers. Several variables (e.g., GRP per capita) exhibited 
# right-skewed distributions and were log-transformed prior to modelling.”



corr_matrix <- df_clean |> 
  select(beds_per_10k, odr, cdr, birth_rate, log_grp) |> 
  cor(use = "complete.obs")

corrplot(corr_matrix, method = "color", type = "hcluster")

# Detect multicollinearity. Understand direction of relationships BEFORE modelling
ggpairs(df_clean, 
        columns = c("beds_per_10k", "odr", "cdr", "birth_rate", "log_grp"),
        lower = list(
          continuous = wrap("points", 
                            color = "steelblue", 
                            size = 0.5, 
                            alpha = 0.4)
        ))





# Correlation analysis was conducted to examine relationships between demographic 
# indicators and service capacity. Moderate correlations were observed between ageing 
# indicators and healthcare capacity, supporting their inclusion in regression models



# healthcare regression model
h_model <- lm(
  beds_per_10k ~ odr + cdr + birth_rate + log_grp,
  data = df_clean
)

summary(h_model)
vif(h_model)

# Predict 'ideal' capacity based on historical ODR
df_clean$predicted_beds <- predict(h_model, newdata = df_clean)

# Calculate the 'Capacity Debt' (Mismatch)
df_clean <- df_clean %>%
  mutate(mismatch = beds_per_10k - predicted_beds)

# Rank provinces for the 15th FYP Risk Map
risk_map_2026 <- df_clean %>%
  filter(year == 2024) %>% # Use most recent full year
  arrange(mismatch)





# Just add a grouping variable
df_clean <- df_clean %>%
  mutate(tier = case_when(
    province %in% c("Beijing", "Shanghai", "Guangdong") ~ "Tier 1",
    province %in% c("Sichuan", "Chongqing", "Hubei") ~ "Tier 2",
    TRUE ~ "Tier 3"
  ))

# Run the model by group
group_models <- df_clean %>%
  group_by(tier) %>%
  do(model = lm(beds_per_10k ~ odr_lag1 + odr_lag2 + odr_lag3 + log_grp, data = .))


# Healthcare capacity appears to respond more strongly to ageing pressure than
# to economic development, suggesting demographic structure is a primary driver
# of public health infrastructure allocation


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

# Primary education resources respond more directly to demographic fluctuations, 
# whereas junior secondary education appears more institutionally stable,
# indicating delayed adjustment mechanisms in education planning.

e_model_pri_lag <- lm(
  student_teacher_pri_ratio ~ birth_rate_lag1 + cdr + log_grp,
  data = df_clean
)
# Education resources respond to demographic change with a time delay

lm(beds_per_10k ~ odr + cdr + birth_rate + log_grp, data = df_clean)