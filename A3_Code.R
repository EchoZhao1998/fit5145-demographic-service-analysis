# ============================================================
# FIT5145 Assignment 3 - R Analysis Script
# Research Question: Does healthcare infrastructure respond
# to ageing pressure with a systematic time lag?
# ============================================================

# --- 1. LIBRARIES ---
library(tidyverse)
library(naniar)
library(visdat)
library(ggplot2)
library(corrplot)
library(car)      # VIF
library(lmtest)   # Durbin-Watson test
library(broom)    # tidy model outputs

# --- 2. DATA LOADING FUNCTION ---
# NOTE: Fixed pipe chain (missing pipe after read_csv in original)
clean_nbs <- function(file_path, value_name, save = TRUE) {
  df <- read_csv(file_path, skip = 3, na = c("", "NA"), show_col_types = FALSE) |>
    select(-any_of(`2025`)) |>                          # ← pipe was missing here in original
    rename(province = Region) |>
    filter(
      !is.na(province),
      !grepl("Data Sources", province),
      province != ""
    ) |>
    mutate(across(-province, as.numeric)) |>
    pivot_longer(
      cols      = -province,
      names_to  = "year",
      values_to = value_name
    ) |>
    mutate(year = as.integer(year))
  
  if (save) {
    dir.create("output", showWarnings = FALSE)
    write_csv(df, paste0("output/", value_name, "_clean.csv"))
  }
  return(df)
}

# --- 3. LOAD DATASETS ---
population <- clean_nbs("data/Resident_Population(year-end)(10000 persons).csv", "population")
odr        <- clean_nbs("data/Old_Dependency_Ratio(Sample_Survey)(%).csv", "odr")
beds       <- clean_nbs("data/Number_of_Beds_in_Health_Care_Institutions(10000_units).csv", "beds")
medical    <- clean_nbs("data/Number_of_Medical_Technical_Personnel(10000 persons).csv", "medical_staff")
grp        <- clean_nbs("data/Per_Capita_Gross_Regional_Product(yuan:person).csv", "grp_per_capita")

# `nov` dataset download later and the data stracture slightly change in NBS of China.
# 1. Process the "nov" data separately
nov <- read_csv("data/Outpatient_Services_of_Health_Institutions.csv", 
                skip = 2, 
                na = c("", "NA", " "), 
                show_col_types = FALSE) |>
  # Clean invisible tabs/spaces from headers
  rename_with(~trimws(.)) |> 
  # Use quotes "2025" here, not backticks
  select(Region, matches("^[0-9]{4}$")) |> 
  rename(province = Region) |>
  filter(
    !is.na(province),
    !grepl("Data Sources|Database|Time", province),
    province != ""
  ) |>
  mutate(across(-province, ~as.numeric(as.character(.)))) |>
  pivot_longer(
    cols      = -province,
    names_to  = "year",
    values_to = "nov"  # Explicitly name the value column "nov"
  ) |>
  mutate(year = as.integer(year)) |> 
  # Only keep the specific 9-year range (2016-2024)
  filter(year >= 2016 & year <= 2024)

# 2. Save it to the output folder (same as your function does)
if(!dir.exists("output")) dir.create("output", recursive = TRUE)
write_csv(nov, "output/nov_clean.csv")
# NOTE: THIS DATASET HAS NO `TIBET`

# --- 4. JOIN ---
library(purrr)
df_raw <- list(population, odr, beds, medical, nov, grp) |>
  reduce(left_join, by = c("province", "year"))

# --- 5. DIAGNOSE MISSING ---
colSums(is.na(df_raw))
miss_var_summary(df_raw)
df_raw |> vis_miss()

# --- 6. HANDLE MISSING ---
# Drop 2020: complete missingness in key indicators due to COVID-19
# (treated as a structural break, not imputed)
df_clean <- df_raw |>
  filter(year != 2020)

# --- 7. FEATURE ENGINEERING ---
df_clean <- df_clean |>
  mutate(
    beds_per_10k    = beds / population * 10000,
    medical_per_10k = medical_staff / population * 10000,
    nov_per_10k     = nov / population * 10000,   # outpatient visits per 10k
    log_grp         = log(grp_per_capita)
  )

# --- 8. CREATE LAG VARIABLES (grouped by province) ---
df_clean <- df_clean |>
  arrange(province, year) |>
  group_by(province) |>
  mutate(
    odr_lag1 = lag(odr, 1),
    odr_lag2 = lag(odr, 2),
    odr_lag3 = lag(odr, 3)
  ) |>
  ungroup()

# ============================================================
# SECTION A: DATA OVERVIEW (Five Number Summary)
# ============================================================
summary(df_clean |>
          select(beds_per_10k, medical_per_10k, nov_per_10k, odr, log_grp))

# Distribution plots
plot_dist <- function(var, label, fill_col = "#4e9af1") {
  ggplot(df_clean, aes(x = .data[[var]])) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 40, fill = fill_col, alpha = 0.7) +
    geom_density(color = "navy", linewidth = 1) +
    labs(title = paste("Distribution of", label), x = label, y = "Density") +
    theme_minimal()
}

plot_dist("beds_per_10k",    "Beds per 10,000")
plot_dist("nov_per_10k",     "Outpatient Visits per 10,000")
plot_dist("odr",             "Old-Age Dependency Ratio (%)")
plot_dist("log_grp",         "Log GRP per Capita")

# ODR trend over time (province lines)
ggplot(df_clean, aes(x = year, y = odr, group = province)) +
  geom_line(alpha = 0.3, color = "steelblue") +
  stat_summary(aes(group = 1), fun = mean, geom = "line",
               color = "red", linewidth = 1.2) +
  labs(title = "Old-Age Dependency Ratio by Province (2016–2024, excl. 2020)",
       subtitle = "Red = national average",
       x = "Year", y = "ODR (%)") +
  theme_minimal()

# ============================================================
# SECTION B: CORRELATION ANALYSIS
# ============================================================
corr_matrix <- df_clean |>
  select(beds_per_10k, medical_per_10k, nov_per_10k, odr, log_grp) |>
  cor(use = "complete.obs")

# Adjust the margins: c(bottom, left, top, right)
# Increase the 3rd value (top) to make room for the title
par(mar = c(1, 1, 4, 1)) 

corrplot(corr_matrix, 
         method = "color", 
         type = "upper",
         
         # 1. Coefficient color and size
         addCoef.col = "#CCCCCC", 
         number.cex = 0.7,      # Increased slightly for readability
         
         # 2. Label color (Dark Grey)
         tl.col = "grey20",      
         tl.srt = 45,           # Rotates top labels for better fit
         
         # 3. Correlation Colors (The actual squares)
         # Using a standard blue-white-red palette is usually better for heatmaps
         col = COL2('RdBu', 10), 
         
         # 4. Title handling
         title = "Correlation Matrix: Healthcare & Demographic Variables",
         mar = c(0, 0, 2, 0))   # Internal margin to push title up

# ============================================================
# SECTION C: MODEL A — DEMAND SPIKE
# Outpatient visits ~ ODR (contemporaneous + lag1)
# Hypothesis: demand responds quickly (lag 0-1)
# ============================================================

library(sandwich)
library(lmtest)

# Model A sample — exclude Tibet (nov entirely missing)
df_modelA <- df_clean |> filter(province != "Tibet") 

model_A0 <- lm(nov_per_10k ~ odr + log_grp + factor(province), data = df_modelA)
model_A1 <- lm(nov_per_10k ~ odr + odr_lag1 + log_grp + factor(province), data = df_modelA)

# Compare
AIC(model_A0, model_A1)

# Drop "summary(model_A1)",which result in high VIF, 
# replaced by `coeftest()` robust versions
coeftest(model_A1, vcov = vcovHC(model_A1, type = "HC3"))

# check multicollinearity between odr and odr_lag1
vif(model_A1)

# Durbin-Watson: check residual autocorrelation
dwtest(model_A1)      

# ============================================================
# SECTION D: MODEL B — SUPPLY LAG (DLM)
# Beds ~ ODR at lag 1, 2, 3
# Hypothesis: infrastructure responds slowly (lag 2-3)
# ============================================================

# Model B sample — keep Tibet (beds data complete)
df_modelB <- df_clean
# Note: the 1 missing ODR row in Tibet will be dropped automatically
# by na.action when lag variables are NA at the edges — no manual handling needed


# Tibet was excluded from Model A 
# due to complete absence of outpatient visit records in NBS data, 
# yielding an analytical sample of 30 provinces. 
# All 31 provinces were retained for Model B."

model_B1 <- lm(beds_per_10k ~ odr_lag1 + log_grp + factor(province), data = df_modelB)
model_B2 <- lm(beds_per_10k ~ odr_lag1 + odr_lag2 + log_grp + factor(province), data = df_modelB)
model_B3 <- lm(beds_per_10k ~ odr_lag1 + odr_lag2 + odr_lag3 + log_grp + factor(province), data = df_modelB)

# Model comparison — find optimal lag
model_comparison <- tibble(
  model   = c("Lag1", "Lag1+2", "Lag1+2+3"),
  AIC     = AIC(model_B1, model_B2, model_B3)$AIC,
  adj_r2  = c(summary(model_B1)$adj.r.squared,
              summary(model_B2)$adj.r.squared,
              summary(model_B3)$adj.r.squared)
)
print(model_comparison)
# → The model with lowest AIC / highest adj R² indicates dominant lag length


coeftest(model_B3, vcov = vcovHC(model_B3, type = "HC3"))

# lag terms may be collinear — acceptable if VIF < 10
vif(model_B3)

# autocorrelation check
dwtest(model_B3)       


df_clean <- df_clean |>
  group_by(province) |>
  mutate(
    odr_demean   = odr - mean(odr, na.rm = TRUE),
    odr_lag1_demean = lag(odr_demean, 1),
    odr_lag2_demean = lag(odr_demean, 2),
    odr_lag3_demean = lag(odr_demean, 3)
  ) |>
  ungroup()

# Refit Model B with demeaned lags
model_B3_demean <- lm(
  beds_per_10k ~ odr_lag1_demean + odr_lag2_demean + odr_lag3_demean + 
    log_grp + factor(province),
  data = df_clean
)

vif(model_B3_demean)
coeftest(model_B3_demean, vcov = vcovHC(model_B3_demean, type = "HC3"))


library(car)

# Joint F-test for all lag terms in Model B
linearHypothesis(
  model_B3_demean,
  c("odr_lag1_demean = 0", 
    "odr_lag2_demean = 0", 
    "odr_lag3_demean = 0"),
  vcov = vcovHC(model_B3_demean, type = "HC3")
)

# Elevated VIF among lag terms is consistent with the slow-moving nature of demographic 
# indicators and is expected in DLM specifications (Hadianfar et al., 2023). 
# Robust standard errors using HC3 correction were applied 
# to address potential inflation of significance.


# Ageing pressure drives demand immediately (Model A, lag 0–1, individually significant),
# but drives supply only as a distributed multi-year effect (Model B, jointly significant p=0.02),
# with no single year dominant — consistent with the
# multi-year budgeting cycles of infrastructure investment.

# ============================================================
# SECTION E: MISMATCH INDEX
# Identifies which provinces are most "behind"
# ============================================================
# ============================================================
# SECTION E: MISMATCH INDEX
# ============================================================

# Generate predictions from validated demeaned model
df_modelB <- df_clean |>
  mutate(
    predicted_beds = predict(model_B3_demean, newdata = df_clean),
    mismatch       = predicted_beds - beds_per_10k
    # Positive = actual beds BELOW prediction → province falling behind
    # Negative = actual beds ABOVE prediction → province ahead of demographic pressure
  )

# Average mismatch per province
province_mismatch <- df_modelB |>
  group_by(province) |>
  summarise(
    avg_mismatch   = mean(mismatch, na.rm = TRUE),
    trend_mismatch = last(mismatch) - first(mismatch)  # worsening or improving?
  ) |>
  arrange(desc(avg_mismatch))

print(province_mismatch, n = 31)

# Top 5 at-risk (largest positive mismatch = most under-supplied)
top5_risk <- province_mismatch |> slice_head(n = 5) |> pull(province)

# Bottom 5 (over-supplied relative to demographic pressure)
bottom5 <- province_mismatch |> slice_tail(n = 5) |> pull(province)

# Plot: mismatch trajectory for top 5 at-risk provinces
df_modelB |>
  filter(province %in% top5_risk) |>
  ggplot(aes(x = year, y = mismatch, color = province)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    title    = "Healthcare Infrastructure Mismatch — Top 5 At-Risk Provinces",
    subtitle = "Positive values = beds below demographically predicted level",
    x        = "Year",
    y        = "Mismatch (Predicted − Actual Beds per 10k)",
    color    = "Province"
  ) +
  theme_minimal()

# Plot: all provinces ranked by average mismatch (bar chart)
province_mismatch |>
  mutate(province = fct_reorder(province, avg_mismatch)) |>
  ggplot(aes(x = avg_mismatch, y = province,
             fill = avg_mismatch > 0)) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "#e74c3c", "FALSE" = "#3498db"),
                    labels = c("TRUE" = "Under-supplied", "FALSE" = "Over-supplied")) +
  labs(
    title = "Provincial Healthcare Mismatch Index",
    subtitle = "Red = beds lagging behind ageing pressure; Blue = ahead",
    x = "Average Mismatch (Predicted − Actual Beds per 10k)",
    y = NULL,
    fill = NULL
  ) +
  theme_minimal()

# Your bar chart computes mismatch = beds_per_10k - demo_predicted, so:
  
# Positive = actual beds > predicted = over-supplied (blue, correct)
# Negative = actual beds < predicted = under-supplied (red, correct)

# Most under-supplied (negative, red):
# Beijing, Guangdong, Tianjin, Fujian, Shanghai, Zhejiang — all wealthy eastern coastal provinces.
# Counterintuitive but explainable: high economic development drives enormous demand that outpaces physical bed capacity

# Most over-supplied (positive, red in current wrong chart): 
# Heilongjiang, Gansu, Guizhou, Sichuan — inland/northeastern provinces with rapid ageing
# but slower demand growth and historically high bed-building investment

#Trend mismatch tells you whether the gap is widening or closing:
  
# Shanxi: trend = -9.13 → gap widening rapidly, becoming more under-supplied
# Inner Mongolia: trend = -7.15 → same concern
# Guangxi: trend = +10.0 → improving, moving toward over-supply
# Guizhou: trend = +7.29 → infrastructure expanding faster than demographic pressure

# One note on Anhui — avg_mismatch = -2.23e-13 (essentially zero)
# means Anhui sits exactly on the predicted line.
# It's neither under nor over-supplied — genuinely matched.
# That's worth one sentence in your report.

# Extract only the lag coefficients (excluding province fixed effects)
b0       <- coef(model_B3_demean)["(Intercept)"]
b_lag1   <- coef(model_B3_demean)["odr_lag1_demean"]
b_lag2   <- coef(model_B3_demean)["odr_lag2_demean"]
b_lag3   <- coef(model_B3_demean)["odr_lag3_demean"]
b_grp    <- coef(model_B3_demean)["log_grp"]

# Compute demographically-predicted beds (no province FE)
df_modelB <- df_clean |>
  mutate(
    demo_predicted = b0 +
      b_lag1 * odr_lag1_demean +
      b_lag2 * odr_lag2_demean +
      b_lag3 * odr_lag3_demean +
      b_grp  * log_grp,
    mismatch = beds_per_10k - demo_predicted
    # Positive = more beds than demographics alone predict (over-supplied)
    # Negative = fewer beds than demographics predict (under-supplied / at-risk)
  )

province_mismatch <- df_modelB |>
  group_by(province) |>
  summarise(
    avg_mismatch   = mean(mismatch, na.rm = TRUE),
    trend_mismatch = last(mismatch, na_rm = TRUE) - 
      first(mismatch, na_rm = TRUE)
  ) |>
  arrange(avg_mismatch)  # most negative = most at-risk

print(province_mismatch, n = 31)

# Correct top 5 at-risk = most NEGATIVE mismatch (under-supplied)
top5_risk <- province_mismatch |> 
  slice_head(n = 5) |> 
  pull(province)

# Replot line chart with corrected label
df_modelB |>
  filter(province %in% top5_risk) |>
  ggplot(aes(x = year, y = mismatch, color = province)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    title    = "Healthcare Infrastructure Mismatch — Top 5 At-Risk Provinces",
    subtitle = "Negative values = actual beds BELOW demographically predicted level (under-supplied)",
    x        = "Year",
    y        = "Mismatch (Actual − Predicted Beds per 10k)",
    color    = "Province"
  ) +
  theme_minimal()

# The mismatch analysis covers 2019–2024 (excluding 2020),
# as the three-year distributed lag structure requires ODR observations from 2016 onwards
# to compute lag terms.

bottom5_risk <- province_mismatch |> 
  slice_tail(n = 5) |>   # ← was slice_head, now slice_tail since arranged ascending
  pull(province)

# Replot line chart with corrected label
df_modelB |>
  filter(province %in% bottom5_risk) |>
  ggplot(aes(x = year, y = mismatch, color = province)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    title    = "Healthcare Infrastructure Mismatch — Top 5 At-Risk Provinces",
    subtitle = "Positive values = actual beds BEYOUND demographically predicted level (under-supplied)",
    x        = "Year",
    y        = "Mismatch (Actual − Predicted Beds per 10k)",
    color    = "Province"
  ) +
  theme_minimal()

# it's actually a well-documented phenomenon in China called "patient migration" (就医流动).
# It means Beijing, Shanghai, Guangdong don't just serve their own populations 
# — they absorb patients from surrounding provinces, creating structural under-supply
# that pure provincial demographic data can't fully capture.

# That's worth one sentence in your report as a limitation/discussion point.

# One small thing still outstanding:
# Your trend_mismatch for Anhui shows -2.23e-13 as avg_mismatch — this is a floating point artefact,
# effectively zero. Not a problem for the analysis but worth being aware of when writing.