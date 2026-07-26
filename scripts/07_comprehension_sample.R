## 07_comprehension_sample.R -------------------------------------------------
## Builds the canonical dataset that retains ALL Set 1 screening outcomes
## (eligible / eligible_corrected / gamble_averse), so that task comprehension
## and risk preference can be modelled as separate channels.
##
## Replicates the feature construction in 04_merge_feature.Rmd exactly, with two
## additions:
##   * screen_s1 is kept as a 3-level ordered comprehension gradient
##   * educ_years is built twice: `educ_years` (as in 04, for comparability) and
##     `educ_years_fix`, which does not silently treat missing schooling data as
##     zero / as level completion
##
## Run from the project root. Output: data/processed/comprehension_sample.rds
## ---------------------------------------------------------------------------

library(haven)
library(tidyverse)

risk <- readRDS("data/processed/risk_index.rds")
bfi  <- readRDS("data/processed/big_five.rds")
cog  <- readRDS("data/processed/cognitive.rds")

ptrack <- read_dta("data/raw/ptrack.dta")
htrack <- read_dta("data/raw/htrack.dta")
dl_raw <- read_dta("data/raw/b3a_dl1.dta")
tk_raw <- read_dta("data/raw/b3a_tk1.dta")
bk_raw <- read_dta("data/raw/bk_sc1.dta")

for (nm in c("ptrack", "htrack", "dl_raw", "tk_raw", "bk_raw")) {
  x <- get(nm); names(x) <- tolower(names(x)); assign(nm, x)
}

delabel <- function(d) {
  mutate(d, across(where(~ inherits(., "haven_labelled")),
                   ~ as.numeric(as.character(.))))
}

## --- demographics ----------------------------------------------------------
demo <- ptrack |>
  delabel() |>
  select(pidlink, age = age_14, sex) |>
  filter(!is.na(age)) |>
  mutate(female = as.integer(sex == 3)) |>
  select(pidlink, age, female)

## --- geography -------------------------------------------------------------
geo_hh <- htrack |> delabel() |> select(hhid14_9, province = sc01_14_14)

urban_hh <- bk_raw |>
  delabel() |>
  select(hhid14_9, urban = sc05) |>
  mutate(rural = as.integer(urban == 2)) |>
  select(hhid14_9, rural)

geo <- geo_hh |>
  left_join(urban_hh, by = "hhid14_9") |>
  filter(!is.na(province)) |>
  mutate(province = factor(province))

geo_pid <- ptrack |>
  select(pidlink, hhid14_9) |>
  filter(!is.na(hhid14_9)) |>
  left_join(geo, by = "hhid14_9") |>
  select(pidlink, province, rural)

## --- education -------------------------------------------------------------
## `educ_years`     : verbatim reproduction of 04_merge_feature.Rmd
## `educ_years_fix` : missing dl06 -> NA (not 0); missing grade -> NA (not the
##                    level maximum, which pmin(..., na.rm = TRUE) silently did)
educ <- dl_raw |>
  delabel() |>
  select(pidlink, dl06, dl07) |>
  mutate(
    dl07_clean = if_else(dl07 %in% c(98, 99), NA_real_, dl07),

    ## ---- legacy version (as published) ----
    educ_years = case_when(
      dl06 == 1 | is.na(dl06)    ~ 0,
      dl06 %in% c(2, 11, 14, 72) ~ pmin(dl07_clean, 6, na.rm = TRUE),
      dl06 %in% c(3, 4, 12, 73)  ~ 6 + pmin(dl07_clean, 3, na.rm = TRUE),
      dl06 %in% c(5, 6, 13, 74)  ~ 9 + pmin(dl07_clean, 3, na.rm = TRUE),
      dl06 == 60                 ~ 12 + pmin(dl07_clean, 3, na.rm = TRUE),
      dl06 == 61                 ~ 12 + pmin(dl07_clean, 4, na.rm = TRUE),
      dl06 == 62                 ~ 16 + pmin(dl07_clean, 2, na.rm = TRUE),
      dl06 == 63                 ~ 18 + pmin(dl07_clean, 3, na.rm = TRUE),
      TRUE                       ~ NA_real_
    ),
    educ_years = if_else(dl07 == 7 & dl06 %in% c(2, 72),    6, educ_years),
    educ_years = if_else(dl07 == 7 & dl06 %in% c(3, 4, 73), 9, educ_years),
    educ_years = if_else(dl07 == 7 & dl06 %in% c(5, 6, 74), 12, educ_years),
    educ_years = if_else(dl07 == 7 & dl06 == 60,            15, educ_years),
    educ_years = if_else(dl07 == 7 & dl06 == 61,            16, educ_years),
    educ_years = if_else(dl07 == 7 & dl06 == 62,            18, educ_years),
    educ_years = if_else(dl07 == 7 & dl06 == 63,            21, educ_years),

    ## ---- corrected version ----
    grad = !is.na(dl07) & dl07 == 7,
    educ_years_fix = case_when(
      is.na(dl06)                ~ NA_real_,
      dl06 == 1                  ~ 0,
      dl06 %in% c(2, 11, 14, 72) ~ if_else(grad, 6,  pmin(dl07_clean, 6)),
      dl06 %in% c(3, 4, 12, 73)  ~ if_else(grad, 9,  6 + pmin(dl07_clean, 3)),
      dl06 %in% c(5, 6, 13, 74)  ~ if_else(grad, 12, 9 + pmin(dl07_clean, 3)),
      dl06 == 60                 ~ if_else(grad, 15, 12 + pmin(dl07_clean, 3)),
      dl06 == 61                 ~ if_else(grad, 16, 12 + pmin(dl07_clean, 4)),
      dl06 == 62                 ~ if_else(grad, 18, 16 + pmin(dl07_clean, 2)),
      dl06 == 63                 ~ if_else(grad, 21, 18 + pmin(dl07_clean, 3)),
      TRUE                       ~ NA_real_
    )
  ) |>
  select(pidlink, educ_years, educ_years_fix)

## --- employment ------------------------------------------------------------
employ <- tk_raw |>
  delabel() |>
  select(pidlink, tk01) |>
  mutate(employed = as.integer(tk01 == 1)) |>
  select(pidlink, employed)

## --- merge, keeping every screening outcome --------------------------------
df <- risk |>
  filter(screen_s1 %in% c("eligible", "eligible_corrected", "gamble_averse")) |>
  left_join(bfi,     by = "pidlink") |>
  left_join(cog,     by = "pidlink") |>
  left_join(demo,    by = "pidlink") |>
  left_join(geo_pid, by = "pidlink") |>
  left_join(educ,    by = "pidlink") |>
  left_join(employ,  by = "pidlink") |>
  filter(age >= 18, age <= 65)

## --- analysis variables ----------------------------------------------------
psn_items <- c(paste0("psn_", sprintf("%02d", c(1:3, 6:8, 10:13, 15))),
               "psn_04r", "psn_05r", "psn_09r", "psn_14r")
cog_vars  <- c("co07count", "co10count", "serial7_score", "memory_self",
               "wscore", "raven_score")
traits    <- c("openness", "conscientiousness", "extraversion",
               "agreeableness", "neuroticism")
demo_vars <- c("age", "female", "educ_years", "rural", "employed")

model_vars <- c(psn_items, cog_vars, demo_vars)

df <- df |>
  mutate(
    ## comprehension gradient: 0 = correct first time ... 2 = confirmed dominated
    comprehension = factor(
      case_when(
        screen_s1 == "eligible"           ~ "correct",
        screen_s1 == "eligible_corrected" ~ "corrected",
        screen_s1 == "gamble_averse"       ~ "confirmed_dominated"
      ),
      levels = c("correct", "corrected", "confirmed_dominated"),
      ordered = TRUE
    ),
    gamble_averse = as.integer(screen_s1 == "gamble_averse"),
    risk_5g  = if_else(gamble_averse == 1L, 0L, as.integer(risk_s1)),
    risk_ord = factor(risk_s1,  ordered = TRUE),
    risk_5g_ord = factor(risk_5g, levels = 0:4, ordered = TRUE),
    ## implied CRRA bounds for Set 1 (see 10_crra_interval.R for derivation)
    crra_lo = case_when(risk_s1 == 1 ~ 2.9150, risk_s1 == 2 ~ 1.0000,
                        risk_s1 == 3 ~ 0.3058, risk_s1 == 4 ~ -Inf),
    crra_hi = case_when(risk_s1 == 1 ~ Inf,    risk_s1 == 2 ~ 2.9150,
                        risk_s1 == 3 ~ 1.0000, risk_s1 == 4 ~ 0.3058)
  )

## complete cases on the modelling variables, so that every specification in
## the paper is estimated on one fixed sample. The 13 respondents who passed
## screening but could not be scored (missing si03/si04/si05) are dropped;
## retaining them is what made risk_dist disagree with the fitted model N.
df_cc <- df |> drop_na(all_of(model_vars)) |> filter(!is.na(risk_5g))

df_cc <- df_cc |>
  mutate(age_c = age - mean(age), age_c2 = age_c^2)

## --- reconciliation report -------------------------------------------------
cat("=========== SAMPLE RECONCILIATION ===========\n")
cat(sprintf("risk_index.rds rows                          : %6d\n", nrow(risk)))
cat(sprintf("screen_s1 non-missing + age 18-65            : %6d\n", nrow(df)))
cat(sprintf("  ... complete cases on model variables      : %6d\n", nrow(df_cc)))
cat("\n-- BEFORE complete-case filter (denominator used by Figure 3) --\n")
print(df |> count(risk_5g) |> mutate(pct = round(100 * n / sum(n), 2)))
cat("\n-- AFTER complete-case filter (denominator used by the models) --\n")
print(df_cc |> count(risk_5g) |> mutate(pct = round(100 * n / sum(n), 2)))
cat("\n-- 4-group primary sample (gamble-averse excluded) --\n")
print(df_cc |> filter(gamble_averse == 0) |> count(risk_ord) |>
        mutate(pct = round(100 * n / sum(n), 2)))
cat(sprintf("\n4-group N = %d   (published: 17,886)\n",
            sum(df_cc$gamble_averse == 0)))
cat(sprintf("5-group N = %d   (published: 25,040)\n", nrow(df_cc)))

cat("\n=========== EDUCATION RECODE CHECK ===========\n")
cat(sprintf("dl06 missing, assigned 0 years by legacy recode : %6d\n",
            sum(is.na(dl_raw$dl06))))
ed <- df_cc |> summarise(
  legacy_mean = mean(educ_years), fix_mean = mean(educ_years_fix, na.rm = TRUE),
  legacy_zero = sum(educ_years == 0), fix_zero = sum(educ_years_fix == 0, na.rm = TRUE),
  fix_na = sum(is.na(educ_years_fix)),
  disagree = sum(abs(educ_years - educ_years_fix) > 0.001, na.rm = TRUE))
print(as.data.frame(ed))

cat("\n=========== COMPREHENSION GRADIENT ===========\n")
grad_tab <- df_cc |>
  group_by(comprehension) |>
  summarise(
    n = n(),
    raven   = mean(raven_score), wscore = mean(wscore),
    serial7 = mean(serial7_score), recall = mean(co07count),
    educ    = mean(educ_years),  age = mean(age),
    female  = 100 * mean(female), .groups = "drop")
print(as.data.frame(grad_tab), digits = 4)

saveRDS(list(full = df, cc = df_cc,
             psn_items = psn_items, cog_vars = cog_vars,
             traits = traits, demo_vars = demo_vars),
        "data/processed/comprehension_sample.rds")

cat("\nSaved data/processed/comprehension_sample.rds\n")
