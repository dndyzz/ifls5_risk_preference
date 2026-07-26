## 14_crra_sensitivity.R -----------------------------------------------------
## The implied CRRA bounds in the main text treat the stated amounts as if they
## were the respondent's whole consumption. With background consumption w, the
## same choice implies a different degree of relative risk aversion, because
## the gamble is a smaller share of total resources.
##
## Indifference condition with background consumption:
##     0.5 u(w + H) + 0.5 u(w + L) = u(w + C)
## with C normalized to 1 (Rp 800,000/month), H = 2, and L taking the values
## used by the Set 1 branching items.
##
## Run from the project root, after 07_comprehension_sample.R.
## Output: output/tables/crra_sensitivity.rds
## ---------------------------------------------------------------------------

suppressMessages({ library(tidyverse); library(survival); library(sandwich); library(lmtest) })

S  <- readRDS("data/processed/comprehension_sample.rds")
d4 <- S$cc |> filter(gamble_averse == 0)

u <- function(x, r) if (abs(r - 1) < 1e-9) log(x) else (x^(1 - r)) / (1 - r)

## indifference r for a given low outcome L and background consumption w
solve_r <- function(L, w, H = 2, C = 1) {
  f <- function(r) 0.5 * u(w + H, r) + 0.5 * u(w + L, r) - u(w + C, r)
  lo <- -5; hi <- 200
  if (f(lo) * f(hi) > 0) return(NA_real_)
  uniroot(f, c(lo, hi), tol = 1e-9)$root
}

W <- c(0, 0.25, 0.5, 1, 2, 5)
Ls <- c(si03 = 0.50, si04 = 0.75, si05 = 0.25)

cat("\n============ CRRA BOUNDS UNDER BACKGROUND CONSUMPTION ============\n")
cat("w is background monthly consumption as a multiple of the certain amount\n")
cat("(Rp 800,000). w = 0 reproduces the bounds reported in the main text.\n\n")

grid <- map_dfr(W, function(w) {
  r03 <- solve_r(Ls["si03"], w); r04 <- solve_r(Ls["si04"], w); r05 <- solve_r(Ls["si05"], w)
  tibble(w = w,
         g1_lower = r04,
         g2 = sprintf("%.2f to %.2f", r03, r04),
         g3 = sprintf("%.2f to %.2f", r05, r03),
         g4_upper = r05,
         r_si03 = r03)
})
print(as.data.frame(grid |> mutate(across(c(g1_lower, g4_upper, r_si03), ~ round(.x, 3)))))

cat("\nThe ordering of the four groups is preserved at every w, so the index\n")
cat("remains monotone in risk aversion. What changes is the numerical scale:\n")
cat("the boundary between groups 1 and 2 moves from")
cat(sprintf(" %.2f at w = 0 to %.2f at w = 5.\n",
            grid$g1_lower[grid$w == 0], grid$g1_lower[grid$w == 5]))
cat("Reported CRRA magnitudes are therefore only interpretable relative to the\n")
cat("assumed w, and we report w = 0 as a convention rather than an estimate.\n")

## ---------------------------------------------------------------------------
## Does the regression evidence depend on w?
## ---------------------------------------------------------------------------
cat("\n--- Interval regression on CRRA bounds, by assumed w ---\n")
cogv <- S$cog_vars
RHS <- paste(paste(cogv, collapse = " + "),
             "female + employed + age_c + age_c2 + educ_years + rural", sep = " + ")

fit_w <- function(w) {
  r03 <- solve_r(Ls["si03"], w); r04 <- solve_r(Ls["si04"], w); r05 <- solve_r(Ls["si05"], w)
  dd <- d4 |>
    mutate(lo = case_when(risk_s1 == 1 ~ r04, risk_s1 == 2 ~ r03,
                          risk_s1 == 3 ~ r05, risk_s1 == 4 ~ NA_real_),
           hi = case_when(risk_s1 == 1 ~ NA_real_, risk_s1 == 2 ~ r04,
                          risk_s1 == 3 ~ r03, risk_s1 == 4 ~ r05))
  m <- survreg(Surv(lo, hi, type = "interval2") ~ ., dist = "gaussian",
               data = dd |> select(lo, hi, all_of(cogv), female, employed,
                                   age_c, age_c2, educ_years, rural))
  tb <- as.data.frame(summary(m)$table)
  tb$term <- rownames(tb)
  tb |> filter(term %in% c(cogv, "female", "educ_years", "rural")) |>
    transmute(term, w = w, est = Value, p = p,
              per_sd = Value * sapply(term, function(v) sd(d4[[v]])))
}

sens <- map_dfr(W, fit_w)
wide <- sens |>
  mutate(lab = sprintf("%.2f%s", per_sd,
                       case_when(p < .001 ~ "***", p < .01 ~ "**",
                                 p < .05 ~ "*", p < .10 ~ "+", TRUE ~ ""))) |>
  select(term, w, lab) |>
  pivot_wider(names_from = w, values_from = lab, names_prefix = "w=")
cat("\nEffect per SD of predictor, positive = more risk averse:\n")
print(as.data.frame(wide))
cat("\n*** p<.001  ** p<.01  * p<.05  + p<.10\n")

sgn <- sens |> group_by(term) |>
  summarise(sign_stable = n_distinct(sign(est)) == 1,
            always_sig = all(p < .05), .groups = "drop")
cat("\nSign stability across all values of w:\n")
print(as.data.frame(sgn))

saveRDS(list(bounds_grid = grid, sensitivity = sens, sign_stability = sgn),
        "output/tables/crra_sensitivity.rds")
cat("\nSaved output/tables/crra_sensitivity.rds\n")
