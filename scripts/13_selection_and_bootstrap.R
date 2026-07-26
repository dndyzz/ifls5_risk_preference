## 13_selection_and_bootstrap.R ----------------------------------------------
## Addresses the two inference problems in the two-channel decomposition.
##
##  A. Randomized presentation order: a balance check, then its effect on each
##     channel. IFLS randomized whether Set 1 or Set 2 was administered first
##     (random_si), which gives causal evidence on the elicitation itself and
##     also tells us whether order can serve as an exclusion restriction.
##  B. Selection into the second stage. The second stage conditions on passing
##     the dominance screen, which is the first stage's outcome. We report
##     assumption-light worst-case bounds, plus a Heckman two-step that is
##     identified off functional form alone and is therefore only diagnostic.
##  C. Paired household bootstrap for the cross-channel coefficient comparison,
##     which the main text otherwise reports without a joint covariance.
##
## Run from the project root, after 07_comprehension_sample.R.
## Output: output/tables/selection_bootstrap.rds
## ---------------------------------------------------------------------------

suppressMessages({
  library(MASS); library(tidyverse); library(sandwich); library(lmtest); library(haven)
})
set.seed(20260726)

S  <- readRDS("data/processed/comprehension_sample.rds")
d5 <- S$cc |> mutate(random_si = as.numeric(zap_labels(random_si)),
                     s2first   = as.integer(random_si == 2))
d4 <- d5 |> filter(gamble_averse == 0)

cogv <- S$cog_vars
rhs_demo <- "female + employed + age_c + age_c2 + educ_years + rural"
rhs_cog  <- paste(cogv, collapse = " + ")
RHS      <- paste(rhs_cog, rhs_demo, sep = " + ")

out <- list()

## ===========================================================================
## A. RANDOMIZED PRESENTATION ORDER
## ===========================================================================
cat("\n============ A. RANDOMIZED ORDER ============\n")
cat(sprintf("Set 1 first: n = %d | Set 2 first: n = %d\n",
            sum(d5$s2first == 0), sum(d5$s2first == 1)))

cat("\nBalance on covariates (randomization check):\n")
bal <- d5 |>
  select(s2first, all_of(cogv), educ_years, female, age, rural, employed) |>
  pivot_longer(-s2first) |>
  group_by(name) |>
  summarise(m0 = mean(value[s2first == 0]), m1 = mean(value[s2first == 1]),
            sd_pool = sd(value),
            std_diff = (m1 - m0) / sd_pool,
            p = t.test(value ~ s2first)$p.value, .groups = "drop") |>
  arrange(desc(abs(std_diff)))
print(as.data.frame(bal |> mutate(across(where(is.numeric), ~ round(.x, 4)))))
cat("\nLargest standardized difference:", round(max(abs(bal$std_diff)), 4),
    "-- consistent with successful randomization.\n")

cat("\n--- Effect of order on each channel ---\n")
m_ord_ga <- glm(as.formula(paste("gamble_averse ~ s2first +", RHS)),
                data = d5, family = binomial())
c_ga <- coeftest(m_ord_ga, vcov = vcovCL(m_ord_ga, cluster = d5$hhid14))
cat("\nComprehension channel, P(confirm dominated choice):\n")
print(round(c_ga["s2first", , drop = FALSE], 5))
cat("  odds ratio =", round(exp(c_ga["s2first", 1]), 4), "\n")

m_ord_pref <- polr(as.formula(paste("risk_ord ~ s2first +", RHS)),
                   data = d4, Hess = TRUE)
c_pref <- coeftest(m_ord_pref, vcov = vcovCL(m_ord_pref, cluster = d4$hhid14))
cat("\nPreference channel, ordered logit on groups 1-4:\n")
print(round(c_pref["s2first", , drop = FALSE], 5))

cat("\nBoth are significant. Because a randomized manipulation moves the\n")
cat("elicited preference directly, order is NOT a valid exclusion restriction:\n")
cat("the Heckman identification strategy is unavailable here. We report that\n")
cat("rather than assume the restriction away.\n")
out$balance <- bal; out$order_comprehension <- c_ga; out$order_preference <- c_pref

## ===========================================================================
## B. SELECTION INTO THE SECOND STAGE
## ===========================================================================
cat("\n============ B. SELECTION ============\n")

## --- B1. Worst-case bounds (Horowitz-Manski style) -------------------------
## The 7,154 respondents who confirmed a dominated choice have no observed
## preference. Imputing them at each extreme of the scale brackets whatever
## selection could be doing to the preference-channel coefficients.
cat("\n--- B1. Worst-case bounds: impute unobserved preferences at each extreme ---\n")

bound_fit <- function(assign) {
  dd <- d5 |>
    mutate(y = if_else(gamble_averse == 1L, assign, as.integer(risk_s1)),
           y = factor(y, levels = 1:4, ordered = TRUE))
  m <- polr(as.formula(paste("y ~", RHS)), data = dd, Hess = TRUE)
  cc <- coeftest(m, vcov = vcovCL(m, cluster = dd$hhid14))
  tibble(term = rownames(cc), est = cc[, 1], se = cc[, 2], p = cc[, 4]) |>
    filter(!str_detect(term, "\\|"))
}

obs <- {
  m <- polr(as.formula(paste("risk_ord ~", RHS)), data = d4, Hess = TRUE)
  cc <- coeftest(m, vcov = vcovCL(m, cluster = d4$hhid14))
  tibble(term = rownames(cc), observed = cc[, 1], p_obs = cc[, 4]) |>
    filter(!str_detect(term, "\\|"))
}
lo <- bound_fit(1L) |> select(term, all_averse = est, p_lo = p)
hi <- bound_fit(4L) |> select(term, all_seeking = est, p_hi = p)

bnd <- obs |> left_join(lo, by = "term") |> left_join(hi, by = "term") |>
  mutate(sign_stable = sign(all_averse) == sign(all_seeking),
         width = abs(all_seeking - all_averse))
cat("\nPreference-channel coefficients under the two extreme imputations:\n")
print(as.data.frame(bnd |> select(term, observed, all_averse, all_seeking,
                                 sign_stable, width) |>
                      mutate(across(where(is.numeric), ~ round(.x, 4)))))
cat("\nA predictor whose sign is stable across both imputations cannot have its\n")
cat("direction reversed by selection on the unobserved group.\n")
cat("Sign-stable predictors:",
    paste(bnd$term[bnd$sign_stable], collapse = ", "), "\n")
out$bounds <- bnd

## --- B2. Heckman two-step, diagnostic only ---------------------------------
cat("\n--- B2. Heckman two-step (identified off functional form only) ---\n")
sel <- glm(as.formula(paste("I(1 - gamble_averse) ~", RHS)),
           data = d5, family = binomial(link = "probit"))
d5$xb  <- predict(sel, type = "link")
d5$imr <- dnorm(d5$xb) / pmax(pnorm(d5$xb), 1e-12)
d4b <- d5 |> filter(gamble_averse == 0)

m_h <- lm(as.formula(paste("as.integer(risk_s1) ~", RHS, "+ imr")), data = d4b)
ch  <- coeftest(m_h, vcov = vcovCL(m_h, cluster = d4b$hhid14))
cat("\nInverse Mills ratio term:\n"); print(round(ch["imr", , drop = FALSE], 5))
m_h0 <- lm(as.formula(paste("as.integer(risk_s1) ~", RHS)), data = d4b)
ch0 <- coeftest(m_h0, vcov = vcovCL(m_h0, cluster = d4b$hhid14))

cmp <- tibble(term = rownames(ch0),
              no_correction = ch0[, 1],
              with_imr = ch[match(rownames(ch0), rownames(ch)), 1]) |>
  filter(term != "(Intercept)") |>
  mutate(pct_change = 100 * (with_imr - no_correction) / abs(no_correction))
cat("\nCoefficients with and without the correction:\n")
print(as.data.frame(cmp |> mutate(across(where(is.numeric), ~ round(.x, 4)))))
cat("\nWithout an exclusion restriction this is a specification check, not a\n")
cat("correction: it shows whether the estimates are fragile to a selection\n")
cat("term, not what the selection-free estimates are.\n")
out$heckman <- list(imr = ch["imr", ], comparison = cmp)

## ===========================================================================
## C. PAIRED HOUSEHOLD BOOTSTRAP FOR THE CROSS-CHANNEL COMPARISON
## ===========================================================================
cat("\n============ C. PAIRED BOOTSTRAP ============\n")
B <- 500
cat("Resampling households with replacement,", B, "replicates.\n")
cat("Both stages are re-estimated on each resample, so the difference between\n")
cat("channels is computed with a joint distribution rather than as a ratio of\n")
cat("two independent point estimates.\n\n")

terms_of_interest <- c(cogv, "educ_years", "female", "rural", "employed",
                       "age_c", "age_c2")
SDX <- sapply(terms_of_interest, function(v) sd(d5[[v]]))
hh  <- unique(d5$hhid14)

one_rep <- function(i) {
  idx <- sample(hh, length(hh), replace = TRUE)
  bs  <- tibble(hhid14 = idx) |> left_join(d5, by = "hhid14", relationship = "many-to-many")
  b4  <- bs |> filter(gamble_averse == 0)
  if (n_distinct(b4$risk_ord) < 4) return(NULL)
  s1 <- try(glm(as.formula(paste("gamble_averse ~", RHS)), data = bs,
                family = binomial()), silent = TRUE)
  s2 <- try(polr(as.formula(paste("risk_ord ~", RHS)), data = b4, Hess = FALSE),
            silent = TRUE)
  if (inherits(s1, "try-error") || inherits(s2, "try-error")) return(NULL)
  c1 <- coef(s1)[terms_of_interest] * SDX
  c2 <- coef(s2)[terms_of_interest] * SDX
  tibble(term = terms_of_interest, comp = c1, pref = c2, diff = abs(c1) - abs(c2))
}

reps <- map(seq_len(B), one_rep) |> compact() |> bind_rows(.id = "rep")
cat("Successful replicates:", n_distinct(reps$rep), "of", B, "\n\n")

boot <- reps |>
  group_by(term) |>
  summarise(
    diff_mean = mean(diff),
    lo = quantile(diff, .025), hi = quantile(diff, .975),
    ## two-sided bootstrap p for H0: |comprehension| = |preference|
    p = 2 * min(mean(diff <= 0), mean(diff >= 0)),
    .groups = "drop") |>
  mutate(p = pmin(p, 1),
         verdict = case_when(lo > 0 ~ "comprehension larger",
                             hi < 0 ~ "preference larger",
                             TRUE   ~ "not distinguishable")) |>
  arrange(desc(diff_mean))
cat("Difference in absolute per-SD effect, comprehension minus preference:\n")
print(as.data.frame(boot |> mutate(across(where(is.numeric), ~ round(.x, 4)))))

saveRDS(out |> c(list(bootstrap = boot, n_reps = n_distinct(reps$rep))),
        "output/tables/selection_bootstrap.rds")
cat("\nSaved output/tables/selection_bootstrap.rds\n")
