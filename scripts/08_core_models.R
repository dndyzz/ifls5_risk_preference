## 08_core_models.R ----------------------------------------------------------
## Diagnostics and re-specification for the risk-preference paper.
##
##  A. Proportional-odds test on the published primary specification
##  B. Household-clustered standard errors
##  C. Multinomial logit, to expose the non-monotone cognition profile that the
##     ordered model forces into a single monotone latent index
##  D. Two-stage (hurdle) model: comprehension channel then preference channel
##  E. Ordered model of the 3-level comprehension gradient
##
## Run from the project root, after 07_comprehension_sample.R.
## Output: output/tables/core_models.rds
## ---------------------------------------------------------------------------

suppressMessages({
  library(MASS); library(tidyverse); library(sandwich); library(lmtest)
  library(brant); library(nnet)
})

S  <- readRDS("data/processed/comprehension_sample.rds")
d5 <- S$cc                                   # 25,040: all screening outcomes
d4 <- S$cc |> filter(gamble_averse == 0)     # 17,886: primary sample

stopifnot(nrow(d4) == 17886, nrow(d5) == 25040)

psn <- S$psn_items; cogv <- S$cog_vars
rhs_demo <- "female + employed + age_c + age_c2 + educ_years + rural"
rhs_cog  <- paste(cogv, collapse = " + ")
rhs_psn  <- paste(psn,  collapse = " + ")
rhs_full <- paste(rhs_psn, rhs_cog, rhs_demo, sep = " + ")

out <- list()

## ===========================================================================
## A. PROPORTIONAL ODDS
## ===========================================================================
cat("\n===================== A. PROPORTIONAL ODDS =====================\n")

## Parsimonious specification for the PO test: brant() needs a well-conditioned
## design matrix, and the test is about the outcome structure, not the controls.
f_po <- as.formula(paste("risk_ord ~", rhs_cog, "+", rhs_demo))
m_po <- polr(f_po, data = d4, Hess = TRUE, method = "logistic")

brant_res <- try(brant(m_po), silent = TRUE)
if (!inherits(brant_res, "try-error")) {
  cat("\nBrant test (omnibus + per-predictor):\n"); print(brant_res)
  out$brant <- brant_res
} else {
  cat("brant() failed:", conditionMessage(attr(brant_res, "condition")), "\n")
}

## Direct evidence: separate binary logits at each cumulative cut point.
## Under proportional odds the coefficient on a predictor should be stable
## across cut points.
cat("\nCut-point-specific binary logits (Y > k), key predictors:\n")
cutfit <- map_dfr(1:3, function(k) {
  dk <- d4 |> mutate(y = as.integer(as.integer(risk_ord) > k))
  mk <- glm(as.formula(paste("y ~", rhs_cog, "+", rhs_demo)),
            data = dk, family = binomial())
  ck <- coeftest(mk, vcov = vcovCL(mk, cluster = dk$hhid14))
  tibble(cut = paste0("Y>", k), term = rownames(ck),
         est = ck[, 1], se = ck[, 2], p = ck[, 4])
})
print(cutfit |>
        filter(term %in% c("raven_score", "wscore", "serial7_score",
                           "co07count", "memory_self", "female", "educ_years")) |>
        mutate(across(c(est, se), ~ round(.x, 4)), p = round(p, 4)) |>
        pivot_wider(names_from = cut, values_from = c(est, se, p)) |>
        as.data.frame())
out$cutfit <- cutfit

## ===========================================================================
## B. CLUSTERED STANDARD ERRORS
## ===========================================================================
cat("\n===================== B. CLUSTERED SE =====================\n")
cat(sprintf("individuals = %d | households = %d | provinces = %d\n",
            nrow(d4), n_distinct(d4$hhid14), n_distinct(d4$province)))

f_full <- as.formula(paste("risk_ord ~", rhs_full))
m_ord_full <- polr(f_full, data = d4, Hess = TRUE, method = "logistic")

naive <- coeftest(m_ord_full)
clust <- coeftest(m_ord_full, vcov = vcovCL(m_ord_full, cluster = d4$hhid14))

cmp <- tibble(
  term    = rownames(naive),
  est     = naive[, 1],
  se_iid  = naive[, 2],
  se_hh   = clust[match(rownames(naive), rownames(clust)), 2],
  p_iid   = naive[, 4],
  p_hh    = clust[match(rownames(naive), rownames(clust)), 4]
) |>
  filter(!term %in% c("1|2", "2|3", "3|4")) |>
  mutate(se_ratio = se_hh / se_iid,
         flips = (p_iid < .05) & (p_hh >= .05))

cat("\nSE inflation from household clustering: median ratio =",
    round(median(cmp$se_ratio), 4), "| max =", round(max(cmp$se_ratio), 4), "\n")
cat("\nPredictors significant at .05 naively but NOT under clustering:\n")
print(cmp |> filter(flips) |>
        mutate(across(c(est, se_iid, se_hh, p_iid, p_hh), ~ round(.x, 4))) |>
        as.data.frame())
cat("\nAll cognitive + key demographic terms:\n")
print(cmp |> filter(term %in% c(cogv, "female", "educ_years", "age_c", "age_c2",
                                "rural", "employed")) |>
        mutate(across(c(est, se_iid, se_hh, p_iid, p_hh, se_ratio), ~ round(.x, 4))) |>
        as.data.frame())
out$clustered <- cmp

## ===========================================================================
## C. MULTINOMIAL LOGIT — non-monotone cognition
## ===========================================================================
cat("\n===================== C. MULTINOMIAL =====================\n")
d4m <- d4 |> mutate(y = relevel(factor(as.integer(risk_ord)), ref = "1"))
m_mnl <- multinom(as.formula(paste("y ~", rhs_cog, "+", rhs_demo)),
                  data = d4m, trace = FALSE, maxit = 500)

cat("\nMultinomial coefficients (reference = Group 1, most risk averse):\n")
mnl_co <- summary(m_mnl)$coefficients
mnl_se <- summary(m_mnl)$standard.errors
mnl_tab <- map_dfr(rownames(mnl_co), function(g)
  tibble(group = g, term = colnames(mnl_co),
         est = mnl_co[g, ], se = mnl_se[g, ],
         z = mnl_co[g, ] / mnl_se[g, ],
         p = 2 * pnorm(-abs(mnl_co[g, ] / mnl_se[g, ]))))
print(mnl_tab |>
        filter(term %in% c("raven_score", "wscore", "serial7_score", "female")) |>
        mutate(across(c(est, se, z), ~ round(.x, 4)), p = round(p, 4)) |>
        as.data.frame())

cat("\nLR test: multinomial vs ordered logit (same predictors)\n")
ll_ord <- as.numeric(logLik(m_po)); k_ord <- attr(logLik(m_po), "df")
ll_mnl <- as.numeric(logLik(m_mnl)); k_mnl <- length(coef(m_mnl))
lr <- 2 * (ll_mnl - ll_ord); ddf <- k_mnl - k_ord
cat(sprintf("  ordered  LL = %.2f (k = %d)\n  multinom LL = %.2f (k = %d)\n",
            ll_ord, k_ord, ll_mnl, k_mnl))
cat(sprintf("  LR = %.2f, df = %d, p = %.3g\n", lr, ddf, pchisq(lr, ddf, lower.tail = FALSE)))
out$mnl <- mnl_tab
out$lr_mnl_vs_ord <- c(lr = lr, df = ddf,
                       p = pchisq(lr, ddf, lower.tail = FALSE))

## ===========================================================================
## D. TWO-STAGE (HURDLE) MODEL
## ===========================================================================
cat("\n===================== D. TWO-STAGE MODEL =====================\n")

## Stage 1 — comprehension: who confirms a dominated choice?
cat("\n--- Stage 1: P(gamble averse) on the full 25,040 sample ---\n")
m_s1 <- glm(as.formula(paste("gamble_averse ~", rhs_cog, "+", rhs_demo)),
            data = d5, family = binomial())
s1 <- coeftest(m_s1, vcov = vcovCL(m_s1, cluster = d5$hhid14))
s1_tab <- tibble(term = rownames(s1), est = s1[, 1], se = s1[, 2], p = s1[, 4]) |>
  mutate(OR = exp(est),
         ## standardized: change in log-odds per SD of predictor
         sd_x = map_dbl(term, ~ if (.x %in% names(d5) && is.numeric(d5[[.x]]))
                                  sd(d5[[.x]]) else NA_real_),
         est_sd = est * sd_x)
print(s1_tab |> mutate(across(where(is.numeric), ~ round(.x, 4))) |> as.data.frame())
cat(sprintf("\nStage 1 McFadden R2 = %.4f\n",
            1 - as.numeric(logLik(m_s1)) /
              as.numeric(logLik(glm(gamble_averse ~ 1, data = d5, family = binomial())))))

## Stage 2 — preference: ordered logit among respondents who comprehended
cat("\n--- Stage 2: ordered logit on groups 1-4 among comprehenders ---\n")
m_s2 <- polr(f_po, data = d4, Hess = TRUE, method = "logistic")
s2 <- coeftest(m_s2, vcov = vcovCL(m_s2, cluster = d4$hhid14))
s2_tab <- tibble(term = rownames(s2), est = s2[, 1], se = s2[, 2], p = s2[, 4]) |>
  filter(!term %in% c("1|2", "2|3", "3|4")) |>
  mutate(sd_x = map_dbl(term, ~ if (.x %in% names(d4) && is.numeric(d4[[.x]]))
                                  sd(d4[[.x]]) else NA_real_),
         est_sd = est * sd_x)
print(s2_tab |> mutate(across(where(is.numeric), ~ round(.x, 4))) |> as.data.frame())

cat("\n--- Side by side: same predictor, two channels (per-SD log-odds) ---\n")
side <- s1_tab |> select(term, comprehension_channel = est_sd, p_s1 = p) |>
  inner_join(s2_tab |> select(term, preference_channel = est_sd, p_s2 = p),
             by = "term") |>
  filter(term != "(Intercept)") |>
  mutate(ratio = abs(comprehension_channel) / abs(preference_channel))
print(side |> mutate(across(where(is.numeric), ~ round(.x, 4))) |> as.data.frame())
out$stage1 <- s1_tab; out$stage2 <- s2_tab; out$side <- side

## ===========================================================================
## E. COMPREHENSION GRADIENT (3 ordered levels)
## ===========================================================================
cat("\n===================== E. COMPREHENSION GRADIENT =====================\n")
m_grad <- polr(as.formula(paste("comprehension ~", rhs_cog, "+", rhs_demo)),
               data = d5, Hess = TRUE, method = "logistic")
g <- coeftest(m_grad, vcov = vcovCL(m_grad, cluster = d5$hhid14))
grad_tab <- tibble(term = rownames(g), est = g[, 1], se = g[, 2], p = g[, 4]) |>
  filter(!str_detect(term, "\\|")) |>
  mutate(sd_x = map_dbl(term, ~ if (.x %in% names(d5) && is.numeric(d5[[.x]]))
                                  sd(d5[[.x]]) else NA_real_),
         est_sd = est * sd_x, OR_per_SD = exp(est_sd))
print(grad_tab |> mutate(across(where(is.numeric), ~ round(.x, 4))) |> as.data.frame())

cat("\nBrant test on the comprehension gradient:\n")
bg <- try(brant(m_grad), silent = TRUE)
if (!inherits(bg, "try-error")) print(bg) else cat("  (failed)\n")
out$gradient <- grad_tab

saveRDS(out, "output/tables/core_models.rds")
cat("\nSaved output/tables/core_models.rds\n")
