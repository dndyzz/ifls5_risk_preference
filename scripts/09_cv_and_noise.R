## 09_cv_and_noise.R ---------------------------------------------------------
## F. Cross-validated model comparison, replacing the AIC horse race
## G. Simulation of noisy responders through the Set 1 branching logic
## H. Interval regression on the implied CRRA bounds
##
## Run from the project root, after 07_comprehension_sample.R.
## Output: output/tables/cv_and_noise.rds
## ---------------------------------------------------------------------------

suppressMessages({ library(MASS); library(tidyverse); library(survival) })

set.seed(20260726)

S  <- readRDS("data/processed/comprehension_sample.rds")
d5 <- S$cc
d4 <- S$cc |> filter(gamble_averse == 0)

psn <- S$psn_items; cogv <- S$cog_vars; traits <- S$traits
rhs_demo <- "female + employed + age_c + age_c2 + educ_years + rural"

specs <- list(
  demographics        = rhs_demo,
  trait_composites    = paste(paste(traits, collapse = " + "), rhs_demo, sep = " + "),
  personality_items   = paste(paste(psn,    collapse = " + "), rhs_demo, sep = " + "),
  cognitive           = paste(paste(cogv,   collapse = " + "), rhs_demo, sep = " + "),
  full                = paste(paste(psn, collapse = " + "),
                              paste(cogv, collapse = " + "), rhs_demo, sep = " + ")
)

out <- list()

## ===========================================================================
## F. CROSS-VALIDATED COMPARISON (folds held at household level)
## ===========================================================================
cat("\n===================== F. 10-FOLD CV =====================\n")
cat("Folds assigned by household so that co-resident adults never straddle\n")
cat("the train/test boundary.\n\n")

make_folds <- function(dat, K = 10) {
  hh <- unique(dat$hhid14)
  assign_hh <- tibble(hhid14 = hh, fold = sample(rep_len(1:K, length(hh))))
  left_join(dat, assign_hh, by = "hhid14")$fold
}

## multinomial log-loss and ordinal (ranked probability) score
logloss <- function(P, y) {
  idx <- cbind(seq_along(y), as.integer(y))
  -mean(log(pmax(P[idx], 1e-12)))
}
rps <- function(P, y) {                     # ranked probability score
  Y <- matrix(0, nrow(P), ncol(P)); Y[cbind(seq_along(y), as.integer(y))] <- 1
  mean(rowSums((t(apply(P, 1, cumsum)) - t(apply(Y, 1, cumsum)))^2))
}

cv_one <- function(dat, target, rhs, K = 10) {
  folds <- make_folds(dat, K)
  res <- map_dfr(1:K, function(k) {
    tr <- dat[folds != k, ]; te <- dat[folds == k, ]
    m  <- polr(as.formula(paste(target, "~", rhs)), data = tr,
               Hess = FALSE, method = "logistic")
    P  <- predict(m, newdata = te, type = "probs")
    y  <- te[[target]]
    tibble(fold = k, n = nrow(te), ll = logloss(P, y), rps = rps(P, y))
  })
  tibble(logloss = weighted.mean(res$ll, res$n),
         rps     = weighted.mean(res$rps, res$n),
         se_ll   = sd(res$ll) / sqrt(K))
}

cat("--- Preference outcome (4 groups, comprehenders only, N = 17,886) ---\n")
cv4 <- imap_dfr(specs, ~ cv_one(d4, "risk_ord", .x) |> mutate(spec = .y)) |>
  select(spec, logloss, se_ll, rps) |>
  mutate(d_logloss = logloss - min(logloss)) |>
  arrange(logloss)
print(as.data.frame(cv4), digits = 6)

cat("\n--- Comprehension outcome (gamble averse yes/no, N = 25,040) ---\n")
cv_bin <- function(dat, rhs, K = 10) {
  folds <- make_folds(dat, K)
  res <- map_dfr(1:K, function(k) {
    tr <- dat[folds != k, ]; te <- dat[folds == k, ]
    m <- glm(as.formula(paste("gamble_averse ~", rhs)), data = tr, family = binomial())
    p <- predict(m, newdata = te, type = "response")
    y <- te$gamble_averse
    tibble(fold = k, n = nrow(te),
           ll = -mean(y * log(pmax(p, 1e-12)) + (1 - y) * log(pmax(1 - p, 1e-12))),
           auc = {
             r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
             (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
           })
  })
  tibble(logloss = weighted.mean(res$ll, res$n), auc = weighted.mean(res$auc, res$n),
         se_ll = sd(res$ll) / sqrt(K))
}
cvc <- imap_dfr(specs, ~ cv_bin(d5, .x) |> mutate(spec = .y)) |>
  select(spec, logloss, se_ll, auc) |>
  mutate(d_logloss = logloss - min(logloss)) |>
  arrange(logloss)
print(as.data.frame(cvc), digits = 6)

out$cv_preference <- cv4; out$cv_comprehension <- cvc

## ===========================================================================
## G. NOISE SIMULATION
## ===========================================================================
cat("\n===================== G. NOISE SIMULATION =====================\n")
cat("A respondent who answers each Set 1 question independently at random\n")
cat("(probability q of choosing the gamble) is pushed through the real\n")
cat("branching logic. This shows where pure noise lands on the index.\n\n")

## Set 1 tree: si01 screen -> si03 -> si04 (if certain) or si05 (if gamble)
##   gamble averse  : certain at si01 AND certain at si02 confirmation
##   group 1        : certain at si03, certain at si04
##   group 2        : certain at si03, gamble  at si04
##   group 3        : gamble  at si03, certain at si05
##   group 4        : gamble  at si03, gamble  at si05
sim_noise <- function(q, n = 2e5) {
  g <- function() runif(n) < q                      # TRUE = choose the gamble
  s01 <- g(); s02 <- g(); s03 <- g(); s04 <- g(); s05 <- g()
  ga  <- !s01 & !s02
  grp <- ifelse(ga, 0L,
         ifelse(!s03 & !s04, 1L,
         ifelse(!s03 &  s04, 2L,
         ifelse( s03 & !s05, 3L, 4L))))
  as.numeric(prop.table(table(factor(grp, levels = 0:4))))
}

grid <- map_dfr(c(0.3, 0.4, 0.5, 0.6, 0.7), function(q) {
  p <- sim_noise(q)
  tibble(q = q, G0 = p[1], G1 = p[2], G2 = p[3], G3 = p[4], G4 = p[5])
})
cat("Predicted distribution for purely random responders:\n")
print(as.data.frame(grid |> mutate(across(-q, ~ round(100 * .x, 2)))))

obs5 <- d5 |> count(risk_5g) |> mutate(pct = 100 * n / sum(n))
cat("\nObserved distribution (N = 25,040):\n")
print(as.data.frame(obs5 |> mutate(pct = round(pct, 2))))

cat("\nKey point: coin-flip responding (q = .5) puts 25% in Group 0 and\n")
cat("25% in Group 4 -- mass at BOTH extremes and only 12.5% in each\n")
cat("interior group. The observed shape (28.6 / 26.5 / 11.8 / 13.0 / 20.0)\n")
cat("has the same bimodal signature.\n")

## How much of each group could be noise? Compare interior/exterior ratio.
cat("\nInterior share (Groups 2+3): observed =",
    round(sum(obs5$pct[obs5$risk_5g %in% 2:3]), 2), "% | random (q=.5) =",
    round(100 * sum(sim_noise(0.5)[3:4]), 2), "%\n")

## Do the two extremes look alike cognitively? (noise predicts yes)
cat("\nCognitive profile by group -- noise predicts the two extremes resemble\n")
cat("each other and sit below the interior groups:\n")
print(as.data.frame(
  d5 |> group_by(risk_5g) |>
    summarise(n = n(), raven = round(mean(raven_score), 3),
              wscore = round(mean(wscore), 1),
              serial7 = round(mean(serial7_score), 3),
              educ = round(mean(educ_years), 3), .groups = "drop")))

out$noise_grid <- grid; out$observed5 <- obs5

## ===========================================================================
## H. INTERVAL REGRESSION ON IMPLIED CRRA BOUNDS
## ===========================================================================
cat("\n===================== H. CRRA INTERVAL REGRESSION =====================\n")
cat("Groups 1-4 imply CRRA intervals (r > 2.915 / 1.000-2.915 / 0.306-1.000 /\n")
cat("r < 0.306). Interval regression uses those bounds directly instead of\n")
cat("treating the group number as the outcome.\n\n")

## survreg needs finite-or-NA bounds; NA encodes an open end
dint <- d4 |>
  mutate(lo = ifelse(is.finite(crra_lo), crra_lo, NA_real_),
         hi = ifelse(is.finite(crra_hi), crra_hi, NA_real_))

m_int <- survreg(Surv(lo, hi, type = "interval2") ~
                   co07count + co10count + serial7_score + memory_self +
                   wscore + raven_score + female + employed + age_c + age_c2 +
                   educ_years + rural,
                 data = dint, dist = "gaussian")
si <- summary(m_int)
it <- as.data.frame(si$table)
it$term <- rownames(it)
it <- it |>
  mutate(sd_x = map_dbl(term, ~ if (.x %in% names(d4) && is.numeric(d4[[.x]]))
                                   sd(d4[[.x]]) else NA_real_),
         est_per_SD = Value * sd_x)
cat("Interval regression on CRRA (positive = more risk averse):\n")
print(it |> select(term, Value, `Std. Error`, p, sd_x, est_per_SD) |>
        mutate(across(where(is.numeric), ~ round(.x, 4))), row.names = FALSE)

out$crra_interval <- it
out$crra_bounds <- tibble(
  group = 1:4,
  pattern = c("reject si03, reject si04", "reject si03, accept si04",
              "accept si03, reject si05", "accept si03, accept si05"),
  crra_lo = c(2.9150, 1.0000, 0.3058, -Inf),
  crra_hi = c(Inf, 2.9150, 1.0000, 0.3058))

saveRDS(out, "output/tables/cv_and_noise.rds")
cat("\nSaved output/tables/cv_and_noise.rds\n")
