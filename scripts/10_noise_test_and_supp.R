## 10_noise_test_and_supp.R --------------------------------------------------
## I. Decisive test of the noise account: does the response distribution of
##    low-ability respondents converge on the coin-flip prediction?
## J. Set 2 (high stakes, includes a loss prospect) -- reported, not dropped
## K. Psychometrics for the 3-item BFI scales done properly
##
## Run from the project root, after 07_comprehension_sample.R.
## Output: output/tables/noise_test_and_supp.rds
## ---------------------------------------------------------------------------

suppressMessages({ library(MASS); library(tidyverse) })
set.seed(20260726)

S  <- readRDS("data/processed/comprehension_sample.rds")
d5 <- S$cc
d4 <- S$cc |> filter(gamble_averse == 0)
out <- list()

## ===========================================================================
## I. NOISE CONVERGENCE TEST
## ===========================================================================
cat("\n============ I. DOES LOW ABILITY CONVERGE ON COIN FLIP? ============\n")

## Analytic distribution over the 5 categories for a responder who picks the
## gamble with probability q at every node of the Set 1 tree.
noise_dist <- function(q) {
  c(G0 = (1 - q)^2,                        # certain at si01 and at si02
    G1 = (1 - (1 - q)^2) * (1 - q) * (1 - q),
    G2 = (1 - (1 - q)^2) * (1 - q) * q,
    G3 = (1 - (1 - q)^2) * q * (1 - q),
    G4 = (1 - (1 - q)^2) * q * q)
}
stopifnot(abs(sum(noise_dist(0.5)) - 1) < 1e-12)

## Composite cognitive score: mean of z-scored objective measures
d5 <- d5 |>
  mutate(cog_z = rowMeans(scale(across(c(raven_score, wscore, serial7_score,
                                         co07count, co10count)))),
         cog_q = ntile(cog_z, 5))

## chi-square style distance between an observed distribution and the
## coin-flip prediction
dist_to_noise <- function(p_obs, q = 0.5) {
  p_exp <- noise_dist(q)
  sum((p_obs - p_exp)^2 / p_exp)
}

qt <- d5 |>
  group_by(cog_q) |>
  summarise(n = n(),
            cog_z = mean(cog_z),
            G0 = mean(risk_5g == 0), G1 = mean(risk_5g == 1),
            G2 = mean(risk_5g == 2), G3 = mean(risk_5g == 3),
            G4 = mean(risk_5g == 4), .groups = "drop") |>
  rowwise() |>
  mutate(dist_coinflip = dist_to_noise(c(G0, G1, G2, G3, G4))) |>
  ungroup()

cat("\nObserved distribution by cognitive quintile (%), plus distance to the\n")
cat("coin-flip prediction (lower = more consistent with random responding):\n\n")
print(as.data.frame(qt |> mutate(across(c(G0, G1, G2, G3, G4), ~ round(100 * .x, 2)),
                                 cog_z = round(cog_z, 3),
                                 dist_coinflip = round(dist_coinflip, 4))))
cat("\nCoin-flip reference (q = .5), %:\n")
print(round(100 * noise_dist(0.5), 2))

cat("\nSpearman correlation between cognitive quintile and distance-to-noise: ")
cat(round(cor(qt$cog_q, qt$dist_coinflip, method = "spearman"), 3), "\n")

## Best-fitting implied q per quintile: if low-ability respondents are noisier,
## their behaviour should be closer to some q, and the fit should be better.
fit_q <- function(p_obs) {
  o <- optimize(function(q) dist_to_noise(p_obs, q), c(0.05, 0.95))
  c(q_hat = o$minimum, dist = o$objective)
}
qfit <- qt |> rowwise() |>
  mutate(q_hat = fit_q(c(G0, G1, G2, G3, G4))["q_hat"],
         dist_best = fit_q(c(G0, G1, G2, G3, G4))["dist"]) |>
  ungroup() |>
  select(cog_q, n, q_hat, dist_best)
cat("\nBest-fitting q and residual distance by quintile:\n")
print(as.data.frame(qfit |> mutate(across(c(q_hat, dist_best), ~ round(.x, 4)))))

## Interior-response share is the sharpest single summary: pure noise cannot
## produce a concentrated interior, and genuine graded preference should.
cat("\nInterior share (Groups 2+3) by cognitive quintile:\n")
print(as.data.frame(
  d5 |> group_by(cog_q) |>
    summarise(n = n(), interior_pct = round(100 * mean(risk_5g %in% 2:3), 2),
              extreme_pct = round(100 * mean(risk_5g %in% c(0, 4)), 2),
              .groups = "drop")))
cat("\nCoin-flip interior share =", round(100 * sum(noise_dist(0.5)[3:4]), 2), "%\n")

## Formal test: is extreme (0 or 4) vs interior (1,2,3) predicted by ability?
d5 <- d5 |> mutate(extreme = as.integer(risk_5g %in% c(0, 4)))
m_ext <- glm(extreme ~ cog_z + female + age_c + age_c2 + educ_years + rural +
               employed, data = d5, family = binomial())
cat("\nLogit: P(extreme category | ability), N =", nrow(d5), "\n")
print(round(summary(m_ext)$coefficients, 4))
cat("\nOdds ratio per SD of composite cognitive score:",
    round(exp(coef(m_ext)["cog_z"]), 4), "\n")

out$quintiles <- qt; out$qfit <- qfit; out$extreme_model <- m_ext

## ===========================================================================
## J. SET 2 -- reported rather than dropped
## ===========================================================================
cat("\n===================== J. SET 2 =====================\n")
cat("Set 2 escalates stakes AND changes the prospect structure:\n")
cat("  si13: certain Rp 4m  vs (Rp 12m, Rp 0)\n")
cat("  si14: certain Rp 4m  vs (Rp 8m,  Rp 2m)\n")
cat("  si15: certain Rp 4m  vs (Rp 16m, LOSS of Rp 2m)   <-- loss domain\n")
cat("So Group 4 in Set 2 indexes loss tolerance, not simply risk tolerance.\n")
cat("The manuscript's claim that the IFLS elicitation involves no possibility\n")
cat("of loss holds for Set 1 only.\n\n")

s2 <- d5 |> filter(!is.na(risk_s2))
cat("Set 2 distribution among the analytic sample:\n")
print(as.data.frame(d5 |> count(risk_s2) |>
                      mutate(pct = round(100 * n / sum(n), 2))))
cat("\nSet 2 screening outcomes:\n")
print(as.data.frame(d5 |> count(screen_s2) |>
                      mutate(pct = round(100 * n / sum(n), 2))))

cat("\nCross-set agreement among those scored on both:\n")
both <- d5 |> filter(!is.na(risk_s1), !is.na(risk_s2))
cat(sprintf("  N = %d | Pearson r = %.3f | Spearman rho = %.3f | exact agreement = %.1f%%\n",
            nrow(both), cor(both$risk_s1, both$risk_s2),
            cor(both$risk_s1, both$risk_s2, method = "spearman"),
            100 * mean(both$risk_s1 == both$risk_s2)))

cat("\nPrimary specification re-estimated on Set 2:\n")
d2 <- d5 |> filter(!is.na(risk_s2)) |> mutate(risk_ord2 = factor(risk_s2, ordered = TRUE))
m_s2mod <- polr(risk_ord2 ~ co07count + co10count + serial7_score + memory_self +
                  wscore + raven_score + female + employed + age_c + age_c2 +
                  educ_years + rural, data = d2, Hess = TRUE)
print(round(summary(m_s2mod)$coefficients, 4))
out$set2_model <- m_s2mod

## ===========================================================================
## K. PSYCHOMETRICS DONE PROPERLY
## ===========================================================================
cat("\n===================== K. BFI-15 PSYCHOMETRICS =====================\n")
cat("Alpha on a 3-item scale is bounded by the mean inter-item correlation;\n")
cat("reporting alpha alone overstates how anomalous these values are.\n\n")

## BFI-15 (Gerlitz & Schupp, 2005) mapping, as used in 02_big_five.Rmd
trait_items <- list(
  openness          = c("psn_03", "psn_08", "psn_10"),   # original, imagination, artistic
  conscientiousness = c("psn_02", "psn_09r", "psn_12"),  # thorough, lazy(R), efficient
  extraversion      = c("psn_01", "psn_04r", "psn_13"),  # talkative, reserved(R), outgoing
  agreeableness     = c("psn_06", "psn_11", "psn_14r"),  # forgiving, kind, rude(R)
  neuroticism       = c("psn_05r", "psn_07", "psn_15")   # relaxed(R), worries, nervous
)

psy <- imap_dfr(trait_items, function(items, trait) {
  present <- items[items %in% names(d4)]
  if (length(present) < 2) return(tibble(trait = trait, k = length(present)))
  X <- as.matrix(d4[, present]); k <- ncol(X)
  R <- cor(X); mic <- mean(R[lower.tri(R)])
  alpha <- (k * mic) / (1 + (k - 1) * mic)             # standardized alpha
  ## raw alpha
  v <- var(X); a_raw <- (k / (k - 1)) * (1 - sum(diag(v)) / sum(v))
  ## Spearman-Brown: alpha implied if the scale had 10 items at the same MIC
  sb10 <- (10 * mic) / (1 + 9 * mic)
  tibble(trait = trait, k = k, mean_inter_item_r = mic,
         alpha_raw = a_raw, alpha_std = alpha, alpha_if_10_items = sb10)
})
print(as.data.frame(psy |> mutate(across(where(is.numeric), ~ round(.x, 3)))))

cat("\nThe same information as a reliability-attenuation correction: an\n")
cat("observed trait-outcome correlation r is attenuated by sqrt(alpha).\n")
print(as.data.frame(psy |> transmute(trait,
                                     alpha_std = round(alpha_std, 3),
                                     attenuation_factor = round(sqrt(alpha_std), 3),
                                     disattenuation_multiplier = round(1 / sqrt(alpha_std), 2))))
out$psychometrics <- psy

saveRDS(out, "output/tables/noise_test_and_supp.rds")
cat("\nSaved output/tables/noise_test_and_supp.rds\n")
