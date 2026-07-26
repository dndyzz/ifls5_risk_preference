## 12_crossset_stability.R ---------------------------------------------------
## Discriminating test between the noise account and a preference account.
##
## Both accounts predict that low-ability respondents give more extreme
## responses. They differ on STABILITY: if extreme responses are noise, they
## should not reproduce across the two independent elicitations (Set 1 and
## Set 2). If they are genuine extreme preferences, they should.
##
## Set 2 differs from Set 1 in stakes and includes a loss prospect, so some
## disagreement is expected for everyone. That constant is not the test. The
## test is whether agreement rises with cognitive ability, and whether it does
## so specifically for respondents whose Set 1 answer was extreme.
##
## Run from the project root, after 07_comprehension_sample.R.
## Output: output/tables/crossset_stability.rds
## ---------------------------------------------------------------------------

suppressMessages({ library(tidyverse); library(sandwich); library(lmtest) })
set.seed(20260726)

S  <- readRDS("data/processed/comprehension_sample.rds")
d5 <- S$cc

d5 <- d5 |>
  mutate(cog_z = rowMeans(scale(across(c(raven_score, wscore, serial7_score,
                                         co07count, co10count)))),
         cog_q = ntile(cog_z, 5))

## Respondents scored on both sets. Set 2 "gamble averse" is the mirror-image
## screening failure (choosing the dominated risky option), so we build a
## comparable 5-level Set 2 index before comparing.
d5 <- d5 |>
  mutate(ga_s2  = as.integer(screen_s2 == "gamble_averse"),
         risk2_5g = if_else(ga_s2 == 1L, 0L, as.integer(risk_s2)))

both <- d5 |> filter(!is.na(risk_5g), !is.na(risk2_5g))

cat("========== CROSS-SET STABILITY ==========\n")
cat(sprintf("N scored on both sets = %d of %d\n\n", nrow(both), nrow(d5)))

## ---------------------------------------------------------------------------
## 1. Agreement by cognitive quintile
## ---------------------------------------------------------------------------
tab <- both |>
  group_by(cog_q) |>
  summarise(n = n(),
            cog_z    = mean(cog_z),
            rho      = cor(risk_5g, risk2_5g, method = "spearman"),
            exact    = 100 * mean(risk_5g == risk2_5g),
            mean_gap = mean(abs(risk_5g - risk2_5g)),
            .groups = "drop")
cat("Agreement between the two elicitations, by cognitive quintile:\n")
print(as.data.frame(tab |> mutate(across(c(cog_z, rho, mean_gap), ~ round(.x, 3)),
                                  exact = round(exact, 2))))
cat("\nUnder pure random responding the expected cross-set rank correlation is 0.\n")

## ---------------------------------------------------------------------------
## 2. Stability of EXTREME Set 1 answers specifically
## ---------------------------------------------------------------------------
cat("\n--- Do extreme Set 1 answers reproduce in Set 2? ---\n")
ext <- both |>
  mutate(s1_extreme = risk_5g %in% c(0, 4),
         s2_extreme = risk2_5g %in% c(0, 4)) |>
  group_by(cog_q) |>
  summarise(n_ext_s1 = sum(s1_extreme),
            ## among those extreme in Set 1, share also extreme in Set 2
            repro = 100 * mean(s2_extreme[s1_extreme]),
            ## and among those NOT extreme in Set 1
            base  = 100 * mean(s2_extreme[!s1_extreme]),
            .groups = "drop") |>
  mutate(lift = repro - base)
print(as.data.frame(ext |> mutate(across(c(repro, base, lift), ~ round(.x, 2)))))
cat("\n'repro' = P(extreme in Set 2 | extreme in Set 1); 'base' = P(extreme in\n")
cat("Set 2 | not extreme in Set 1). A genuine extreme preference should show a\n")
cat("large lift. Noise predicts a small one.\n")

## ---------------------------------------------------------------------------
## 3. Individual-level tests
## ---------------------------------------------------------------------------
cat("\n--- Individual-level regressions (household-clustered SEs) ---\n")

## (a) absolute cross-set discrepancy on ability
m_gap <- lm(abs(risk_5g - risk2_5g) ~ cog_z + female + age_c + age_c2 +
              educ_years + rural + employed, data = both)
cg <- coeftest(m_gap, vcov = vcovCL(m_gap, cluster = both$hhid14))
cat("\n(a) |Set1 - Set2| discrepancy:\n"); print(round(cg[1:4, ], 4))

## (b) exact agreement on ability
both <- both |> mutate(agree = as.integer(risk_5g == risk2_5g))
m_agr <- glm(agree ~ cog_z + female + age_c + age_c2 + educ_years + rural +
               employed, data = both, family = binomial())
ca <- coeftest(m_agr, vcov = vcovCL(m_agr, cluster = both$hhid14))
cat("\n(b) P(exact agreement):\n"); print(round(ca[1:4, ], 4))
cat("    OR per SD of ability =", round(exp(coef(m_agr)["cog_z"]), 4), "\n")

## (c) the sharpest version: among Set 1 extremes, does ability predict
##     reproducing the extreme in Set 2?
sub <- both |> filter(risk_5g %in% c(0, 4)) |>
  mutate(s2_extreme = as.integer(risk2_5g %in% c(0, 4)))
m_rep <- glm(s2_extreme ~ cog_z + female + age_c + age_c2 + educ_years +
               rural + employed, data = sub, family = binomial())
cr <- coeftest(m_rep, vcov = vcovCL(m_rep, cluster = sub$hhid14))
cat("\n(c) Among Set 1 extremes (n =", nrow(sub), "), P(extreme again in Set 2):\n")
print(round(cr[1:4, ], 4))
cat("    OR per SD of ability =", round(exp(coef(m_rep)["cog_z"]), 4), "\n")

## ---------------------------------------------------------------------------
## 4. What the two accounts predict, side by side
## ---------------------------------------------------------------------------
cat("\n--- Interpretation guide ---\n")
cat("Noise account      : rho rises with ability; agreement rises with ability;\n")
cat("                     extreme answers reproduce poorly at low ability.\n")
cat("Preference account : rho roughly flat in ability; extreme answers reproduce\n")
cat("                     equally well at all ability levels.\n")

out <- list(quintiles = tab, extremes = ext,
            m_gap = cg, m_agree = ca, m_repro = cr,
            n_both = nrow(both))
saveRDS(out, "output/tables/crossset_stability.rds")
cat("\nSaved output/tables/crossset_stability.rds\n")
