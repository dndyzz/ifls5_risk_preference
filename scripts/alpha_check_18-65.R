# ==============================================================================
# Reliability re-check on analytic sample (age 18-65)
# ==============================================================================

analytic <- readRDS("data/processed/analytic_sample_18_65.rds")

trait_items <- list(
  extraversion      = c("psn_01", "psn_04r", "psn_13"),
  agreeableness     = c("psn_06", "psn_11", "psn_14r"),
  conscientiousness = c("psn_02", "psn_09r", "psn_12"),
  neuroticism       = c("psn_05r", "psn_07", "psn_15"),
  openness          = c("psn_03", "psn_08", "psn_10")
)

alpha_analytic <- map_dfr(trait_items, function(items) {
  mat <- analytic |> select(all_of(items)) |> drop_na()
  a <- psych::alpha(mat, check.keys = FALSE)
  tibble(
    alpha_raw  = a$total$raw_alpha,
    alpha_std  = a$total$std.alpha,
    n_complete = nrow(mat)
  )
}, .id = "trait")

print(alpha_analytic)