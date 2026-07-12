library(ggplot2)
library(dplyr)

proj <- "."  # run from the project root
outd <- file.path(proj, "output/figures/submission")
dir.create(outd, recursive = TRUE, showWarnings = FALSE)

r  <- readRDS(file.path(proj, "output/tables/baseline_results_18_65.rds"))
rb <- readRDS(file.path(proj, "output/tables/robustness_5group.rds"))

NAVY <- "#2E5EAA"; GOLD <- "#B8860B"
INK <- "#1a1a1a"; GRID <- "grey88"

thm <- theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text  = element_text(color = INK),
    axis.title = element_text(color = INK),
    legend.text = element_text(color = INK),
    strip.text = element_text(color = INK, face = "bold", hjust = 0),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "top"
  )

save_fig <- function(p, name, w, h) {
  ggsave(file.path(outd, paste0(name, ".png")), p, width = w, height = h,
         units = "mm", dpi = 300, bg = "white")
  ggsave(file.path(outd, paste0(name, ".pdf")), p, width = w, height = h,
         units = "mm", device = cairo_pdf, bg = "white")
  cat("saved:", name, "\n")
}

lab <- c(
  female = "Female", employed = "Employed", age_c = "Age (centered)",
  age_c2 = "Age²", educ_years = "Education (years)", rural = "Rural",
  wscore = "Number series (W-score)", co07count = "Word recall (immediate)",
  co10count = "Word recall (delayed)", memory_self = "Self-rated memory",
  raven_score = "Raven's matrices", serial7_score = "Serial 7 score",
  psn_01 = "PSN_01 (talkative)", psn_02 = "PSN_02", psn_03 = "PSN_03",
  psn_04r = "PSN_04r (reserved, rev.)", psn_05r = "PSN_05r",
  psn_06 = "PSN_06", psn_07 = "PSN_07 (worries a lot)",
  psn_08 = "PSN_08 (active imagination)", psn_09r = "PSN_09r (lazy, rev.)",
  psn_10 = "PSN_10", psn_11 = "PSN_11", psn_12 = "PSN_12", psn_13 = "PSN_13",
  psn_14r = "PSN_14r", psn_15 = "PSN_15"
)
dom <- function(term) case_when(
  term %in% c("female","employed","age_c","age_c2","educ_years","rural") ~ "Demographics",
  term %in% c("wscore","co07count","co10count","memory_self","raven_score","serial7_score") ~ "Cognitive",
  TRUE ~ "Personality items"
)

# ---- Figure 1: distribution of 4-group risk index -----------------------------
d1 <- r$by_risk |>
  transmute(group = risk_s1, n) |>
  mutate(pct = n / sum(n) * 100,
         glab = c("1\nMost risk averse", "2", "3", "4\nMost risk seeking"))

f1 <- ggplot(d1, aes(factor(group), pct)) +
  geom_col(fill = NAVY, width = 0.62) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.5,
            size = 3.2, color = INK) +
  scale_x_discrete(labels = d1$glab) +
  scale_y_continuous(limits = c(0, 42), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Risk aversion group (Set 1)", y = "Share of respondents (%)") +
  thm + theme(panel.grid.major.x = element_blank())
save_fig(f1, "Figure1_risk_distribution", 140, 95)

# ---- Figure 2: standardized coefficients, full OLS model ----------------------
d2 <- r$ols_std |>
  mutate(
    domain = factor(dom(term), c("Demographics", "Cognitive", "Personality items")),
    label  = lab[term],
    lo = std_beta - 1.96 * std_se,
    hi = std_beta + 1.96 * std_se,
    sig = ifelse(p.value < .05, "p < .05", "p ≥ .05")
  ) |>
  arrange(domain, abs(std_beta)) |>
  mutate(label = factor(label, levels = unique(label)))

f2 <- ggplot(d2, aes(std_beta, label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey55", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, color = NAVY, linewidth = 0.55) +
  geom_point(aes(fill = sig), shape = 21, size = 2.4, color = NAVY, stroke = 0.7) +
  scale_fill_manual(values = c("p < .05" = NAVY, "p ≥ .05" = "white"), name = NULL) +
  facet_grid(domain ~ ., scales = "free_y", space = "free_y") +
  labs(x = "Standardized coefficient (positive = more risk seeking), 95% CI", y = NULL) +
  thm + theme(panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_line(color = GRID, linewidth = 0.3))
save_fig(f2, "Figure2_ols_coefficients", 170, 190)

# ---- Figure 3: 4-group vs 5-group distribution --------------------------------
d3a <- r$by_risk |>
  transmute(group = as.character(risk_s1), pct = n / sum(n) * 100,
            spec = "Primary (4-group, N = 17,886)")
d3b <- rb$risk_dist |>
  transmute(group = as.character(risk_5g), pct,
            spec = "Robustness (5-group, N = 25,040)")
d3 <- bind_rows(d3a, d3b) |>
  mutate(group = factor(group, 0:4, c("0\nGamble averse", "1\nMost risk\naverse", "2", "3", "4\nMost risk\nseeking")),
         spec  = factor(spec, unique(c(d3a$spec, d3b$spec))))

f3 <- ggplot(d3, aes(group, pct, fill = spec)) +
  geom_col(position = position_dodge2(width = 0.8, preserve = "single", padding = 0.08),
           width = 0.75) +
  geom_text(aes(label = sprintf("%.1f", pct)),
            position = position_dodge2(width = 0.75, preserve = "single", padding = 0.08),
            vjust = -0.45, size = 2.8, color = INK) +
  scale_fill_manual(values = c(NAVY, GOLD), name = NULL) +
  scale_y_continuous(limits = c(0, 42), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Risk index group", y = "Share of respondents (%)") +
  thm + theme(panel.grid.major.x = element_blank())
save_fig(f3, "Figure3_distribution_4g_vs_5g", 170, 100)

# ---- Figure 4: coefficient comparison, primary vs robustness ------------------
terms4 <- c("female", "educ_years", "raven_score", "wscore", "serial7_score",
            "co07count", "memory_self", "psn_09r", "psn_04r", "psn_08",
            "rural", "age_c2")
d4 <- bind_rows(
  r$ols_std  |> filter(term %in% terms4) |>
    transmute(term, beta = std_beta, spec = "Primary (4-group)"),
  rb$ols_std |> filter(term %in% terms4) |>
    transmute(term, beta = std_beta, spec = "Robustness (5-group)")
) |>
  mutate(label = factor(lab[term], levels = rev(lab[terms4])),
         spec  = factor(spec, c("Primary (4-group)", "Robustness (5-group)")))

cat("\nFigure 4 data (check against Table 7):\n")
print(tidyr::pivot_wider(d4, id_cols = label, names_from = spec, values_from = beta), n = 15)

f4 <- ggplot(d4, aes(beta, label, fill = spec)) +
  geom_vline(xintercept = 0, color = "grey55", linewidth = 0.4) +
  geom_col(position = position_dodge2(width = 0.8, padding = 0.12), width = 0.72) +
  scale_fill_manual(values = c(NAVY, GOLD), name = NULL) +
  labs(x = "Standardized coefficient (positive = more risk seeking)", y = NULL) +
  thm + theme(panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_line(color = GRID, linewidth = 0.3))
save_fig(f4, "Figure4_coefficients_4g_vs_5g", 170, 140)

cat("\nAll figures written to", outd, "\n")
