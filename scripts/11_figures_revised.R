## 11_figures_revised.R ------------------------------------------------------
## Publication figures for the restructured paper. 300-dpi PNG + vector PDF.
## Run from the project root, after 07-10.
## ---------------------------------------------------------------------------

suppressMessages({ library(tidyverse); library(patchwork) })

S  <- readRDS("data/processed/comprehension_sample.rds")
CM <- readRDS("output/tables/core_models.rds")
CN <- readRDS("output/tables/cv_and_noise.rds")
NS <- readRDS("output/tables/noise_test_and_supp.rds")

d5 <- S$cc
dir.create("output/figures/submission", showWarnings = FALSE, recursive = TRUE)

th <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 8.5, colour = "grey30"),
        legend.position = "top")

save2 <- function(p, name, w, h) {
  ggsave(sprintf("output/figures/submission/%s.png", name), p,
         width = w, height = h, dpi = 300)
  ggsave(sprintf("output/figures/submission/%s.pdf", name), p,
         width = w, height = h, device = cairo_pdf)
  cat("wrote", name, "\n")
}

NAVY <- "#1f3b63"; GOLD <- "#c08a2e"; GREY <- "#8c8c8c"

## ---------------------------------------------------------------------------
## Figure 1 — comprehension gradient: cognition falls monotonically
## ---------------------------------------------------------------------------
grad <- d5 |>
  select(comprehension, raven_score, wscore, serial7_score, co07count, educ_years) |>
  pivot_longer(-comprehension) |>
  group_by(comprehension, name) |>
  summarise(m = mean(value), se = sd(value) / sqrt(n()), .groups = "drop") |>
  group_by(name) |>
  mutate(z = (m - m[comprehension == "correct"]) /
             sd(d5[[first(name)]], na.rm = TRUE),
         z_se = se / sd(d5[[first(name)]], na.rm = TRUE)) |>
  ungroup() |>
  mutate(name = recode(name,
           raven_score = "Raven's matrices", wscore = "Number series (W)",
           serial7_score = "Serial 7", co07count = "Word recall",
           educ_years = "Years of schooling"),
         comprehension = recode(comprehension,
           correct = "Correct\nfirst time", corrected = "Corrected\non challenge",
           confirmed_dominated = "Confirmed\ndominated choice"))

f1 <- ggplot(grad, aes(comprehension, z, group = name, colour = name)) +
  geom_hline(yintercept = 0, colour = GREY, linewidth = .3) +
  geom_line(linewidth = .8) +
  geom_pointrange(aes(ymin = z - 1.96 * z_se, ymax = z + 1.96 * z_se), size = .35) +
  scale_colour_manual(values = c(NAVY, GOLD, "#4c7a3f", "#8c3b3b", "#5b4b8a")) +
  labs(title = "Ability declines monotonically across the comprehension gradient",
       subtitle = str_wrap(paste("Set 1 dominance screen. Differences in SD units relative to respondents who chose correctly the first time;",
                                 "bars are 95% CIs."), 105),
       x = NULL, y = "Difference from 'correct first time' (SD)", colour = NULL) +
  th
save2(f1, "Figure1_comprehension_gradient", 7.2, 4.4)

## ---------------------------------------------------------------------------
## Figure 2 — two-channel decomposition
## ---------------------------------------------------------------------------
lab <- c(raven_score = "Raven's matrices", wscore = "Number series (W)",
         serial7_score = "Serial 7", co07count = "Word recall (imm.)",
         co10count = "Word recall (del.)", memory_self = "Self-rated memory",
         educ_years = "Years of schooling", female = "Female",
         rural = "Rural", employed = "Employed",
         age_c = "Age", age_c2 = "Age squared")

side <- CM$side |>
  filter(term %in% names(lab)) |>
  mutate(label = lab[term]) |>
  select(label, Comprehension = comprehension_channel,
         Preference = preference_channel) |>
  pivot_longer(-label, names_to = "channel", values_to = "beta")

ord <- side |> filter(channel == "Comprehension") |> arrange(beta) |> pull(label)

f2 <- ggplot(side, aes(beta, factor(label, levels = ord), fill = channel)) +
  geom_vline(xintercept = 0, colour = GREY, linewidth = .3) +
  geom_col(position = position_dodge(width = .7), width = .65) +
  scale_fill_manual(values = c(Comprehension = NAVY, Preference = GOLD)) +
  labs(title = "Cognitive variables act on comprehension; gender acts on preference",
       subtitle = str_wrap(paste("Log-odds per SD, household-clustered SEs. Comprehension channel = P(confirming a dominated choice), N = 25,040;",
                                 "negative means less likely to confirm. Preference channel = ordered logit on groups 1-4, N = 17,886;",
                                 "negative means more risk averse."), 108),
       x = "Change in log-odds per SD of predictor", y = NULL, fill = NULL) +
  th
save2(f2, "Figure2_two_channel_decomposition", 7.2, 4.8)

## ---------------------------------------------------------------------------
## Figure 3 — cut-point-specific cognitive effects (why proportional odds fails)
## ---------------------------------------------------------------------------
cut <- CM$cutfit |>
  filter(term %in% c("raven_score", "wscore", "serial7_score", "female")) |>
  mutate(label = lab[term],
         ## put predictors on a common scale: per SD
         sd_x = case_when(term == "raven_score" ~ sd(d5$raven_score),
                          term == "wscore" ~ sd(d5$wscore),
                          term == "serial7_score" ~ sd(d5$serial7_score),
                          term == "female" ~ sd(d5$female)),
         b = est * sd_x, lo = (est - 1.96 * se) * sd_x, hi = (est + 1.96 * se) * sd_x,
         cut = recode(cut, `Y>1` = "Y > 1\n(vs most averse)",
                           `Y>2` = "Y > 2", `Y>3` = "Y > 3\n(vs most seeking)"))

f3 <- ggplot(cut, aes(cut, b, group = label, colour = label)) +
  geom_hline(yintercept = 0, colour = GREY, linewidth = .3) +
  geom_line(linewidth = .8) +
  geom_pointrange(aes(ymin = lo, ymax = hi), size = .35) +
  facet_wrap(~ label, nrow = 1, scales = "free_y") +
  scale_colour_manual(values = c(NAVY, GOLD, "#4c7a3f", "#8c3b3b"), guide = "none") +
  labs(title = "Cognitive effects appear only at the top cut point",
       subtitle = str_wrap(paste("Separate binary logits at each cumulative cut of the 4-group index, household-clustered SEs.",
                                 "A single ordered-logit coefficient averages these effects together."), 120),
       x = NULL, y = "Log-odds per SD") +
  th + theme(axis.text.x = element_text(size = 7.5))
save2(f3, "Figure3_cutpoint_effects", 8.4, 3.8)

## ---------------------------------------------------------------------------
## Figure 4 — noise convergence
## ---------------------------------------------------------------------------
noise_dist <- function(q) {
  c((1 - q)^2, (1 - (1 - q)^2) * (1 - q)^2, (1 - (1 - q)^2) * (1 - q) * q,
    (1 - (1 - q)^2) * q * (1 - q), (1 - (1 - q)^2) * q^2)
}

pa <- NS$quintiles |>
  select(cog_q, G0, G1, G2, G3, G4) |>
  pivot_longer(-cog_q, names_to = "grp", values_to = "p") |>
  mutate(cog_q = factor(cog_q, labels = paste("Q", 1:5)))

ref <- tibble(grp = paste0("G", 0:4), p = noise_dist(0.5))

f4a <- ggplot(pa, aes(grp, p, group = cog_q, colour = cog_q)) +
  geom_col(data = ref, aes(grp, p), inherit.aes = FALSE,
           fill = GREY, alpha = .25, width = .8) +
  geom_line(linewidth = .7) + geom_point(size = 1.4) +
  scale_colour_manual(values = c("#1f3b63", "#3f7fa6", "#7aa050", "#d18a2b", "#a32b2b")) +
  scale_y_continuous(labels = scales::percent_format(1)) +
  labs(title = "Low-ability respondents converge on the coin-flip distribution",
       subtitle = str_wrap(paste("Grey bars: distribution implied by answering every Set 1 node at random (q = .5).",
                                 "Lines: observed, by cognitive quintile."), 100),
       x = "Risk index category (G0 = confirmed dominated choice)",
       y = "Share of quintile", colour = "Cognitive\nquintile") +
  th

f4b <- ggplot(NS$quintiles, aes(cog_q, dist_coinflip)) +
  geom_line(colour = NAVY, linewidth = .8) +
  geom_point(colour = NAVY, size = 2.2) +
  scale_x_continuous(breaks = 1:5) +
  labs(title = "Distance to the coin-flip prediction",
       subtitle = str_wrap(paste("Downward but not strictly monotone; with five points the rank",
                                 "correlation is descriptive only (rho = -0.90, p = .083)."), 46),
       x = "Cognitive quintile", y = "Chi-square distance") +
  th

f4 <- f4a + f4b + plot_layout(widths = c(2, 1))
save2(f4, "Figure4_noise_convergence", 10, 4.2)

cat("\nAll figures written to output/figures/submission/\n")
