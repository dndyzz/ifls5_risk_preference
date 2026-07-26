# IFLS5 Risk Preference

Analysis code for the manuscript **"Does Cognitive Ability Predict Risk Preference or Task Comprehension? Disentangling Two Channels in a Large Indonesian Survey"** (Aulya, Sofyan, Prasetyo, & Aribowo).

The study compares Big Five personality traits, cognitive ability measures, and demographics as predictors of financial risk preferences elicited with an adaptive multiple price list in the fifth wave of the Indonesia Family Life Survey (IFLS5, 2014–2015), and separates task comprehension from preference via a gamble-averse robustness check.

## Data

IFLS5 microdata are **not redistributed in this repository**. They are available free of charge from the RAND Corporation after registration:
<https://www.rand.org/well-being/social-and-behavioral-policy/data/FLS/IFLS/ifls5.html>

After downloading, place these Stata files in `data/raw/`:

```
b3a_si.dta    b3a_dl1.dta   b3a_tk1.dta   b3b_psn.dta   b3b_co1.dta
b3b_cob.dta   ek_ek1.dta    ek_ek2.dta    bk_ar1.dta    bk_sc1.dta
ptrack.dta    htrack.dta
```

## Environment

R ≥ 4.5 with [renv](https://rstudio.github.io/renv/). Restore the package library with:

```r
renv::restore()
```

## Pipeline

Run the R Markdown scripts in `scripts/` in numbered order:

| Script | What it does |
|---|---|
| `01_risk_index.Rmd` | Builds the risk aversion index from module SI (screening, gamble-averse classification) → `data/processed/risk_index.rds` |
| `02_big_five.Rmd` | BFI-15 items, reverse coding, trait composites → `big_five.rds` |
| `03_cognitive.Rmd` | Six cognitive measures (word recall, Serial 7, self-rated memory, number-series W-score, Raven's) → `cognitive.rds` |
| `04_merge_feature.Rmd` | Merges modules with demographics, geography, education, employment → analytic sample |
| `05_Baseline_Model.Rmd` | Primary analysis: five nested ordered-logit specifications, OLS with fully standardized betas (b × SDx/SDy), average marginal effects → `output/tables/baseline_results_18_65.rds` |
| `06_Robustness_Check_5Group.Rmd` | Re-estimates all specifications with gamble-averse respondents included as Group 0 → `output/tables/robustness_5group.rds` |

Scripts `07`–`12` produce the results reported in the current manuscript. They are plain `.R` files and are run from the project root, in order, after `01`–`04` have produced the processed datasets:

| Script | What it does |
|---|---|
| `07_comprehension_sample.R` | Rebuilds the analytic sample retaining **all** Set 1 screening outcomes, so comprehension and preference can be modeled separately. Adds the 3-level comprehension gradient (`correct` / `corrected` / `confirmed_dominated`), implied CRRA bounds, and a corrected education recode for sensitivity → `data/processed/comprehension_sample.rds` |
| `08_core_models.R` | Brant test and cut-point-specific logits for proportional odds; household-clustered SEs; multinomial logit; the two-stage (comprehension → preference) decomposition; ordered model of the comprehension gradient → `output/tables/core_models.rds` |
| `09_cv_and_noise.R` | 10-fold cross-validated model comparison with folds held at household level; analytic and simulated distribution for random responding through the Set 1 branching tree; interval regression on implied CRRA bounds → `output/tables/cv_and_noise.rds` |
| `10_noise_test_and_supp.R` | Convergence of observed responses on the coin-flip prediction by cognitive quintile; extreme-response logit; full Set 2 analysis; BFI-15 psychometrics (mean inter-item r, Spearman-Brown) → `output/tables/noise_test_and_supp.rds` |
| `11_figures_revised.R` | The four manuscript figures, 300-dpi PNG + vector PDF |
| `12_crossset_stability.R` | Discriminating test between the noise and preference accounts: cross-set agreement by cognitive quintile and reproduction of extreme responses → `output/tables/crossset_stability.rds` |
| `13_selection_and_bootstrap.R` | Randomization check and effects of the randomized presentation order (`random_si`) on both channels; worst-case bounds on the preference channel under selection; Heckman two-step reported as a failed specification; 500-replicate paired household bootstrap for the cross-channel comparison → `output/tables/selection_bootstrap.rds` |
| `14_crra_sensitivity.R` | Implied CRRA bounds and the interval regression re-derived under background consumption w = 0 to 5 times the certain amount → `output/tables/crra_sensitivity.rds` |

Supporting scripts:

- `scripts/alpha_check_18-65.R` — Cronbach's alpha for the BFI-15 scales reported in the paper
- `scripts/app.R` — Shiny demo that predicts risk-preference group probabilities

Note that `05`/`06` reproduce the earlier draft's specifications and are retained for comparability; the sample-size reconciliation in `07` (13 respondents who passed screening but could not be scored) explains the small differences against them.

## Outputs

- `output/figures/submission/` — publication figures (300-dpi PNG + vector PDF), committed
- `output/tables/` — fitted model objects and result tables (**not committed**: R model objects embed individual-level data)

## Manuscript

- `docs/Risk_Preference_Draft_v2.docx` — current draft (two-channel decomposition; scripts `07`–`12`)
- `docs/Risk_Preference_Draft.docx` — earlier draft (personality vs. cognition comparison; scripts `01`–`06`)

## License

The code in this repository is released under the [MIT License](LICENSE). The IFLS5 data are subject to RAND's data use agreement and are not covered by this license.
