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

Supporting scripts:

- `scripts/alpha_check_18-65.R` — Cronbach's alpha for the BFI-15 scales reported in the paper
- `scripts/app.R` — Shiny demo that predicts risk-preference group probabilities

## Outputs

- `output/figures/submission/` — publication figures (300-dpi PNG + vector PDF), committed
- `output/tables/` — fitted model objects and result tables (**not committed**: R model objects embed individual-level data)

## Manuscript

The current draft lives in `docs/Risk_Preference_Draft.docx`.
