# ============================================================
# 01 - Data Preparation
# Load IFLS5 .dta files, merge, construct risk aversion index
# ============================================================

library(haven)
library(tidyverse)

# --- Load raw data ---
# Taruh file .dta IFLS5 di data/raw/
# si <- read_dta("data/raw/b3a_si.dta")   # Risk preference module
# bf <- read_dta("data/raw/b3a_bf.dta")   # Big Five personality
# cog <- read_dta("data/raw/b3a_cog.dta") # Cognitive measures
# demo <- read_dta("data/raw/bk_ar1.dta") # Demographics

# --- TODO ---
# 1. Construct risk aversion index from SI module
# 2. Compute Big Five trait scores (reverse-code)
# 3. Merge all on PIDLINK
# 4. Handle missing values
# 5. Save to data/processed/