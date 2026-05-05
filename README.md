# Reef Restoration WTP Explorer

Interactive companion to Chapter 2 of *Blueprints for the Blue*: a discrete choice experiment estimating Florida residents' willingness-to-pay for coral reef restoration outcomes.

Live numbers come from a real Apollo ANA-MNL fit on 800 FL respondents (2,400 choice observations). Read the model from `data/model_m5.rds`; rebuild it with `Rscript build_app_data.R`.

## Run locally

```r
install.packages(c("shiny","bslib","dplyr","ggplot2","leaflet",
                   "sf","DT","MASS","tidyr","bsicons","tigris",
                   "readxl","mlogit","testthat"))

# Build the .rds files the app loads
Rscript build_app_data.R   # or: source("build_app_data.R") in R

shiny::runApp(".")
```

## App tabs

- **Overview.** Headline WTP, read live from the model object.
- **WTP Calculator.** Sliders for each attribute (in percentage points). Krinsky-Robb 95% CI.
- **Heterogeneity.** Filter by income / age / coastal / FKNMS-visit. Refits a simple MNL on the subset (~0.1 s) and compares to the full sample.
- **Geography.** County choropleth (n respondents, % choosing restoration). Counties with n<5 suppressed per the privacy rule.
- **Model Diagnostics.** Coefficient table with z/p, fit stats from the real model object.

## Files

```
.
├── app.R                  # entry point
├── R/                     # auto-sourced by Shiny
│   ├── utils_wtp.R        # wtp_marginal / wtp_scenario / fit_subgroup_mnl
│   ├── mod_calculator.R
│   ├── mod_heterogeneity.R
│   ├── mod_map.R
│   └── mod_diagnostics.R
├── data/                  # .rds files the app loads (committed)
│   ├── model_m5.rds
│   ├── choice_data.rds
│   └── fl_counties.rds
├── tests/testthat/        # WTP plumbing tests
├── data_cowork.xlsx       # raw survey data
├── build_app_data.R       # rebuilds data/*.rds from sources
├── README.md
└── CLAUDE.md
```

## Swap in a different model

`build_app_data.R` reads `CoralReef_ANA_model.rds` from the dissertation `Cowork/output/models/` tree. Point `SOURCE_MODEL` at a different Apollo `.rds` to swap, then re-run. The renaming map (`b_coral_att` → `coral_survival`, etc.) is at the top of the file.

## Deploy to Posit Connect Cloud

1. Push this repo to GitHub.
2. Sign in at <https://connect.posit.cloud>, **Publish > Shiny**, point at the repo.
3. Connect Cloud builds and gives you a URL.
4. Embed via iframe in your Quarto site:

```markdown
::: {.column-page}
<iframe src="https://yourname.shinyapps.connect.posit.cloud/reef-wtp/"
        width="100%" height="900" style="border:none;">
</iframe>
:::
```

## Privacy

Respondent geography is shown at county aggregate only. County cells with fewer than 5 respondents are suppressed in the map.
