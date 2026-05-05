# CLAUDE.md

Project memory for Claude Code. Read this at the start of every session.

## Project

Shiny app companion to Chapter 2 of *Blueprints for the Blue* (Brandon's dissertation, FIU Earth System Science). Discrete choice experiment estimating Florida residents' willingness-to-pay for coral reef restoration. Manuscript targets *Ecological Economics*.

Purpose of this app: portfolio piece for postdoc applications. Embedded as iframe on Brandon's Quarto site, deployed via Posit Connect Cloud free tier.

## User preferences

- Code in **R**. Don't suggest Python rewrites.
- **Brief responses.** No throat-clearing, no recapping the prompt.
- **No em dashes** anywhere — comments, docs, commit messages, anywhere.
- **No fabricated data or values.** If something can't be computed from real inputs, stop and ask. Do not fill with plausible numbers.
- No AI-sounding prose ("delve", "navigate", "leverage", "it's worth noting").

## Model context

Headline model is the Apollo **ANA-MNL** in `CoralReef_ANA_model.rds` (in the dissertation `Cowork/output/models/` tree), repackaged into `data/model_m5.rds` by `build_app_data.R`. Each attribute has paired attended/non-attended coefficients; the app uses the **attended class** for headline WTP.

Marginal WTP from this model (computed at runtime, not hardcoded):

| Attribute | Marginal WTP |
|---|---|
| Coral survival | ~$4.63 per percentage point |
| Algae reduction | ~$2.60 per percentage point |
| Fish abundance | ~$0.85 per percentage point |

If the source model is swapped, these numbers will change. The Overview tab reads them live from the model object via `wtp_marginal()`.

ANA = attribute non-attendance. The current `model_m5.rds` carries 2 classes (attended / non-attended) per attribute, not 3. Certainty correction is **not** currently applied; the manuscript's "M5" with certainty correction does not exist as a saved file yet. If/when it does, drop it into `data/` and re-point `build_app_data.R`.

## Repo structure

```
.
├── app.R                      # entry point, theme, navbar
├── CLAUDE.md                  # this file
├── README.md                  # human-facing docs
├── R/
│   ├── utils_wtp.R            # WTP prediction (touches model internals)
│   ├── mod_calculator.R       # scenario sliders
│   ├── mod_heterogeneity.R    # demographic filters
│   ├── mod_map.R              # county choropleth
│   └── mod_diagnostics.R      # model fit, coefficients
├── data/                      # app-ready .rds files (loaded by app.R)
│   ├── model_m5.rds           # repackaged ANA-MNL: {coefficients, vcov, meta}
│   ├── choice_data.rds        # long-format DCE rows + per-row demographics
│   └── fl_counties.rds        # FL county sf with n_resp + pct_restoration
├── data-raw/                  # source xlsx, codebook (gitignored)
├── tests/testthat/            # reproduction tests
├── build_app_data.R           # builds the three data/*.rds files
└── data_cowork.xlsx           # raw survey data (committed; OK to publish)
```

Files in `R/` are auto-sourced by Shiny — `app.R` does not call `source()`. `data/` is what the app reads at runtime; rebuild it by running `Rscript build_app_data.R`.

## Domain conventions

- DCE = discrete choice experiment
- WTP = willingness-to-pay (per-household, one-time payment in this study)
- ANA = attribute non-attendance
- MMNL = mixed multinomial logit
- Attribute units: coral survival (pp, 0-45), macroalgae reduction (pp, 0-90), reef fish abundance (pp increase, 0-300), cost ($/household)
- Sample: 800 FL adult residents, fielded 2024, **3** choice tasks each → **2,400** choice observations (7,200 long-format rows)

## Privacy rule

Respondent geography shows at **county aggregate only**. Never plot individual respondent points or zip-level dots. County cells with n < 5 are suppressed (NA) on the map.

## Workflow expectations

1. **Inspect before editing.** Run `str()` and `class()` on unfamiliar objects and report findings before changing code that depends on them.
2. **Diff plan before major changes.** For anything touching `utils_wtp.R`, the model loading logic, or the heterogeneity module, show the plan first.
3. **Tests are non-negotiable.** Any change to WTP computation runs the testthat suite before being declared done.
4. **Ask, don't fabricate.** Default move when uncertain is to ask Brandon, not to invent.

## Deployment

Target: Posit Connect Cloud free tier, embedded via iframe on Brandon's Quarto site. Don't add shinyapps.io-specific config unless explicitly asked.

Required packages: shiny, bslib, dplyr, ggplot2, leaflet, sf, DT, MASS, tidyr, bsicons, tigris, testthat. If a change adds a dependency, flag it.

## Out of scope

- Don't rewrite the manuscript or generate new analysis text.
- Don't propose switching frameworks (no plotly-everywhere, no Quarto dashboards as replacement).
- Don't add authentication, user accounts, or database backends.
- Don't add tracking, analytics, or telemetry.
