# build_app_data.R --------------------------------------------------
# Build the three .rds files the Shiny app loads, from real sources:
#   1. data/model_m5.rds      (repackaged from CoralReef_ANA_model.rds)
#   2. data/choice_data.rds   (respondent-level, from data_cowork.xlsx)
#   3. data/fl_counties.rds   (FL county sf + per-county aggregates)
#
# Run once. Re-run only if the source model or xlsx changes.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(sf)
})

set.seed(42)

`%||%` <- function(a, b) if (is.null(a)) b else a

# Sources --------------------------------------------------------------
COWORK_MODEL_DIR <- "B:/OneDrive - Florida International University/Research Work/Dissertation/Social Survey/Data/Cowork/output/models"
SOURCE_MODEL <- file.path(COWORK_MODEL_DIR, "CoralReef_ANA_model.rds")
SOURCE_XLSX  <- "data_cowork.xlsx"

stopifnot(file.exists(SOURCE_MODEL), file.exists(SOURCE_XLSX))
dir.create("data", showWarnings = FALSE)

# 1. Model -------------------------------------------------------------
# Apollo ANA model: estimate vector with b_*_att and b_*_nonatt pairs.
# Repackage to the {coefficients, vcov, meta} shape utils_wtp.R reads.

# Read xlsx once; we use it for attendance shares, choice_data, and demographics
raw <- suppressWarnings(read_excel(SOURCE_XLSX))

political_levels <- c(
  "Liberal","Moderate Leaning Liberal","Moderate",
  "Moderate Leaning Conservative","Conservative"
)

# Attendance shares from importance ratings (strict definition)
attended <- function(x) {
  ifelse(x %in% c("Very important", "Extremely important"), 1, 0)
}
resp_one <- raw %>%
  group_by(respondent_id) %>% slice(1) %>% ungroup() %>%
  transmute(
    coral_survival  = attended(coral_survival_importances_dce),
    algae_reduction = attended(algae_importance_dce),
    fish_abundance  = attended(fish_importance_dce),
    cost            = attended(cost_importance_dce)
  )
attendance_shares <- vapply(resp_one, mean, numeric(1), na.rm = TRUE)

apollo_obj <- readRDS(SOURCE_MODEL)
est <- apollo_obj$estimate
vc  <- apollo_obj$varcov

rename_map <- c(
  asc_alt2          = "asc_alt2",
  b_algae_att       = "algae_reduction",
  b_coral_att       = "coral_survival",
  b_fish_att        = "fish_abundance",
  b_cost_att        = "cost",
  b_algae_nonatt    = "algae_reduction_nonatt",
  b_coral_nonatt    = "coral_survival_nonatt",
  b_fish_nonatt     = "fish_abundance_nonatt",
  b_cost_nonatt     = "cost_nonatt"
)

keep <- intersect(names(est), names(rename_map))
new_names <- unname(rename_map[keep])

beta <- setNames(est[keep], new_names)
V    <- vc[keep, keep, drop = FALSE]
dimnames(V) <- list(new_names, new_names)

model_m5 <- list(
  coefficients = beta,
  vcov         = V,
  meta = list(
    source_file   = basename(SOURCE_MODEL),
    model_label   = "ANA-MNL (attended vs non-attended)",
    LL            = apollo_obj$maximum,
    nObs          = apollo_obj$nObs %||% 2400,
    nParams       = length(est) - sum(apollo_obj$fixed),
    fit_method    = "BFGS (Apollo, robust SE)",
    headline_attrs = c("coral_survival", "algae_reduction", "fish_abundance"),
    cost_name      = "cost",
    attr_units     = c(
      coral_survival  = "percentage points",
      algae_reduction = "percentage points",
      fish_abundance  = "percentage points"
    ),
    attendance_shares = attendance_shares
  )
)

saveRDS(model_m5, "data/model_m5.rds")
cat("[1/3] data/model_m5.rds written.\n")
cat("  Attendance shares (strict):\n")
for (a in names(attendance_shares))
  cat(sprintf("    %-18s %.1f%%\n", a, 100 * attendance_shares[[a]]))
cat("  Marginal WTP (attended class):\n")
cat(sprintf("    coral:  $%.2f / pp\n",  -beta["coral_survival"]  / beta["cost"]))
cat(sprintf("    algae:  $%.2f / pp\n",  -beta["algae_reduction"] / beta["cost"]))
cat(sprintf("    fish:   $%.2f / pp\n",  -beta["fish_abundance"]  / beta["cost"]))
cat("  Weighted WTP (attendance share x attended WTP):\n")
cat(sprintf("    coral:  $%.2f / pp\n",
            attendance_shares[["coral_survival"]]  * -beta["coral_survival"]  / beta["cost"]))
cat(sprintf("    algae:  $%.2f / pp\n",
            attendance_shares[["algae_reduction"]] * -beta["algae_reduction"] / beta["cost"]))
cat(sprintf("    fish:   $%.2f / pp\n",
            attendance_shares[["fish_abundance"]]  * -beta["fish_abundance"]  / beta["cost"]))

# 2. Choice data -------------------------------------------------------
# Long-format DCE rows + per-row demographic columns the heterogeneity
# tab needs to filter on. Real respondent IDs are kept (string),
# zip/county aggregation handled in the map step.

raw <- suppressWarnings(read_excel(SOURCE_XLSX))

income_bucket <- function(x) {
  case_when(
    x %in% c("Less than $20,000", "$20,000 to $39,999",
             "$40,000 to $59,999")                              ~ "<$50k",
    x %in% c("$60,000 to $79,999", "$80,000 to $99,999")        ~ "$50-100k",
    x %in% c("$100,000 to $119,999", "$120,000 to $139,999",
             "$140,000 to $159,999")                            ~ "$100-150k",
    !is.na(x)                                                   ~ ">$150k",
    TRUE                                                        ~ NA_character_
  )
}

age_bucket <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  case_when(
    x >= 18 & x <= 34 ~ "18-34",
    x >= 35 & x <= 54 ~ "35-54",
    x >= 55           ~ "55+",
    TRUE              ~ NA_character_
  )
}

coastal_fl <- c(
  "Miami-Dade","Broward","Palm Beach","Martin","St. Lucie","Indian River",
  "Brevard","Volusia","Flagler","St. Johns","Duval","Nassau",
  "Monroe","Collier","Lee","Charlotte","Sarasota","Manatee","Hillsborough",
  "Pinellas","Pasco","Hernando","Citrus","Levy","Dixie","Taylor",
  "Jefferson","Wakulla","Franklin","Gulf","Bay","Walton","Okaloosa",
  "Santa Rosa","Escambia"
)

# Data-driven terciles since NEP in this dataset is a sum of centered
# Likert items (range observed: -11 to 12), not a 1-5 mean.
nep_numeric_all <- suppressWarnings(as.numeric(raw$nep_score))
nep_q <- quantile(nep_numeric_all, probs = c(1/3, 2/3), na.rm = TRUE)
nep_bucket <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  case_when(
    is.na(x)      ~ NA_character_,
    x <= nep_q[1] ~ "Low NEP",
    x <= nep_q[2] ~ "Mid NEP",
    TRUE          ~ "High NEP"
  )
}

choice_data <- raw %>%
  transmute(
    respondent_id = as.character(respondent_id),
    round         = as.integer(round),
    choice_set    = as.integer(choice_set),
    alt           = as.integer(alternative),
    chosen        = as.integer(chosen),
    algae_reduction = as.numeric(algae_reduction),
    coral_survival  = as.numeric(coral_survival),
    fish_abundance  = as.numeric(fish_abundance),
    price           = as.numeric(price),
    opt_out         = as.integer(opt_out),
    income          = income_bucket(income),
    age_numeric     = suppressWarnings(as.numeric(age)),
    age             = age_bucket(age),
    county          = as.character(county),
    coastal         = ifelse(county %in% coastal_fl, "Yes", "No"),
    visit           = ifelse(fknms_visitation == "Never" | is.na(fknms_visitation),
                              "No", "Yes"),
    political_id    = factor(political_id, levels = political_levels),
    nep_score       = suppressWarnings(as.numeric(nep_score)),
    nep             = nep_bucket(nep_score)
  )

saveRDS(choice_data, "data/choice_data.rds")
cat("[2/3] data/choice_data.rds written.",
    "rows=", nrow(choice_data),
    "respondents=", length(unique(choice_data$respondent_id)), "\n")

# 3. Counties sf -------------------------------------------------------
# tigris FL counties, joined with respondent counts and chose-restoration
# share. Cells with n < 5 suppressed (privacy rule in CLAUDE.md).

if (!requireNamespace("tigris", quietly = TRUE)) {
  stop("Install {tigris} to build fl_counties.rds.")
}

fl <- tigris::counties(state = "FL", cb = TRUE, year = 2022, progress_bar = FALSE)
fl <- sf::st_as_sf(fl)

resp_per_county <- choice_data %>%
  distinct(respondent_id, county) %>%
  count(county, name = "n_resp")

# Per-respondent share of choices that picked a restoration plan (1 or 2)
chose_share <- choice_data %>%
  group_by(respondent_id, county) %>%
  summarise(n_tasks = n() / 3,  # 3 alts per task
            n_picked_restoration = sum(chosen == 1 & alt %in% c(1, 2)),
            .groups = "drop") %>%
  mutate(pct_picked = 100 * n_picked_restoration / n_tasks) %>%
  group_by(county) %>%
  summarise(mean_pct_restoration = mean(pct_picked, na.rm = TRUE), .groups = "drop")

county_stats <- resp_per_county %>%
  left_join(chose_share, by = "county")

fl <- fl %>%
  left_join(county_stats, by = c("NAME" = "county")) %>%
  mutate(pct_restoration = mean_pct_restoration) %>%
  select(NAME, n_resp, pct_restoration, geometry)

saveRDS(fl, "data/fl_counties.rds")
cat("[3/3] data/fl_counties.rds written.",
    "counties_with_n>=1=", sum(!is.na(fl$n_resp) & fl$n_resp >= 1),
    "no_respondents=", sum(is.na(fl$n_resp)), "\n")

cat("\nDone. App-ready files in data/.\n")
