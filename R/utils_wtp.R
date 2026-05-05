# utils_wtp.R --------------------------------------------------------
# WTP helpers. Read the {coefficients, vcov, meta} object built by
# build_app_data.R from CoralReef_ANA_model.rds. All attribute deltas
# are in percentage points.

# Marginal WTP for one attribute, with simulated CIs (Krinsky-Robb).
wtp_marginal <- function(model, attr, cost = "cost",
                         n_draws = 2000, ci = 0.95) {
  beta  <- model$coefficients
  V     <- model$vcov
  draws <- MASS::mvrnorm(n_draws, mu = beta, Sigma = V)
  wtp_d <- -draws[, attr] / draws[, cost]
  q <- c((1 - ci) / 2, 0.5, 1 - (1 - ci) / 2)
  out <- quantile(wtp_d, q, na.rm = TRUE)
  list(lwr = unname(out[1]), med = unname(out[2]), upr = unname(out[3]),
       draws = wtp_d)
}

# WTP for a scenario (named list of attribute deltas, all in pp)
wtp_scenario <- function(model, scenario, n_draws = 2000, ci = 0.95) {
  beta  <- model$coefficients
  V     <- model$vcov
  draws <- MASS::mvrnorm(n_draws, mu = beta, Sigma = V)
  num   <- 0
  for (a in names(scenario)) num <- num + scenario[[a]] * draws[, a]
  wtp_d <- -num / draws[, "cost"]
  q <- c((1 - ci) / 2, 0.5, 1 - (1 - ci) / 2)
  out <- quantile(wtp_d, q, na.rm = TRUE)
  list(lwr = unname(out[1]), med = unname(out[2]), upr = unname(out[3]),
       draws = wtp_d)
}

# Refit a simple MNL on a respondent subset and return marginal WTPs.
# Returns NULL if subset too small or mlogit unavailable.
fit_subgroup_mnl <- function(choice_data, n_draws = 1000) {
  if (!requireNamespace("mlogit", quietly = TRUE)) return(NULL)
  if (length(unique(choice_data$respondent_id)) < 10) return(NULL)

  d <- choice_data
  d$chid <- paste(d$respondent_id, d$round, sep = "_")
  d$alt  <- as.character(d$alt)

  ml_data <- tryCatch(
    mlogit::mlogit.data(
      d, choice = "chosen", shape = "long",
      alt.var = "alt", chid.var = "chid"
    ),
    error = function(e) NULL
  )
  if (is.null(ml_data)) return(NULL)

  fit <- tryCatch(
    mlogit::mlogit(
      chosen ~ algae_reduction + coral_survival + fish_abundance + price | 0,
      data = ml_data
    ),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)

  beta <- stats::coef(fit)
  V    <- stats::vcov(fit)
  draws <- MASS::mvrnorm(n_draws, mu = beta, Sigma = V)

  attrs <- c("coral_survival", "algae_reduction", "fish_abundance")
  out <- lapply(attrs, function(a) {
    w <- -draws[, a] / draws[, "price"]
    c(lwr = unname(quantile(w, 0.025, na.rm = TRUE)),
      med = unname(quantile(w, 0.5,   na.rm = TRUE)),
      upr = unname(quantile(w, 0.975, na.rm = TRUE)))
  })
  res <- do.call(rbind, out)
  rownames(res) <- attrs
  list(
    n_resp = length(unique(choice_data$respondent_id)),
    n_obs  = nrow(choice_data) / 3,
    wtp    = res
  )
}
