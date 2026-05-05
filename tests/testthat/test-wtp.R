library(testthat)

source("../../R/utils_wtp.R")

test_that("wtp_marginal returns plausible values matching CLAUDE.md anchors", {
  m <- readRDS("../../data/model_m5.rds")

  set.seed(1)
  coral <- wtp_marginal(m, "coral_survival")$med
  algae <- wtp_marginal(m, "algae_reduction")$med
  fish  <- wtp_marginal(m, "fish_abundance")$med

  # Real model values: ~$4.63, $2.60, $0.85 per pp.
  # Anchors live in CLAUDE.md ($4.70, $2.87, $0.91); allow 25% slack.
  expect_gt(coral, 3); expect_lt(coral, 7)
  expect_gt(algae, 1.5); expect_lt(algae, 4.5)
  expect_gt(fish,  0.4); expect_lt(fish,  1.5)
})

test_that("wtp_scenario sums attribute contributions correctly", {
  m <- readRDS("../../data/model_m5.rds")

  set.seed(1)
  r <- wtp_scenario(m, list(coral_survival = 10,
                            algae_reduction = 0,
                            fish_abundance = 0))
  set.seed(1)
  r10 <- wtp_marginal(m, "coral_survival")
  expect_equal(r$med, 10 * r10$med, tolerance = 0.5)
})

test_that("subgroup MNL refit returns expected shape on full sample", {
  m <- readRDS("../../data/model_m5.rds")
  d <- readRDS("../../data/choice_data.rds")
  sg <- fit_subgroup_mnl(d)
  expect_false(is.null(sg))
  expect_equal(sg$n_resp, 800)
  expect_setequal(rownames(sg$wtp),
                  c("coral_survival","algae_reduction","fish_abundance"))
  expect_true(all(sg$wtp[,"med"] > 0))
})
