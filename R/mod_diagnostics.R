# mod_diagnostics.R --------------------------------------------------
# Tab 5: technical view. All values read from the real model object;
# attendance rates computed from respondent importance ratings.

diagnostics_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header("Model coefficients (ANA-MNL)"),
      DTOutput(ns("coef_tbl"))
    ),
    card(
      card_header("Implied marginal WTP"),
      DTOutput(ns("wtp_tbl"))
    ),
    card(
      card_header("Fit statistics"),
      tableOutput(ns("fit_tbl"))
    ),
    card(
      card_header("Notes"),
      card_body(markdown(paste(
        "**Model.** Apollo ANA multinomial logit (`CoralReef_ANA_model.rds`).",
        "Each attribute has two coefficients: one for respondents who",
        "report attending to that attribute and one for those who don't.",
        "Headline WTP uses the attended-class coefficients.",
        "",
        "**Sample.** 800 Florida residents, 3 choice tasks each",
        "(2,400 choice observations, 7,200 long-format rows).",
        "",
        "**Caveats.** Sample is FL residents only; transfer to other regions",
        "requires benefit transfer adjustment.",
        sep = "\n"
      )))
    )
  )
}

diagnostics_server <- function(id, model, choice_dat) {
  moduleServer(id, function(input, output, session) {

    output$coef_tbl <- renderDT({
      beta <- model$coefficients
      se   <- sqrt(diag(model$vcov))
      df <- data.frame(
        Parameter = names(beta),
        Estimate  = round(unname(beta), 4),
        SE        = round(unname(se),   4)
      )
      df$z <- round(df$Estimate / df$SE, 2)
      df$p <- round(2 * pnorm(-abs(df$z)), 3)
      datatable(df, rownames = FALSE, options = list(pageLength = 10, dom = "tip"))
    })

    output$wtp_tbl <- renderDT({
      attrs  <- c("coral_survival","algae_reduction","fish_abundance")
      labels <- c("Coral survival","Algae reduction","Fish abundance")
      rows <- lapply(attrs, function(a) {
        w <- wtp_marginal(model, a)
        data.frame(
          Attribute = NA, Median = w$med, Lower = w$lwr, Upper = w$upr,
          stringsAsFactors = FALSE
        )
      })
      df <- do.call(rbind, rows)
      df$Attribute <- labels
      df$Median <- round(df$Median, 2)
      df$Lower  <- round(df$Lower,  2)
      df$Upper  <- round(df$Upper,  2)
      colnames(df) <- c("Attribute","Median ($/pp)","2.5% ($/pp)","97.5% ($/pp)")
      datatable(df, rownames = FALSE, options = list(dom = "t"))
    })

    output$fit_tbl <- renderTable({
      meta <- model$meta
      data.frame(
        Statistic = c("Log-likelihood","N parameters","N observations",
                      "Source model file"),
        Value     = c(formatC(meta$LL, format = "f", digits = 1),
                      as.character(meta$nParams),
                      formatC(meta$nObs, format = "d", big.mark = ","),
                      meta$source_file)
      )
    }, striped = TRUE, hover = TRUE)
  })
}
