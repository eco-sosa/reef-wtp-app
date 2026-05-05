# mod_heterogeneity.R ------------------------------------------------
# Tab 3: subgroup WTP. Re-fits a simple MNL on the filtered respondent
# subset using mlogit, then reports marginal WTP with 95% CIs.
# Refit takes ~1-3 seconds on the full sample; smaller subsets faster.

heterogeneity_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      title = "Filter respondents",
      selectInput(ns("income"), "Household income",
                  choices = c("All","<$50k","$50-100k","$100-150k",">$150k"),
                  selected = "All"),
      selectInput(ns("age"), "Age group",
                  choices = c("All","18-34","35-54","55+"),
                  selected = "All"),
      selectInput(ns("coastal"), "Coastal county resident",
                  choices = c("All","Yes","No"), selected = "All"),
      selectInput(ns("visit"), "Visited FKNMS",
                  choices = c("All","Yes","No"), selected = "All"),
      selectInput(ns("political"), "Political ideology",
                  choices = c("All","Liberal","Moderate Leaning Liberal",
                              "Moderate","Moderate Leaning Conservative",
                              "Conservative"),
                  selected = "All"),
      selectInput(ns("nep"), "NEP score (environmental concern)",
                  choices = c("All","Low NEP","Mid NEP","High NEP"),
                  selected = "All"),
      hr(),
      textOutput(ns("n_filt")),
      helpText("Subgroup model is a simple MNL (no ANA, no random",
               "coefficients) refit on the filtered respondents.",
               "Full-sample reference uses the headline ANA-MNL.")
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Subgroup WTP estimates"),
        plotOutput(ns("subgroup_plot"), height = "380px")
      ),
      card(
        card_header("Comparison to full sample"),
        DTOutput(ns("compare_tbl"))
      )
    )
  )
}

heterogeneity_server <- function(id, model, choice_dat) {
  moduleServer(id, function(input, output, session) {

    full_wtp <- list(
      coral_survival  = wtp_marginal(model, "coral_survival"),
      algae_reduction = wtp_marginal(model, "algae_reduction"),
      fish_abundance  = wtp_marginal(model, "fish_abundance")
    )

    filtered <- reactive({
      d <- choice_dat
      if (input$income    != "All") d <- d[!is.na(d$income)       & d$income       == input$income,    ]
      if (input$age       != "All") d <- d[!is.na(d$age)          & d$age          == input$age,       ]
      if (input$coastal   != "All") d <- d[!is.na(d$coastal)      & d$coastal      == input$coastal,   ]
      if (input$visit     != "All") d <- d[!is.na(d$visit)        & d$visit        == input$visit,     ]
      if (input$political != "All") d <- d[!is.na(d$political_id) & as.character(d$political_id) == input$political, ]
      if (input$nep       != "All") d <- d[!is.na(d$nep)          & d$nep          == input$nep,       ]
      d
    })

    subgroup_fit <- reactive({
      d <- filtered()
      fit_subgroup_mnl(d)
    })

    output$n_filt <- renderText({
      n <- length(unique(filtered()$respondent_id))
      sprintf("Respondents in subgroup: %d", n)
    })

    output$subgroup_plot <- renderPlot({
      sg <- subgroup_fit()

      full_df <- data.frame(
        attribute = c("Coral outplant survival","Macroalgae reduction","Fish abundance"),
        med  = c(full_wtp$coral_survival$med,  full_wtp$algae_reduction$med,
                 full_wtp$fish_abundance$med),
        lwr  = c(full_wtp$coral_survival$lwr,  full_wtp$algae_reduction$lwr,
                 full_wtp$fish_abundance$lwr),
        upr  = c(full_wtp$coral_survival$upr,  full_wtp$algae_reduction$upr,
                 full_wtp$fish_abundance$upr),
        group = "Full sample"
      )

      if (is.null(sg)) {
        df <- full_df
        sub <- ggplot()
      } else {
        sub_df <- data.frame(
          attribute = c("Coral outplant survival","Macroalgae reduction","Fish abundance"),
          med = c(sg$wtp["coral_survival","med"],
                  sg$wtp["algae_reduction","med"],
                  sg$wtp["fish_abundance","med"]),
          lwr = c(sg$wtp["coral_survival","lwr"],
                  sg$wtp["algae_reduction","lwr"],
                  sg$wtp["fish_abundance","lwr"]),
          upr = c(sg$wtp["coral_survival","upr"],
                  sg$wtp["algae_reduction","upr"],
                  sg$wtp["fish_abundance","upr"]),
          group = "Selected subgroup"
        )
        df <- rbind(full_df, sub_df)
      }

      ggplot(df, aes(attribute, med, fill = group)) +
        geom_col(position = position_dodge(width = 0.7), width = 0.6) +
        geom_errorbar(aes(ymin = lwr, ymax = upr),
                      position = position_dodge(width = 0.7),
                      width = 0.18, color = "#1A2E35") +
        scale_fill_manual(values = c("Full sample" = "#9AA8AB",
                                     "Selected subgroup" = "#0E5C6B")) +
        labs(x = NULL, y = "Marginal WTP ($/pp)", fill = NULL,
             caption = if (is.null(subgroup_fit()))
               "Subgroup too small to refit (need >= 10 respondents)" else NULL) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "top")
    })

    output$compare_tbl <- renderDT({
      sg <- subgroup_fit()
      attrs <- c("coral_survival","algae_reduction","fish_abundance")
      labels <- c("Coral outplant survival","Macroalgae reduction","Fish abundance")
      full_med <- vapply(attrs, function(a) full_wtp[[a]]$med, numeric(1))

      if (is.null(sg)) {
        df <- data.frame(
          Attribute            = labels,
          `Full sample ($/pp)` = round(full_med, 2),
          `Subgroup ($/pp)`    = NA_real_,
          check.names = FALSE
        )
      } else {
        sub_med <- sg$wtp[attrs, "med"]
        df <- data.frame(
          Attribute            = labels,
          `Full sample ($/pp)` = round(full_med, 2),
          `Subgroup ($/pp)`    = round(sub_med, 2),
          `% diff`             = round(100 * (sub_med - full_med) / full_med, 1),
          check.names = FALSE
        )
      }
      datatable(df, rownames = FALSE, options = list(dom = "t"))
    })
  })
}
