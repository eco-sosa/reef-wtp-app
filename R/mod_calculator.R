# mod_calculator.R ---------------------------------------------------
# Tab 2: scenario builder. All sliders are in percentage points to
# match the units the model was fit on (data ranges shown in labels).

calculator_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      title = "Scenario inputs",
      sliderInput(ns("coral"),  "Coral survival increase (pp, data range 0-45)",
                  min = 0, max = 45,  value = 10, step = 1),
      sliderInput(ns("algae"),  "Macroalgae reduction (pp, data range 0-90)",
                  min = 0, max = 90,  value = 20, step = 1),
      sliderInput(ns("fish"),   "Reef fish abundance (pp increase, data range 0-300)",
                  min = 0, max = 300, value = 50, step = 5),
      hr(),
      helpText("Estimates are per-household, one-time payment. ",
               "Marginal WTP read from the ANA-MNL (attended class).")
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Estimated household WTP"),
        card_body(
          uiOutput(ns("wtp_headline")),
          plotOutput(ns("wtp_contrib"), height = "240px")
        )
      ),
      card(
        card_header("Distribution of simulated draws"),
        plotOutput(ns("wtp_dist"), height = "240px")
      )
    )
  )
}

calculator_server <- function(id, model) {
  moduleServer(id, function(input, output, session) {

    scenario_wtp <- reactive({
      sc <- list(
        coral_survival  = input$coral,
        algae_reduction = input$algae,
        fish_abundance  = input$fish
      )
      wtp_scenario(model, sc)
    })

    output$wtp_headline <- renderUI({
      r <- scenario_wtp()
      tags$div(
        style = "font-family: 'Fraunces', serif; padding: 1rem 0;",
        tags$div(style = "font-size: 2.6rem; color: #0E5C6B; font-weight: 500;",
                 sprintf("$%.2f", r$med)),
        tags$div(style = "color: #555; font-size: 0.95rem;",
                 sprintf("95%% CI: $%.2f - $%.2f", r$lwr, r$upr))
      )
    })

    output$wtp_contrib <- renderPlot({
      contrib <- data.frame(
        attribute = c("Coral survival", "Algae reduction", "Fish abundance"),
        wtp = c(
          wtp_marginal(model, "coral_survival")$med  * input$coral,
          wtp_marginal(model, "algae_reduction")$med * input$algae,
          wtp_marginal(model, "fish_abundance")$med  * input$fish
        )
      )
      ggplot(contrib, aes(reorder(attribute, wtp), wtp)) +
        geom_col(fill = "#0E5C6B", width = 0.6) +
        geom_text(aes(label = sprintf("$%.2f", wtp)),
                  hjust = -0.15, size = 4, color = "#1A2E35") +
        coord_flip() +
        labs(x = NULL, y = "Contribution to WTP ($)",
             title = "Per-attribute contribution") +
        theme_minimal(base_size = 12) +
        theme(panel.grid.major.y = element_blank(),
              plot.title = element_text(family = "serif"))
    })

    output$wtp_dist <- renderPlot({
      r <- scenario_wtp()
      df <- data.frame(wtp = r$draws)
      ggplot(df, aes(wtp)) +
        geom_histogram(bins = 40, fill = "#C76A4A", color = "white", alpha = 0.9) +
        geom_vline(xintercept = r$med, linetype = "dashed",
                   color = "#0E5C6B", linewidth = 0.8) +
        labs(x = "Simulated WTP ($)", y = "Frequency") +
        theme_minimal(base_size = 12)
    })
  })
}
