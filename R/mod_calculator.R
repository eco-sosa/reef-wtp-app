# mod_calculator.R ---------------------------------------------------
# Tab 2: per-attribute WTP. Each attribute has its own slider and its
# own live WTP card. Values are weighted WTP (attendance share x
# attended-class WTP), scaled by the slider value.

attr_card <- function(ns, id_prefix, label, slider_id, min, max, step, value,
                      data_range_lbl) {
  card(
    card_header(label),
    layout_columns(
      col_widths = c(7, 5),
      div(
        sliderInput(ns(slider_id), data_range_lbl,
                    min = min, max = max, value = value, step = step,
                    width = "100%"),
        helpText(sprintf("Range observed in survey: %s", data_range_lbl))
      ),
      div(
        uiOutput(ns(paste0(id_prefix, "_headline"))),
        plotOutput(ns(paste0(id_prefix, "_dist")), height = "120px")
      )
    )
  )
}

calculator_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "padding: 0.5rem 1rem 0;",
      markdown(paste(
        "Drag a slider to see the household WTP attributable to that",
        "attribute alone. Numbers are **weighted WTP**: the attended-class",
        "marginal WTP scaled by the share of respondents who reported",
        "attending to that attribute. Per-household, one-time payment."
      ))
    ),
    attr_card(ns, "coral", "Coral survival",
              "coral", 0, 45, 1, 10, "0-45 pp"),
    attr_card(ns, "algae", "Macroalgae reduction",
              "algae", 0, 90, 1, 20, "0-90 pp"),
    attr_card(ns, "fish",  "Reef fish abundance",
              "fish",  0, 300, 5, 50, "0-300 pp")
  )
}

calculator_server <- function(id, model) {
  moduleServer(id, function(input, output, session) {

    render_attr <- function(id_prefix, attr_name, slider_input) {
      r <- reactive({
        w <- wtp_marginal(model, attr_name, weighted = TRUE)
        delta <- slider_input()
        list(
          lwr = w$lwr * delta,
          med = w$med * delta,
          upr = w$upr * delta,
          per_pp = w$med,
          share = model$meta$attendance_shares[[attr_name]],
          draws = w$draws * delta
        )
      })

      output[[paste0(id_prefix, "_headline")]] <- renderUI({
        rv <- r()
        tags$div(
          style = "font-family: 'Fraunces', serif; padding: 0.4rem 0 0.6rem;",
          tags$div(style = "font-size: 2.0rem; color: #0E5C6B; font-weight: 500; line-height: 1;",
                   sprintf("$%.2f", rv$med)),
          tags$div(style = "color: #555; font-size: 0.9rem; margin-top: 0.25rem;",
                   sprintf("95%% CI: $%.2f - $%.2f", rv$lwr, rv$upr)),
          tags$div(style = "color: #777; font-size: 0.8rem; margin-top: 0.4rem;",
                   sprintf("$%.2f / pp x %d pp x %.0f%% attendance",
                           rv$per_pp / rv$share, slider_input(), 100 * rv$share))
        )
      })

      output[[paste0(id_prefix, "_dist")]] <- renderPlot({
        rv <- r()
        df <- data.frame(wtp = rv$draws)
        ggplot(df, aes(wtp)) +
          geom_histogram(bins = 30, fill = "#C76A4A", color = "white", alpha = 0.9) +
          geom_vline(xintercept = rv$med, linetype = "dashed",
                     color = "#0E5C6B", linewidth = 0.6) +
          labs(x = NULL, y = NULL) +
          theme_minimal(base_size = 10) +
          theme(axis.text.y = element_blank(),
                panel.grid.minor = element_blank(),
                plot.margin = margin(2, 4, 2, 4))
      })
    }

    render_attr("coral", "coral_survival",  reactive(input$coral))
    render_attr("algae", "algae_reduction", reactive(input$algae))
    render_attr("fish",  "fish_abundance",  reactive(input$fish))
  })
}
