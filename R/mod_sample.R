# mod_sample.R -------------------------------------------------------
# Tab: sample distributions. One row per respondent (taken from the
# choice_data by slicing first row per respondent_id).

sample_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "padding: 0.5rem 1rem 0;",
        markdown(paste(
          "Distribution of the 800 survey respondents across age,",
          "political ideology, and New Ecological Paradigm (NEP) score.",
          "These are the demographics most predictive of differences in",
          "WTP in the manuscript; use the **Heterogeneity** tab to",
          "filter the model on each."
        ))),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Age (years)"),
        plotOutput(ns("age_hist"), height = "280px")
      ),
      card(
        card_header("Political ideology"),
        plotOutput(ns("pol_bar"), height = "280px")
      ),
      card(
        full_screen = FALSE,
        card_header("NEP score (higher = greater environmental concern)"),
        plotOutput(ns("nep_hist"), height = "280px")
      ),
      card(
        card_header("Sample summary"),
        tableOutput(ns("summary_tbl"))
      )
    )
  )
}

sample_server <- function(id, choice_dat) {
  moduleServer(id, function(input, output, session) {

    one_per <- reactive({
      choice_dat[!duplicated(choice_dat$respondent_id), ]
    })

    output$age_hist <- renderPlot({
      d <- one_per()
      ggplot(d, aes(age_numeric)) +
        geom_histogram(binwidth = 5, fill = "#0E5C6B", color = "white",
                       alpha = 0.9, boundary = 0) +
        labs(x = "Age (years)", y = "Respondents") +
        theme_minimal(base_size = 12) +
        theme(panel.grid.minor = element_blank())
    })

    output$pol_bar <- renderPlot({
      d <- one_per()
      d$political_id <- droplevels(d$political_id)
      counts <- as.data.frame(table(d$political_id, useNA = "no"))
      colnames(counts) <- c("ideology", "n")
      counts$ideology <- factor(counts$ideology,
        levels = c("Liberal","Moderate Leaning Liberal","Moderate",
                   "Moderate Leaning Conservative","Conservative"))
      ggplot(counts, aes(ideology, n)) +
        geom_col(fill = "#0E5C6B", width = 0.7) +
        geom_text(aes(label = n), vjust = -0.4, size = 3.6) +
        labs(x = NULL, y = "Respondents") +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(angle = 25, hjust = 1),
              panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank())
    })

    output$nep_hist <- renderPlot({
      d <- one_per()
      ggplot(d[!is.na(d$nep_score), ], aes(nep_score)) +
        geom_histogram(binwidth = 1, fill = "#C76A4A", color = "white",
                       alpha = 0.9) +
        labs(x = "NEP score (sum of 6 centered Likert items)",
             y = "Respondents") +
        theme_minimal(base_size = 12) +
        theme(panel.grid.minor = element_blank())
    })

    output$summary_tbl <- renderTable({
      d <- one_per()
      data.frame(
        Statistic = c("N respondents",
                      "Mean age", "Median age",
                      "% identifying liberal-leaning",
                      "% identifying conservative-leaning",
                      "Mean NEP score", "Median NEP score"),
        Value = c(
          formatC(nrow(d), format = "d", big.mark = ","),
          sprintf("%.1f", mean(d$age_numeric, na.rm = TRUE)),
          sprintf("%.0f", median(d$age_numeric, na.rm = TRUE)),
          sprintf("%.1f%%", 100 * mean(d$political_id %in%
                    c("Liberal","Moderate Leaning Liberal"), na.rm = TRUE)),
          sprintf("%.1f%%", 100 * mean(d$political_id %in%
                    c("Moderate Leaning Conservative","Conservative"), na.rm = TRUE)),
          sprintf("%.2f", mean(d$nep_score, na.rm = TRUE)),
          sprintf("%.2f", median(d$nep_score, na.rm = TRUE))
        )
      )
    }, striped = TRUE, hover = TRUE)
  })
}
