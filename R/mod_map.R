# mod_map.R ----------------------------------------------------------
# Tab 4: county-level choropleth. County cells with n<5 respondents are
# suppressed (NA) per the privacy rule in CLAUDE.md.

map_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 280,
      title = "Map options",
      radioButtons(ns("metric"), "Display",
        choices = c("Respondent count" = "n_resp",
                    "% choosing restoration" = "pct_restoration"),
        selected = "n_resp"),
      helpText("Counties shaded by aggregate value. Cells with fewer than",
               "5 respondents are shown grey to protect privacy.")
    ),
    card(
      full_screen = TRUE,
      card_header("Florida counties"),
      leafletOutput(ns("map"), height = "560px")
    )
  )
}

map_server <- function(id, county_sf) {
  moduleServer(id, function(input, output, session) {

    output$map <- renderLeaflet({
      var <- input$metric
      vals <- county_sf[[var]]
      pal <- colorNumeric("viridis", domain = vals, na.color = "#DDDDDD")

      label_fmt <- function(v) {
        if (is.na(v)) return("n < 5 (suppressed)")
        if (var == "n_resp") return(formatC(v, format = "d"))
        sprintf("%.1f%%", v)
      }
      labels <- vapply(vals, label_fmt, character(1))

      leaflet(county_sf) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        addPolygons(
          fillColor = ~pal(vals),
          fillOpacity = 0.85,
          color = "white", weight = 0.8,
          label = sprintf("%s: %s", county_sf$NAME, labels),
          highlightOptions = highlightOptions(weight = 2, color = "#C76A4A",
                                              bringToFront = TRUE)
        ) |>
        addLegend("bottomright", pal = pal, values = vals,
                  title = switch(var,
                    n_resp          = "N respondents",
                    pct_restoration = "% chose restoration"),
                  opacity = 0.9, na.label = "n < 5")
    })
  })
}
