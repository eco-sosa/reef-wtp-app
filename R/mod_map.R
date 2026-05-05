# mod_map.R ----------------------------------------------------------
# Tab 4: county-level choropleth. All counties with at least 1 respondent
# are shown. Counties with no respondents are grey.

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
      helpText("Counties shaded by aggregate value. Counties with no",
               "respondents are shown grey.")
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
        if (is.na(v)) return("no respondents")
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
          highlightOptions = highlightOptions(weight = 2, color = "#C9A227",
                                              bringToFront = TRUE)
        ) |>
        addLegend("bottomright", pal = pal, values = vals,
                  title = switch(var,
                    n_resp          = "N respondents",
                    pct_restoration = "% chose restoration"),
                  opacity = 0.9, na.label = "no data")
    })
  })
}
