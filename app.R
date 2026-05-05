# Florida Reef Restoration WTP Explorer
# Companion app to Chapter 2 of "Blueprints for the Blue"
# Brandon Sosa, FIU Earth System Science

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(leaflet)
library(sf)
library(DT)

# Files in R/ are auto-sourced by Shiny. No manual source() needed.

model_obj  <- readRDS("data/model_m5.rds")
choice_dat <- readRDS("data/choice_data.rds")
county_sf  <- readRDS("data/fl_counties.rds")

# Headline marginal WTP (attended class) and sample stats, all live
.hl_coral <- wtp_marginal(model_obj, "coral_survival")
.hl_algae <- wtp_marginal(model_obj, "algae_reduction")
.hl_fish  <- wtp_marginal(model_obj, "fish_abundance")

.one_per <- choice_dat[!duplicated(choice_dat$respondent_id), ]
.stats <- list(
  n_resp        = nrow(.one_per),
  n_obs         = sum(choice_dat$alt == 1),
  n_counties    = sum(!is.na(county_sf$n_resp) & county_sf$n_resp >= 1),
  mean_age      = mean(.one_per$age_numeric, na.rm = TRUE),
  mean_nep      = mean(.one_per$nep_score,   na.rm = TRUE),
  pct_coastal   = 100 * mean(.one_per$coastal == "Yes", na.rm = TRUE),
  pct_visit     = 100 * mean(.one_per$visit   == "Yes", na.rm = TRUE),
  att_coral     = 100 * model_obj$meta$attendance_shares[["coral_survival"]],
  att_algae     = 100 * model_obj$meta$attendance_shares[["algae_reduction"]],
  att_fish      = 100 * model_obj$meta$attendance_shares[["fish_abundance"]]
)

reef_theme <- bs_theme(
  version = 5,
  bg = "#FAF7F2",
  fg = "#1A2E35",
  primary = "#0E5C6B",
  secondary = "#C76A4A",
  base_font = font_google("Source Sans 3"),
  heading_font = font_google("Fraunces", wght = "500"),
  font_scale = 1.0
)

ui <- page_navbar(
  title = tags$span(
    style = "font-family: 'Fraunces', serif; font-weight: 500;",
    "Reef Restoration WTP Explorer"
  ),
  theme = reef_theme,
  navbar_options = navbar_options(bg = "#0E5C6B"),

  nav_panel(
    "Overview",
    # Hero: per-attribute WTP value boxes
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(
        title    = "Coral outplant survival",
        value    = sprintf("$%.2f / pp", .hl_coral$med),
        showcase = bsicons::bs_icon("water"),
        theme    = "primary",
        p(sprintf("95%% CI $%.2f - $%.2f", .hl_coral$lwr, .hl_coral$upr))
      ),
      value_box(
        title    = "Macroalgae reduction",
        value    = sprintf("$%.2f / pp", .hl_algae$med),
        showcase = bsicons::bs_icon("flower3"),
        theme    = "primary",
        p(sprintf("95%% CI $%.2f - $%.2f", .hl_algae$lwr, .hl_algae$upr))
      ),
      value_box(
        title    = "Fish abundance in restored areas",
        value    = sprintf("$%.2f / pp", .hl_fish$med),
        showcase = bsicons::bs_icon("droplet-half"),
        theme    = "primary",
        p(sprintf("95%% CI $%.2f - $%.2f", .hl_fish$lwr, .hl_fish$upr))
      )
    ),
    # About + mini map
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("About this study"),
        card_body(
          markdown(paste0("
This interactive companion accompanies a discrete choice experiment
estimating Florida residents' willingness-to-pay (WTP) for coral reef
restoration outcomes. **N = 800** Florida adults, fielded 2024,
**3 choice tasks per respondent** (2,400 choice observations).

The selected model is an **attribute non-attendance multinomial logit**
(ANA-MNL). Each attribute carries paired attended / non-attended
coefficients; headline numbers above use the **attended class**
(respondents who report taking that attribute into account when
choosing). Use the tabs above to simulate per-attribute WTP,
explore demographic heterogeneity, view geographic patterns,
and inspect model diagnostics."))
        )
      ),
      card(
        card_header("Where respondents live"),
        leafletOutput("overview_map", height = "320px")
      )
    ),
    # Stats grid
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box("Respondents",   formatC(.stats$n_resp,    format = "d", big.mark = ","),
                showcase = bsicons::bs_icon("people"),     theme = "secondary"),
      value_box("Choice obs.",   formatC(.stats$n_obs,     format = "d", big.mark = ","),
                showcase = bsicons::bs_icon("list-check"), theme = "secondary"),
      value_box("FL counties (n>=1)", as.character(.stats$n_counties),
                showcase = bsicons::bs_icon("geo-alt"),    theme = "secondary"),
      value_box("Mean age (yrs)", sprintf("%.1f", .stats$mean_age),
                showcase = bsicons::bs_icon("person"),     theme = "secondary"),
      value_box("Coastal county", sprintf("%.0f%%", .stats$pct_coastal),
                showcase = bsicons::bs_icon("water"),      theme = "light"),
      value_box("Visited FKNMS",  sprintf("%.0f%%", .stats$pct_visit),
                showcase = bsicons::bs_icon("compass"),    theme = "light"),
      value_box("Mean NEP score", sprintf("%.2f", .stats$mean_nep),
                showcase = bsicons::bs_icon("globe-americas"), theme = "light"),
      value_box(
        title = "Attended (coral / algae / fish)",
        value = sprintf("%.0f / %.0f / %.0f%%",
                        .stats$att_coral, .stats$att_algae, .stats$att_fish),
        showcase = bsicons::bs_icon("eye"),
        theme = "light"
      )
    )
  ),

  nav_panel("WTP Calculator",   calculator_ui("calc")),
  nav_panel("Sample",           sample_ui("samp")),
  nav_panel("Heterogeneity",    heterogeneity_ui("het")),
  nav_panel("Geography",        map_ui("map")),
  nav_panel("Model Diagnostics", diagnostics_ui("diag")),

  nav_spacer()
)

server <- function(input, output, session) {
  calculator_server("calc", model_obj)
  sample_server("samp", choice_dat)
  heterogeneity_server("het", model_obj, choice_dat)
  map_server("map", county_sf)
  diagnostics_server("diag", model_obj, choice_dat)

  output$overview_map <- renderLeaflet({
    pal <- colorNumeric("viridis", domain = county_sf$n_resp,
                        na.color = "#DDDDDD")
    label_fmt <- function(v) {
      if (is.na(v)) "no respondents" else formatC(v, format = "d")
    }
    labels <- vapply(county_sf$n_resp, label_fmt, character(1))
    leaflet(county_sf,
            options = leafletOptions(zoomControl = FALSE,
                                     attributionControl = FALSE)) |>
      addProviderTiles(providers$CartoDB.PositronNoLabels) |>
      addPolygons(
        fillColor = ~pal(n_resp),
        fillOpacity = 0.85,
        color = "white", weight = 0.6,
        label = sprintf("%s: %s", county_sf$NAME, labels),
        highlightOptions = highlightOptions(weight = 1.5, color = "#C76A4A",
                                            bringToFront = TRUE)
      ) |>
      addLegend("bottomright", pal = pal, values = ~n_resp,
                title = "N respondents", opacity = 0.9, na.label = "none")
  })
}

shinyApp(ui, server)
