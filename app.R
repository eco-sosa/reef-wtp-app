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

# Headline marginal WTP from the loaded model (attended class)
.hl <- list(
  coral = wtp_marginal(model_obj, "coral_survival")$med,
  algae = wtp_marginal(model_obj, "algae_reduction")$med,
  fish  = wtp_marginal(model_obj, "fish_abundance")$med
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
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("About this study"),
        card_body(
          markdown(sprintf("
This interactive companion accompanies a discrete choice experiment
estimating Florida residents' willingness-to-pay (WTP) for coral reef
restoration outcomes. **N = 800** Florida adults, fielded 2024,
**3 choice tasks per respondent** (2,400 choice observations).

The displayed model is an **attribute non-attendance multinomial logit**
(ANA-MNL). Headline marginal WTP for the attended class, per percentage
point of attribute change:

- **$%.2f** per pp increase in coral outplant survival
- **$%.2f** per pp reduction in macroalgae cover
- **$%.2f** per pp increase in fish abundance in restored areas

Use the tabs above to simulate scenarios, explore demographic
heterogeneity, view geographic patterns, and inspect model diagnostics.
          ", .hl$coral, .hl$algae, .hl$fish))
        )
      ),
      card(
        card_header("Quick stats"),
        layout_columns(
          col_widths = c(6, 6),
          value_box("Respondents",  "800",   showcase = bsicons::bs_icon("people")),
          value_box("Choice tasks", "2,400", showcase = bsicons::bs_icon("list-check")),
          value_box("FL counties",  "67",    showcase = bsicons::bs_icon("geo-alt")),
          value_box("Attendance classes", "2", showcase = bsicons::bs_icon("diagram-3"))
        )
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
}

shinyApp(ui, server)
