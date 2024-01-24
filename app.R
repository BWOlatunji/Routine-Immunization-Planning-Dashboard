library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(histoslider)
library(lorem)
library(bsicons)
library(leaflet)
library(mapview)
library(osmdata)    # Open Street Map Overpass API
library(osrm)       # Open Street Map Routing API

library(sf)         # Simple Features
library(nngeo)      # Nearest Neighbors

health_care_facilities_geo <- read_rds("data/health_care_facilities_geo.rds")


CHOICES <- list(
    state_names = distinct(health_care_facilities_geo, state_name) |> pull(),
    lga_names = distinct(health_care_facilities_geo, lga_name) |> pull(),
    accessibility = distinct(health_care_facilities_geo, accessibility) |> pull()
)



ui <- page_navbar(
    theme = bs_theme(
        version = 5
    ),
    lang = "en",
    title = tags$span(
        tags$img(
            src = "ngr_logo.png",
            width = "46px",
            height = "auto",
            class = "me-3",
            alt = "Nigeria Flag logo"
        ),
        "Routine Immunization Planning in Nigeria Dashboard"
    ),
    sidebar = sidebar(width = 275),
    nav_spacer(),
    nav_panel(
        "RI overview",
        #uiOutput("value_boxes"),
        layout_columns(
            #flights_card, avg_delay_by_category
        ),
        #delay_dist
    ),
    nav_panel(
        "Trip Plan",
        # card(
        #     card_header("Flight data"),
        #     DT::dataTableOutput("export")
        # )
    )
)


server <- function(input, output, session) {
  
}

shinyApp(ui, server)

