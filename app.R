library(shiny)
library(bslib)
library(tidyverse)
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

sidebar_acc <- accordion(
  open = c("State", "LGA"),
  accordion_panel(
    "Location",
    icon = fontawesome::fa("tree-city"),
    uiOutput("location_reset"),
    selectizeInput(
      "state_name", "State",
      choices = CHOICES$state_names,
      options = list(plugins = "remove_button", closeAfterSelect = TRUE)
    ),
    uiOutput("lga_name")
  ),
  accordion_panel(
    "RI Service Status",
    # <i class="fa-solid fa-syringe"></i>
    icon = fontawesome::fa("syringe"),
    radioButtons(inputId = "ri_service_status",
                 label = "RI Status",
                 choices = c("Available" = "1",
                             "Not Available" = "0"))
  )
)


PRIMARY <- "#0675DD"

ui <- page_navbar(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  theme = bs_theme(
    preset = "shiny",
    "primary" = PRIMARY
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
        "Routine Immunization Dashboard"
    ),
    sidebar = sidebar(width = 275, sidebar_acc),
    nav_spacer(),
    nav_panel(
        "RI overview",
        uiOutput("value_boxes"),
        layout_columns(
          div(
            class = "panel", 
            div(
              class = "panel-header",
              h4("Hospitals with Cold Chain Equipment")
            ),
            div(
              class = "panel-body",
              leafletOutput(outputId = "map_plot")
            )
          )
        ),
        #delay_dist
    ),
    nav_panel(
        "Trip Plan"#,
        # card(
        #     card_header("Flight data"),
        #     DT::dataTableOutput("export")
        # )
    )
)


server <- function(input, output, session) {
  
  output$lga_name <- renderUI({
    req(c(input$state_name))
    lgas <- health_care_facilities_geo |> 
      filter(state_name == input$state_name) |> 
      distinct(lga_name) |> pull()
      
    selectizeInput(
      "lga_name", "LGA",
      choices = lgas,
      options = list(plugins = "remove_button", closeAfterSelect = TRUE)
    )
  })
  
  summary_vals <- reactiveValues(
  cce_count = scales::comma(sum(as.numeric(health_care_facilities_geo$cce_quantity))),
  
  state_count = health_care_facilities_geo |> 
    filter(cce_quantity>0) |> distinct(state_name) |> nrow(),
  
  ri_service = scales::comma(health_care_facilities_geo |> 
    filter(cce_quantity>0 & ri_service_status == 1) |> 
    pull(cce_quantity) |> as.numeric() |> sum()),
  
  no_ri_service = scales::comma(health_care_facilities_geo |> 
    filter(cce_quantity>0 & ri_service_status == 0) |> 
    pull(cce_quantity) |> as.numeric() |> sum()) #,
  
  # accessible_ri_service = scales::comma(health_care_facilities_geo |> 
  #                                 filter(cce_quantity>0 & ri_service_status == 1 & accessibility==1) |> 
  #                                 select(accessibility) |> nrow())
  )
  
  output$value_boxes <- renderUI({
    n_cce <- value_box(
      "A TOTAL OF",
      paste(summary_vals$cce_count, "CCEs"),
      paste("Across", summary_vals$state_count, "states in Nigeria"),
      theme_color = "primary",
      showcase = icon("hospital")
    )
    
    ri_service <- value_box(
      "RI Service",
      paste(summary_vals$ri_service, " CCEs"),
      tags$p(paste(
        " with RI services"
      )),
      theme_color = "success",
      showcase = icon("vial-circle-check")
    )
    
    no_ri_service <- value_box(
      "RI SERVICE",
      paste0(summary_vals$no_ri_service, " CCEs "),
      tags$p(paste(
        " with no RI services"
      )),
      theme_color = "danger",
      showcase = icon("syringe")
    )
    
    layout_columns(class = "mb-0", n_cce, ri_service, no_ri_service)
  })
}

shinyApp(ui, server)

