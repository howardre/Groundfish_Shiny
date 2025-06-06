## Load libraries
library(leaflet)
library(maps)
library(sp)
library(tidyverse)
library(here)
library(shiny)
library(shinythemes)
library(RColorBrewer)
library(rsconnect)
library(shinyWidgets)

## Import the data
specimens_filter <- readRDS("specimens_filter.RDS")

## Make an interactive map
col_pal <- colorNumeric(palette = "magma", 
                        domain = specimens_filter$length_cm, 
                        na.color = "transparent",
                        reverse = TRUE)

setSliderColor <- function(color, sliderId) {
  
  # some tests to control inputs
  stopifnot(!is.null(color))
  stopifnot(is.character(color))
  stopifnot(is.numeric(sliderId))
  stopifnot(!is.null(sliderId))
  
  # the css class for ionrangeslider starts from 0
  # therefore need to remove 1 from sliderId
  sliderId <- sliderId - 1
  
  # create custom css background for each slider
  # selected by the user
  sliderCol <- lapply(sliderId, FUN = function(i) {
    paste0(
      ".js-irs-", i, " .irs-single,",
      " .js-irs-", i, " .irs-from,",
      " .js-irs-", i, " .irs-to,",
      " .js-irs-", i, " .irs-bar-edge,",
      " .js-irs-", i,
      " .irs-bar{  border-color: transparent;background: ", color[i+1],
      "; border-top: 1px solid ", color[i+1],
      "; border-bottom: 1px solid ", color[i+1],
      ";}"
    )
  })
  
  # insert this custom css code in the head
  # of the shiny app
  custom_head <- tags$head(tags$style(HTML(as.character(sliderCol))))
  return(custom_head)
}

ui <-
    fluidPage(titlePanel("Size and Location of Oregon Groundfishes"),
              theme = shinytheme("darkly"),
              chooseSliderSkin("Flat"),
              setSliderColor("#DAA520", 1),
              sidebarLayout(
                  mainPanel(leafletOutput("map")),
                  sidebarPanel(
                      fluidRow(sliderInput(inputId = "range",
                                           label = "Lengths",
                                           min = min(specimens_filter$length_cm),
                                           max = max(specimens_filter$length_cm),
                                           value = c(min(specimens_filter$length_cm), 
                                                     max(specimens_filter$length_cm)),
                                           ticks = FALSE),
                               selectInput(inputId = "species", 
                                           label = "Species",
                                           choices = unique(as.character(specimens_filter$common_name)),
                                           selected = "Dover sole"),
                               selectInput(inputId = "year", 
                                           label = "Year",
                                           choices = sort(unique(specimens_filter$year)),
                                           selected = 2003,
                                           multiple = TRUE),
                               tags$h1(strong("About"), style = "font-size:15px;"),
                               tags$div(
                                   "The data used to create this tool are available from",
                                   tags$a(href="https://www.webapps.nwfsc.noaa.gov/data/map", 
                                          "the NOAA NWFSC/FRAM Data Warehouse"), 
                                   "under the Trawl Survey specimens file.", style = "font-size:13px;")
                               )
                      )
                  )
              )

# shiny server input/output
server <- function(input, output, session) {
    # filteredData <- reactive({
    #   specimens_filter[specimens_filter$length_cm >= 1 & 
    #                      specimens_filter$length_cm <= 100,]
    # })
    filter_type <- reactive({
        specimens_filter %>%
            filter(common_name %in% input$species,
                   year %in% input$year)
    })
    filter_range <- reactive({
        filter_type() %>% 
            filter(length_cm >= input$range[1] & 
                   length_cm <= input$range[2])
    })
    output$map <- renderLeaflet({
        col_pal <- colorNumeric(palette = "magma", 
                                domain = filter_type()$length_cm, 
                                na.color = "transparent",
                                reverse = TRUE)
        leaflet() %>%
            addProviderTiles(providers$Esri.OceanBasemap) %>%
            # set boundaries for map
            addCircleMarkers(
                data = filter_range(),
                lng = ~ longitude_dd,
                lat = ~ latitude_dd,
                group = "myMarkers",
                radius = 3,
                weight = 1,
                opacity = 100,
                fillOpacity = 100,
                color = ~ ifelse(length_cm > 0, col_pal(length_cm), NA)) %>%
            addLegend(
                "bottomleft",
                pal = col_pal,
                values = filter_type()$length_cm,
                title = "Size of fish",
                opacity = 0.5,
                bins = 4,
                layerId = "legend")
        # add legend for the map
        
    })
    observeEvent(input$species, {
        leafletProxy("map", data = filter_range()[filter_range()$common_name == input$species &
                                                      filter_range()$year == input$year, ]) %>%
            removeControl("legend") %>%
            clearGroup("myMarkers") %>%
            addCircleMarkers(
                ~longitude_dd,
                ~latitude_dd,
                group = "myMarkers",
                radius = 3,
                weight = 1,
                opacity = 100,
                fillOpacity = 100,
                color = ~ ifelse(length_cm > 0, 
                                 col_pal(length_cm), NA))  
        updateSliderInput(session, "range", 
                          min = min(filter_type()$length_cm),
                          max = max(filter_type()$length_cm),
                          value = c(min(filter_type()$length_cm), 
                                    max(filter_type()$length_cm)))
    })
    
}

shinyApp(ui, server)