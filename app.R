options(encoding = "big5")
# Load packages ----

# Load data
source("factory_Taiwan.R", local = TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Taiwan Factory in Metal Product Industry"),
    
    # Input widgets
    sidebarLayout(
        sidebarPanel(
            helpText("Pleace choose the class you would like:"),
            
            selectInput("item", label = "Which class?",
                        choices = list("2511",
                                       "2512",
                                       "2521",
                                       "2522",
                                       "2531",
                                       "2539",
                                       "2541",
                                       "2542",
                                       "2543",
                                       "2544",
                                       "2549",
                                       "2591",
                                       "2592",
                                       "2599",
                                       "Total")),

            submitButton("Submit")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            conditionalPanel("input.item == '2511'",
                             leafletOutput("Map_01")),
            conditionalPanel("input.item == '2512'",
                             leafletOutput("Map_02")),
            conditionalPanel("input.item == '2521'",
                             leafletOutput("Map_03")),
            conditionalPanel("input.item == '2522'",
                             leafletOutput("Map_04")),
            conditionalPanel("input.item == '2531'",
                             leafletOutput("Map_05")),
            conditionalPanel("input.item == '2539'",
                             leafletOutput("Map_06")),
            conditionalPanel("input.item == '2541'",
                             leafletOutput("Map_07")),
            conditionalPanel("input.item == '2542'",
                             leafletOutput("Map_08")),
            conditionalPanel("input.item == '2543'",
                             leafletOutput("Map_09")),
            conditionalPanel("input.item == '2544'",
                             leafletOutput("Map_10")),
            conditionalPanel("input.item == '2549'",
                             leafletOutput("Map_11")),
            conditionalPanel("input.item == '2591'",
                             leafletOutput("Map_12")),
            conditionalPanel("input.item == '2592'",
                             leafletOutput("Map_13")),
            conditionalPanel("input.item == '2599'",
                             leafletOutput("Map_14")),
            conditionalPanel("input.item == 'Total'",
                             leafletOutput("Map_15")),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$Map_Original = renderLeaflet({
        leaflet(Taiwan_C) %>%
            addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
                        fillColor = "blue") %>%
            setView(120.982024, 23.973875, zoom = 7)
    })
    output$Map_01 = renderLeaflet({map_01})
    output$Map_02 = renderLeaflet({map_02})
    output$Map_03 = renderLeaflet({map_03})
    output$Map_04 = renderLeaflet({map_04})
    output$Map_05 = renderLeaflet({map_05})
    output$Map_06 = renderLeaflet({map_06})
    output$Map_07 = renderLeaflet({map_07})
    output$Map_08 = renderLeaflet({map_08})
    output$Map_09 = renderLeaflet({map_09})
    output$Map_10 = renderLeaflet({map_10})
    output$Map_11 = renderLeaflet({map_11})
    output$Map_12 = renderLeaflet({map_12})
    output$Map_13 = renderLeaflet({map_13})
    output$Map_14 = renderLeaflet({map_14})
    output$Map_15 = renderLeaflet({map_15})

}

# Run the application 
shinyApp(ui = ui, server = server)
