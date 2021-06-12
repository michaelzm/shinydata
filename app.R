# Source includes module paths and libraries
source('global.R')

# Define UI for application that draws a histogram
ui <- dashboardPage(skin="green",
                    dashboardHeader(title = "DataCo Supply Chain"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Analytics", tabName="Analytics", icon=icon("chart-pie")),
                        menuItem("Map View", startExpanded=FALSE, icon=icon("map-marked-alt"),
                                 menuSubItem('Customers', tabName = 'MapViewCustomer', icon = icon('users')),
                                 menuSubItem('Deliveries', tabName = 'MapViewDelivery', icon = icon('people-carry'))),
                        menuItem("Forecast", tabName="ForecastView", icon=icon("chart-line"))
                      )
                    ),
                    dashboardBody(
                      # Conditional Panel is used to hide content while loading data, wait for setupComplete in server
                      conditionalPanel(condition = "output.setupComplete",
                        tabItems(
                          tabItem(tabName="Analytics", analytics_ui("Analytics")),
                          tabItem(tabName="MapViewCustomer", map_customer_ui("MapViewCustomer")),
                          tabItem(tabName="MapViewDelivery", map_delivery_ui("MapViewDelivery")),
                          tabItem(tabName="ForecastView", forecast_ui("Forecast"))
                        )
                      ),
                      # This is the loading screen displayed while data is loading
                      conditionalPanel(condition = "!output.setupComplete",
                                          h2("loading data ..."))
                    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Reactive Value to check if the data was loaded and the setup is completed
  rv <- reactiveValues()
  rv$setupComplete <- FALSE
  
  callModule(Analytics, "Analytics")
  callModule(MapViewCustomer, "MapViewCustomer")
  callModule(MapViewDelivery, "MapViewDelivery")
  callModule(ForecastView, "Forecast")
  
  # Data load complete
  rv$setupComplete <- TRUE
  
  # Hide loading Screen and show App
  output$setupComplete <- reactive({
    return(rv$setupComplete)
  })
  outputOptions(output, 'setupComplete', suspendWhenHidden=FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
