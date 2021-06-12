# Interface

map_customer_ui <-function(id){
  ns<-NS(id)
  
  tagList(
    # Header information
    fluidRow(
      column(width=12, verbatimTextOutput(ns("report_info")))),
    # Boxes for filtering
    fluidRow(
      box(title="Filter", status='primary', width=12, collapsible = TRUE, collapsed = FALSE,
          column(width=4, selectizeInput(ns("Category"), label="Category",choices=NULL, multiple=TRUE)),
          column(width=4, selectizeInput(ns("CustomerType"), label="Customer Type",choices=NULL, multiple=TRUE)),
          column(width=4, actionButton(ns("calc_storage"), "Calculate Storage Location"), align = "center")
      )
    ),
    
    # Box for the Map
    fluidRow(
      uiOutput(ns("box_map"))
    ),
    
    # Boxes for information about customers
    fluidRow(
      uiOutput(ns("box_info_1")),
      uiOutput(ns("box_info_2"))
    ),
    fluidRow(
      uiOutput(ns("box_info_3")),
      uiOutput(ns("box_info_4"))
    )
  )
}


# Functionality

MapViewCustomer <-function(input,output,session){
  
  # Text to describe the displayed page
  output$report_info<-renderText(
    HTML("Map-based information about our customers")
  )

  # Handling of filter values
  selected <- reactiveValues(CategoryName = NULL,
                             CustomerSegment = NULL)
  
  filter <- reactiveValues(CategoryName = unique(data$CategoryName),
                           CustomerSegment = unique(data$CustomerSegment))
  
  observeEvent(eventExpr = input$Category, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$CategoryName <- input$Category
    filter$CategoryName <- if(is.null(selected$CategoryName)) unique(data$CategoryName) else selected$CategoryName
  })
  
  observeEvent(eventExpr = input$CustomerType, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$CustomerSegment <- input$CustomerType
    filter$CustomerSegment <- if(is.null(selected$CustomerSegment)) unique(data$CustomerSegment) else selected$CustomerSegment
  })
  
  # Handling Action button
  observeEvent(eventExpr = input$calc_storage, ignoreNULL = FALSE, ignoreInit = TRUE, {
    
    # Calculation of storage locations (Center-of-Gravity-Method)
    temp <- data %>%
      filter(CategoryName %in% filter$CategoryName, CustomerSegment %in% filter$CustomerSegment, OrderStatus %in% "COMPLETE") %>%
      group_by(CustomerId, `Daysforshipping(real)`, Latitude, Longitude) %>%
      summarise(Benefitperorder = sum(Benefitperorder), Salespercustomer = sum(Salespercustomer), .groups = 'drop') %>%
      ungroup()
    
    sum_days <- sum(temp$`Daysforshipping(real)`)
    center_long <- sum(temp[,"Daysforshipping(real)"] * temp[,"Longitude"])/sum_days
    center_lat <- sum(temp[,"Daysforshipping(real)"] * temp[,"Latitude"])/sum_days
    
    # Add storage marker; Move map to calculated storage position and zoom in
    leafletProxy("map", data=map_data()) %>% addTiles() %>%
      clearMarkers() %>% clearMarkerClusters() %>%
      addAwesomeMarkers(icon = awesomeIcons(markerColor = "blue"),
                        lng=~center_long, lat=~center_lat, popup=paste(sep='</br>',
                                                                paste('<b>New Storage</b>', '</br>lat: ', center_lat, '</br>long: ', center_long))) %>% 
      flyTo(lng = center_long, lat = center_lat, zoom = 10) %>%
      addAwesomeMarkers(clusterOptions = markerClusterOptions(),
                      icon = awesomeIcons(markerColor = map_data()$color_lbls),
                      lng=~Longitude, lat=~Latitude, popup=~popup_content)
  })
  
  
  # Create and fill the filterboxes
  updateSelectizeInput(session, "Category", label=paste0("Category (", length(unique(data$CategoryName)),")"), choices = sort(unique(data$CategoryName)), server=TRUE)
  updateSelectizeInput(session, "CustomerType", label=paste0("CustomerType (", length(unique(data$CustomerSegment)),")"), choices = sort(unique(data$CustomerSegment)), server=TRUE)

  # Reactive value for filtered data. Reacts to set filters
  filterinput_data<-reactive({
    temp_filter<-data%>%
      filter(CategoryName %in% filter$CategoryName, CustomerSegment %in% filter$CustomerSegment)
  })
  
  # Create map box
  output$box_map <- renderUI({
    ns <- session$ns
    
    box(title="Location of Customers", status="primary", width=12,
        leafletOutput(ns("map")))
  })
  
  # Get and transform the data for the map
  map_data <- reactive({
    
    temp <- data %>%
      filter(CategoryName %in% filter$CategoryName, CustomerSegment %in% filter$CustomerSegment) %>%
      group_by(CustomerId, CustomerFname, CustomerLname, CustomerCity, CustomerCountry, CustomerState, CustomerZipcode, CustomerStreet, Latitude, Longitude) %>%
      summarise(Benefitperorder = sum(Benefitperorder), Salespercustomer = sum(Salespercustomer), .groups = 'drop') %>%
      ungroup() %>%
      mutate(color_lbls = ifelse(Benefitperorder > 0, "green", "red"),
             popup_content = paste(sep='</br>',
                                   paste('<b>', paste(sep=' ', CustomerFname, CustomerLname), '</b>', '</br>', CustomerCountry, ', ', CustomerState, '</br>', CustomerZipcode, '</br>', CustomerStreet,
                                         '</br>Average Benefit: ', round(Benefitperorder, digits=0))))
  })
  
  # Render the map
  output$map <- renderLeaflet({
    # If no coordinates exist, a warning is displayed and an empty map is generated
    if(NROW(map_data()) == 0){
      showNotification("No data for selected filter combination", type = "warning")
      leaflet(data=map_data()) %>%
        addProviderTiles(providers$OpenStreetMap, options = providerTileOptions(minZoom = 1))
    } 
    # If only one coordinate exists, the boundaries of the map can not be set. Instead the view is set to the coordinate
    else if(NROW(map_data()) == 1){
      leaflet(data=map_data()) %>%
        addProviderTiles(providers$OpenStreetMap, options = providerTileOptions(minZoom = 1)) %>%
        addAwesomeMarkers(clusterOptions = markerClusterOptions(),
                          icon = awesomeIcons(markerColor = map_data()$color_lbls),
                          lng=map_data()$Longitude, lat=map_data()$Latitude, popup=map_data()$popup_content) %>%
        addLegend("topright", labels = c("Negative Benefit", "Positive Benefit"), title = "Benefit Class", colors = c("#ff2d00", "#00ff36")) %>%
        setView(lng =map_data()$Longitude, lat =map_data()$Latitude, zoom = 4)
    } 
    # Else we can set the map boundaries to  the max/min Long/Lat of the data
    else {
      leaflet(data=map_data()) %>%
        addProviderTiles(providers$OpenStreetMap, options = providerTileOptions(minZoom = 1)) %>%
        fitBounds(~min(Longitude), ~min(Latitude),
                  ~max(Longitude), ~max(Latitude)) %>%
        addAwesomeMarkers(clusterOptions = markerClusterOptions(),
                          icon = awesomeIcons(markerColor = map_data()$color_lbls),
                          lng=~Longitude, lat=~Latitude, popup=~popup_content) %>%
        addLegend("topright", labels = c("Negative Benefit", "Positive Benefit"), title = "Benefit Class", colors = c("#ff2d00", "#00ff36"))
      }
  })
  
  # Observe map changes
  observe(
    leafletProxy("map", data=map_data()) %>%
      clearMarkers() %>% clearMarkerClusters() %>%
      addAwesomeMarkers(clusterOptions = markerClusterOptions(),
                        icon = awesomeIcons(markerColor = map_data()$color_lbls),
                        lng=~Longitude, lat=~Latitude, popup=~popup_content)
  )
  
  # Generate Infoboxes
  # Sum of customers (filtered)
  output$box_info_1<-renderUI({
    ns<-session$ns
    infoBox("Customer Count",
            width = 6,
            value= vec_size(data%>%
                              filter(CategoryName %in% filter$CategoryName, CustomerSegment %in% filter$CustomerSegment) %>%
                              distinct(CustomerId)),
            icon = icon("user-friends"),
            color = "aqua",
            )
    })
  
  # Sum of orders (filtered)
  output$box_info_2<-renderUI({
    ns<-session$ns
    infoBox("Total Orders",
            width = 6,
            value= vec_size(data%>%
                              filter(CategoryName %in% filter$CategoryName, CustomerSegment %in% filter$CustomerSegment)),
            icon = icon("tasks"),
            color = "green",
            )
  })
  
  # Average Product Price (filtered)
  output$box_info_3<-renderUI({
    ns<-session$ns
    temp <- data%>%
      filter(CategoryName %in% filter$CategoryName, CustomerSegment %in% filter$CustomerSegment)
    infoBox("Average Product Price",
            width = 6,
            value= round(mean(temp$ProductPrice), digits = 2),
            icon = icon("tags"),
            color = "red")
  })
  
  # Total Benefit (filtered)
  output$box_info_4<-renderUI({
    ns<-session$ns
    temp <- data%>%
      filter(CategoryName %in% filter$CategoryName, CustomerSegment %in% filter$CustomerSegment)
    
    infoBox("Total Benefit",
            width = 6,
            value= round(sum(temp$Benefitperorder), digits = 2),
            icon = icon("coins"),
            color = "yellow")
  })
  
}