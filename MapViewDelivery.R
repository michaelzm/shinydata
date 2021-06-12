#Interface

map_delivery_ui <-function(id){
  ns<-NS(id)
  
  
  tagList(
    # Header information
    fluidRow(
      column(width=12, verbatimTextOutput(ns("report_info")))),
    # Boxes for filtering
    fluidRow(
      box(title="Filter", status='primary', width=12, collapsible = TRUE, collapsed = FALSE,
          column(width=4, selectizeInput(ns("Category"), label="Category",choices=NULL, multiple=FALSE)),
          column(width=4, selectizeInput(ns("CustomerType"), label="Customer Type",choices=NULL, multiple=FALSE)),
          column(width=4, selectizeInput(ns("DeliveryStatus"), label="Delivery Status",choices=NULL, multiple=FALSE))
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

MapViewDelivery <-function(input,output,session){
  
  # Text to describe the displayed page
  output$report_info<-renderText(
    HTML("Map-based information about delivery status")
  )
  
  # Handling of filter values
  selected <- reactiveValues(CategoryName = NULL,
                             CustomerSegment = NULL,
                             DeliveryStatus = NULL)
  
  filter <- reactiveValues(CategoryName = unique(data$CategoryName),
                           CustomerSegment = unique(data$CustomerSegment),
                           DeliveryStatus = unique(data$DeliveryStatus)
                           )
  
  observeEvent(eventExpr = input$Category, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$CategoryName <- input$Category
    filter$CategoryName <- if(is.null(selected$CategoryName)) unique(data$CategoryName) else selected$CategoryName
  })
  
  observeEvent(eventExpr = input$CustomerType, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$CustomerSegment <- input$CustomerType
    filter$CustomerSegment <- if(is.null(selected$CustomerSegment)) unique(data$CustomerSegment) else selected$CustomerSegment
  })
  
  observeEvent(eventExpr = input$DeliveryStatus, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$DeliveryStatus <- input$DeliveryStatus
    filter$DeliveryStatus <- if(is.null(selected$DeliveryStatus)) unique(data$DeliveryStatus) else selected$DeliveryStatus
  })
  
  
  # Fill the filterboxes with Data from DB
  updateSelectizeInput(session, "Category", label=paste0("Category (", length(unique(data$CategoryName)),")"), choices = sort(unique(data$CategoryName)), server=TRUE)
  updateSelectizeInput(session, "CustomerType", label=paste0("CustomerType (", length(unique(data$CustomerSegment)),")"), choices = sort(unique(data$CustomerSegment)), server=TRUE)
  updateSelectizeInput(session, "DeliveryStatus", label=paste0("DeliveryStatus (", length(unique(data$DeliveryStatus)),")"), choices = sort(unique(data$DeliveryStatus)), server=TRUE)
  
  filterinput_data<-reactive({
    temp_filter<-data%>%
      filter(CategoryName %in% filter$CategoryName, CustomerSegment %in% filter$CustomerSegment, DeliveryStatus %in% filter$DeliveryStatus)
  })
  
  # Create box for map
  output$box_map <- renderUI({
    ns <- session$ns
    
    box(title="Delivery Overview", status="primary", width=12,
        leafletOutput(ns("map")))
  })
  
  # Get and transform the data for the map
  map_data <- reactive({
    
    temp <- data %>%
      filter(CategoryName %in% filter$CategoryName, CustomerSegment %in% filter$CustomerSegment, DeliveryStatus %in% filter$DeliveryStatus) %>%
      group_by(CustomerId, CategoryName, CustomerSegment, OrderId, orderDate_clean, `Daysforshipping(real)`, `Daysforshipment(scheduled)`, DeliveryStatus, Latitude, Longitude) %>%
      ungroup() %>%
      mutate(color_lbls = ifelse(`Daysforshipping(real)` <= `Daysforshipment(scheduled)`, "green", "red"),
             popup_content = paste(sep='</br>',
                                   paste('<b>', OrderId, '</b>', '</br>', DeliveryStatus, '</br>', orderDate_clean, '</br>', CategoryName, '</br>Days estimated: ', `Daysforshipment(scheduled)`,
                                         '</br>Days acutal: ', `Daysforshipping(real)`)))
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
        addProviderTiles(providers$OpenStreetMap.DE, options = providerTileOptions(minZoom = 1)) %>%
        addCircleMarkers(clusterOptions = markerClusterOptions(),
                         color = map_data()$color_lbls,
                         lng=map_data()$Longitude, lat=map_data()$Latitude, popup=map_data()$popup_content) %>%
        setView(lng =map_data()$Longitude, lat =map_data()$Latitude, zoom = 4)
    } 
    # Else we can set the map boundaries to  the max/min Long/Lat of the data
    else {
      leaflet(data=map_data()) %>%
        addProviderTiles(providers$OpenStreetMap.DE, options = providerTileOptions(minZoom = 1)) %>%
        fitBounds(~min(Longitude), ~min(Latitude),
                  ~max(Longitude), ~max(Latitude)) %>%
        addCircleMarkers(clusterOptions = markerClusterOptions(),
                         color = map_data()$color_lbls,
                         lng=~Longitude, lat=~Latitude, popup=~popup_content)
    }
  })
  
  # Observe map changes
  observe(
    leafletProxy("map", data=map_data()) %>%
      clearMarkers() %>% clearMarkerClusters() %>%
      addCircleMarkers(clusterOptions = markerClusterOptions(),
                       color = map_data()$color_lbls,
                       lng=~Longitude, lat=~Latitude, popup=~popup_content)
  )
  
  # Generate Infoboxes
  # Total Orders (filtered)
  output$box_info_1<-renderUI({
    ns<-session$ns
    infoBox("Total Orders",
            width = 6,
            value= vec_size(data%>%
                              filter(CategoryName %in% filter$CategoryName, CustomerSegment %in% filter$CustomerSegment)),
            icon = icon("tasks"),
            color = "aqua",
    )
  })
  
  # Number of Orders delivered on time (filtered)
  output$box_info_2<-renderUI({
    ns<-session$ns
    
    temp <- data%>%
      filter(CategoryName %in% filter$CategoryName, CustomerSegment %in% filter$CustomerSegment, DeliveryStatus %in% "Shipping on time")
    infoBox("Delivery on Time",
            width = 6,
            value= nrow(temp),
            icon = icon("check"),
            color = "green",
    )
  })
  
  # Number of pending Orders (filtered)
  output$box_info_3<-renderUI({
    ns<-session$ns
    temp <- data%>%
      filter(CategoryName %in% filter$CategoryName, CustomerSegment %in% filter$CustomerSegment, DeliveryStatus %in% "Advance shipping")
    
    infoBox("Pending",
            width = 6,
            value= nrow(temp),
            icon = icon("spinner"),
            color = "yellow")
  }) 
  
  # Number of delayed/canceled Orders (filtered)
  output$box_info_4<-renderUI({
    ns<-session$ns
    temp <- data%>%
      filter(CategoryName %in% filter$CategoryName, CustomerSegment %in% filter$CustomerSegment, DeliveryStatus %in% c("Late delivery", "Shipping canceled"))
    infoBox("Delivery Delayed / Canceled",
            width = 6,
            value= nrow(temp),
            icon = icon("tags"),
            color = "red")
  })
}