#Interface

forecast_ui <-function(id){
  ns<-NS(id)
  
  
  tagList(
    #headline
    fluidRow(
      column(width=12, verbatimTextOutput(ns("headline_text")))
    ),
    fluidRow(
      box(title = "Forecast Horizon", status= "primary", width = 12, collapsible = TRUE, collapsed = FALSE,
          column(width=4, selectizeInput(ns("forecast_timeframe"), label="Time Units", choices = c("Days", "Weeks", "Months", "Years"), selected = "Days", multiple=FALSE)
          ),
          column(
            width=4, numericInput(ns("forecast_period"), "Amount:", 10, min = 1, max = 100),verbatimTextOutput("value_input_num")
          ),
      )
    ),
    fluidRow(
      box(title = "Apply Filters", status= "primary", width = 12, collapsible = TRUE, collapsed = FALSE,
          
          column(
            width=3, selectizeInput(ns("filterbox_category_name"), label="Product Category",choices=NULL, multiple=TRUE)
          ),
          column(
            width=3, selectizeInput(ns("filterbox_customer_segment"), label="Customer Segment",choices=NULL, multiple=TRUE)
          ),
          column(
            width=3, selectizeInput(ns("filterbox_order_region"), label="Order Region",choices=NULL, multiple=TRUE)
          ),
      )),
      fluidRow(
        uiOutput(ns("box_forecast_pretty"))
      ),
      fluidRow(
            uiOutput(ns("box_info_total_sales_forecast")),
            uiOutput(ns("box_info_total_per_day"))
      )
  )}


#body
ForecastView <-function(input,output,session){
  #reactive variables: save the user input
  selected <- reactiveValues(CategoryName = NULL,
                             CustomerSegment = NULL,
                             OrderRegion = NULL,
                             forecast_period = NULL,
                             forecast_timeframe = NULL)
  
  stats <- reactiveValues(total_sales = 0,
                          total_per_day = 0)
  
  #get values for the filter boxes
  filter <- reactiveValues(CategoryName = unique(data$CategoryName),
                           CustomerSegment = unique(data$CustomerSegment),
                           OrderRegion = unique(data$OrderRegion))
  
  # init filterbox observers -> watch for inputs
  observeEvent(eventExpr = input$filterbox_category_name, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$CategoryName <- input$filterbox_category_name
    filter$CategoryName <- if(is.null(selected$CategoryName)) unique(data$CategoryName) else selected$CategoryName
  })
  
  observeEvent(eventExpr = input$filterbox_customer_segment, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$CustomerSegment <- input$filterbox_customer_segment
    filter$CustomerSegment <- if(is.null(selected$CustomerSegment)) unique(data$CustomerSegment) else selected$CustomerSegment
  })
  
  observeEvent(eventExpr = input$filterbox_order_region, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$OrderRegion <- input$filterbox_order_region
    filter$OrderRegion <- if(is.null(selected$OrderRegion)) unique(data$OrderRegion) else selected$OrderRegion
  })
  
  observeEvent(eventExpr = input$forecast_period, ignoreNULL = TRUE, ignoreInit = FALSE, {
    selected$forecast_period <- if(is.na(input$forecast_period)) 1 else input$forecast_period
  })
  
  observeEvent(eventExpr = input$forecast_timeframe, ignoreNULL = FALSE, ignoreInit = FALSE, {
    selected$forecast_timeframe <- input$forecast_timeframe
  })
  
  # initialize values for filterboxes
  updateSelectizeInput(session, "filterbox_category_name", label=paste0("Category (", length(unique(data$CategoryName)),")"), choices = sort(unique(data$CategoryName)), server=TRUE)
  updateSelectizeInput(session, "filterbox_customer_segment", label=paste0("CustomerType (", length(unique(data$CustomerSegment)),")"), choices = sort(unique(data$CustomerSegment)), server=TRUE)
  updateSelectizeInput(session, "filterbox_order_region", label=paste0("Region (", length(unique(data$OrderRegion)),")"), choices = sort(unique(data$OrderRegion)), server=TRUE)
  
  
  # get the reactive data frame
  
  # what is getting filtered / modified
  # 1. filter by values of filterbox
  # 2. only get the order_date and order total
  # 3. parse the date 
  # 4. fill missing dates, replace total turnover by 0 for added days
  # 5. sum up all sales of that day
  
  sales_data_filtered <- reactive({
    fill_from <- as.Date("2015-01-01")
    fill_to <- as.Date("2018-01-31")
    
    filtered_data <- data%>%
      filter(CategoryName %in% filter$CategoryName, CustomerSegment %in% filter$CustomerSegment, OrderRegion %in% filter$OrderRegion) %>% 
      select(`orderdate(DateOrders)`, OrderItemTotal, CategoryName, CustomerSegment, OrderRegion) %>%
      rename(order_date = `orderdate(DateOrders)`, total = OrderItemTotal) %>% 
      mutate(order_date=as.Date(order_date, format = "%m/%d/%Y")) %>% 
      group_by(order_date) %>% summarise(total = sum(total), .groups="drop") %>% 
      complete(order_date = seq.Date(fill_from, fill_to, by="day"), fill = list(total = 0)) %>%
      ungroup()
    
  })
  
  # render ui main text
  output$headline_text<-renderText(
    HTML(
      "Forecast of Sales"
    )
  )
  
  # fill info box with value
  output$box_info_total_sales_forecast<-renderUI({
    ns<-session$ns
    infoBox("Forecast in USD",
            width = 6,
            value= round(stats$total_sales),
            icon = icon("user-friends"),
            color = "aqua",
    )
  })
  
  # fill info box with value
  output$box_info_total_per_day<-renderUI({
    ns<-session$ns
    infoBox("Daily Sales",
            width = 6,
            value= round(stats$total_per_day),
            icon = icon("user-friends"),
            color = "aqua",
    )
  })
  
  output$box_forecast_pretty <- renderUI({
    ns<-session$ns
    
    start_date<-min(data$orderDate_clean)
    end_date<-max(data$orderDate_clean)
    
    box(title="Sales Forecast", status = "primary", width=12,
        plotlyOutput(ns("forecast_pretty")))
  })
  
  
  #calculate the forecast and plot with plotly
  output$forecast_pretty <- renderPlotly({
    
    #start of obs is 1.1.2015
    #end of obs is 31.1.2018
    
    # prepare data
    data_sales <- sales_data_filtered()
    
    
    # convert the historical sales data into time series
    train <- data_sales %>% select(total) %>% as.ts()
    
    # convert the time frame together with numeric input into usable range 
    forecast_period_multiplier <- 1
    if ( selected$forecast_timeframe == "Days") {
      forecast_period_multiplier <- 1
    } else if ( selected$forecast_timeframe == "Weeks") {
      forecast_period_multiplier <- 7
    } else if ( selected$forecast_timeframe == "Months") {
      forecast_period_multiplier <- 30
    } else {
      forecast_period_multiplier <- 365
    }
    
    forecast_period_input <- selected$forecast_period * forecast_period_multiplier
    
    # start of forecast algorithms
    # idea behind: use one with known output and easy to calculate (ses)
    # combine that with the "hidden" knowledge neuronal net can detect
    # weight them 50/50 -> get a rbost forecast model
    
    # simple expo smoothing
    model_0 <- ses(train, h=forecast_period_input)
    
    #neuronal network
    model_1 <- forecast(nnetar(train), h=forecast_period_input)
    
    # store the predicitons in a data frame
    ensemble_results <- data.frame(model_0$mean, model_1$mean)
    
    # store informations about obs of the forecast to use later on
    num_cols = ncol(ensemble_results)
    num_rows = nrow(ensemble_results)
    
    # now get the mean() of the different algorithms
    
    ensemble_results$forecast = rowMeans(ensemble_results[,c(1,2)])
    
    # store the forecast values in its own variable
    forecast_values <- ensemble_results$forecast
    
    
    # use basic_date to generate dates in future depending on period 
    # useful when forecast and historical data is merged back together
    basic_date = as.Date("2018-01-31")
    total_sales <- 0
    for (i in 1:num_rows){
      total_day <- ensemble_results[i, num_cols+1]
      #prevent nonzero values
      if (total_day < 0){
        total_day <- 0
      }
      data_sales <- add_row(data_sales, order_date = basic_date+i, total=total_day) 
      total_sales <- total_sales + total_day
    }
    
    # store the total sales in reactive variable to update info box
    stats$total_sales <- total_sales
    stats$total_per_day <- total_sales / forecast_period_input
    
    # now we can plot the time series containing historical and future data poins
    plot_ly(x = data_sales$order_date) %>%
      add_trace(y=data_sales$total, name='Order Total (history)', type='scatter', mode="lines")
  })
}

