#1) create Interfaces - these will be filled in the function below
analytics_ui<-function(id){
  ns<-NS(id)

  tagList(
    #Headline
    fluidRow(
      column(width=12, verbatimTextOutput(ns("report_info")))),
    #Filter
    fluidRow(
      box(title="Filters", status='primary', width=12, collapsible = TRUE, collapsed = FALSE,
          column(width=3, selectizeInput(ns("Category"), label="Category",choices=NULL, multiple=TRUE)),
          column(width=3, selectizeInput(ns("Customertype"), label="Customertype",choices=NULL, multiple=TRUE)),
          column(width=3, selectizeInput(ns("Market"), label="Market",choices=NULL, multiple=TRUE)),
          column(width=3, selectizeInput(ns("Department"), label="Department",choices=NULL, multiple=TRUE)),
          )
      ),
    #Piecharts
    fluidRow(
      uiOutput(ns("Orderstatus")),
      uiOutput(ns("PaymentType"))
    ),
    #Histogram
    fluidRow(uiOutput(ns("histo"))),
    #Tables
    fluidRow(uiOutput(ns("topCustomers"))),
    fluidRow(uiOutput(ns("topItems")))
  )
}

#2) Functionality
Analytics <-function(input,output,session){
  
  #2.1 Generate Interfaces 
  
  #2.1.1 Generate Headline
  output$report_info<-renderText(
    HTML("Various information about orders")
  )
  
  # 2.1.2 Generate Filterboxes
  updateSelectizeInput(session, "Category", label=paste0("Category (", length(unique(data$CategoryName)),")"), choices = sort(unique(data$CategoryName)), server=TRUE)
  updateSelectizeInput(session, "Customertype", label=paste0("Customertype (", length(unique(data$CustomerSegment)),")"), choices = sort(unique(data$CustomerSegment)), server=TRUE)
  updateSelectizeInput(session, "Market", label=paste0("Market (", length(unique(data$Market)),")"), choices = sort(unique(data$Market)), server=TRUE)
  updateSelectizeInput(session, "Department", label=paste0("Department (", length(unique(data$DepartmentName)),")"), choices = sort(unique(data$DepartmentName)), server=TRUE)
  
  filterinput_data<-reactive({
    
    temp_filter<-data%>%
      filter(CategoryName %in% filter$CategoryName, CustomerSegment %in% filter$CustomerSegment, 
             Market %in% filter$Market, DepartmentName %in% filter$DepartmentName)
    
  })
  #Catch selection of filterboxes
  selected <- reactiveValues(CategoryName = NULL,
                             CustomerSegment = NULL,
                             Market = NULL,
                             DepartmentName = NULL)
  
  filter <- reactiveValues(CategoryName = unique(data$CategoryName),
                           CustomerSegment = unique(data$CustomerSegment),
                           Market = unique(data$Market),
                           DepartmentName = unique(data$DepartmentName))
  
  observeEvent(eventExpr = input$Category, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$CategoryName <- input$Category
    filter$CategoryName <- if(is.null(selected$CategoryName)) unique(data$CategoryName) else selected$CategoryName

  })
  
  observeEvent(eventExpr = input$Customertype, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$CustomerSegment <- input$Customertype
    filter$CustomerSegment <- if(is.null(selected$CustomerSegment)) unique(data$CustomerSegment) else selected$CustomerSegment

  })
  
  observeEvent(eventExpr = input$Market, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$Market <- input$Market
    filter$Market <- if(is.null(selected$Market)) unique(data$Market) else selected$Market

  })
  
  observeEvent(eventExpr = input$Department, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$DepartmentName <- input$Department
    filter$DepartmentName <- if(is.null(selected$DepartmentName)) unique(data$DepartmentName) else selected$DepartmentName

  })
  
  #2.1.3 Generate boxes for the piecharts
  output$Orderstatus <- renderUI({
    ns<-session$ns

    box(title="Orderstatus (Filtered)", status="primary",width=6, collapsible = TRUE, collapsed = FALSE,
        plotlyOutput(ns("orderStatus_pie")))
  })

  output$PaymentType<-renderUI({
    ns<-session$ns

    box(title="Payment Type (Filtered)", status="primary",width=6,collapsible = TRUE, collapsed = FALSE,
        plotlyOutput(ns("paymentType_pie")))
  })
  #2.1.4 Generate box for the histogram
  output$histo<-renderUI({
    ns<-session$ns
    #the selection should contain all available entries of Dates at the time of its creation. 
    #To guarantee that, earliest and latest Date that contain an Entry are selected
    start_date<-min(data$orderDate_clean)
    end_date<-max(data$orderDate_clean)
      
    
    box(title="Order Numbers (Filtered)", status = "primary", width=12,collapsible = TRUE, collapsed = FALSE,
        wellPanel(fluidRow(column(width=9, dateRangeInput(ns('dateRange'), label = 'Filter orders by date', start = as.Date(start_date) , end = as.Date(end_date))),
                 column(width=3, radioButtons(ns("shownHisto"), label="Show...",choices=c("Benefit","Quantity"))))),
        fluidRow(column(width=12, plotlyOutput(ns("histogram")))))
    
    
  })
  #2.1.5 Generate  boxes for the tables
  output$topCustomers<-renderUI({
    ns<-session$ns
    
    box(title="Top Customers (Filtered)", status="primary",width=12,collapsible = TRUE, collapsed = FALSE,
        wellPanel(fluidRow(column(width=12, sliderInput(ns("slider_customers"),label="Number of displayed top customers",min=1,max=50,value=10,step=1)))),
        fluidRow(column(width=12, plotlyOutput(ns("table_customers")))))
        
  })
 output$topItems<-renderUI({
    ns<-session$ns
    
    box(title="Top Items (Filtered)", status="primary",width=12,collapsible = TRUE, collapsed = FALSE,
        wellPanel(fluidRow(column(width=8, sliderInput(ns("slider_items"),label="Number of displayed top items",min=1,max=50,value=10,step=1)),
                 column(width=4,radioButtons(ns("shownItems"), label="Show items by...",choices=c("Benefit","Quantity"))),
        )),
        fluidRow(column(width=12, plotlyOutput(ns("tableItems")))))

  })
  
 #2.2 Fill Interfaces 
 
  #2.2.1 Fill piecharts boxes 
  output$orderStatus_pie<-renderPlotly({
    #Prepare data for piechart -->Count, how often each orderstate occurs
    temp<-filterinput_data()%>%
      count(OrderStatus)
    
    #Fill piechart 
    plot_ly(data=temp, type="pie", labels=temp$OrderStatus, values=temp$n,
            textinfo="label+percent",
            insidetextorientation="radial")%>%
      layout(title="")
    
  })
  output$paymentType_pie<-renderPlotly({
    #Prepare data for piechart -->Count, how often each payment type occurs
    temp<-filterinput_data()%>%
      count(Type)

    #Fill piechart 
    plot_ly(data=temp, type='pie', labels=temp$Type, values=temp$n, 
           textinfo='label+percent',
           insidetextorientation='radial')%>%
     layout(title = "")
     
 })
  #2.2.2 Fill histogram
  output$histogram<-renderPlotly({
    #if display of Benefit is selected at runtime
    if(input$shownHisto == 'Benefit'){
      #Prepare data for histogram --> Sum up benefit for selected date range
      temp <- filterinput_data()%>%
        filter(orderDate_clean>= input$dateRange[1] & orderDate_clean <= input$dateRange[2])%>%
        group_by(orderDate_clean)%>%
        summarise(value=sum(Benefitperorder), .groups = "drop")%>%
        ungroup()
      label<-"Benefit (USD per day)"
      
      # If no data was found with active filters, display warning
      if(NROW(temp) == 0){
        showNotification("No data for selected filter combination", type = "warning")
      }

    }
    #if display of quantity of items ordered is selected at runtime
    if(input$shownHisto == 'Quantity'){
      #prepare data for histogram --> sum up number of items ordered for the selected date range
      temp <- filterinput_data()%>%
        filter(orderDate_clean>= input$dateRange[1] & orderDate_clean <= input$dateRange[2])%>%
        group_by(orderDate_clean)%>%
        summarise(value=sum(OrderItemQuantity), .groups = "drop")%>%
        ungroup()
      label<-"Quantity (pieces per day)"
      
      # If no data was found with active filters, display warning
      if(NROW(temp) == 0){
        showNotification("No data for selected filter combination", type = "warning")
      }
    }
    
    #Fill histogramm 
    plot_ly(x = temp$orderDate_clean) %>%
      add_trace(data=temp, type='bar', y=temp$value, name=label) %>%
      layout(title='', showlegend=TRUE)
  })
  
  #2.2.3 Fill tables
  output$table_customers<-renderPlotly({

    #Prepare data for top customer table --> sum up Benefit for each customer and sort that descending
    temp<-filterinput_data()%>%
      group_by(CustomerId,CustomerFname,CustomerLname,CustomerSegment)%>%
      summarise(benefit=paste(round(sum(Benefitperorder),digits=2),"$",sep=" "), .groups = "drop")%>%
      arrange(desc(benefit))%>%
      ungroup()%>%
      #only display number of top Customers that were selected in slider
      slice(1:input$slider_customers[1])
    
    #Fill customer table 
    plot_ly(
      type = 'table',
      #Format headline
      header = list(
        values = c("Benefit", "Customer ID", "Name", "Segment"),
        align = c(rep("center",ncol(temp))),
        line = list(width = 1, color = 'black'),
        fill = list(color = "grey"),
        font = list(family = "Arial", size = 14, color = "white")
      ),
      cells = list(
        #pass prepared data as values
        values = rbind(temp$benefit, temp$CustomerId,paste(temp$CustomerFname,temp$CustomerLname,sep=" "),temp$CustomerSegment),
        align = c(rep("center",ncol(temp))),
        line = list(color = "black", width = 1),
        font = list(family = "Arial", size = 12, color = c("black"))
      ))
  })
  

  output$tableItems<-renderPlotly({
    #if display of Benefit is selected at runtime
    if(input$shownItems == 'Benefit'){
      #Prepare data for top item table --> sum up benefit for each item and sort that descending
      temp<-filterinput_data()%>%
        group_by(OrderItemCardprodId,ProductName,CategoryName,DepartmentName)%>%
        summarise(value=paste(round(sum(OrderItemTotal),digits=2),"$",sep=" "), .groups = "drop")%>%
        arrange(desc(value))%>%
        ungroup()%>%
        #only display number of top items that were selected in slider
        slice(1:input$slider_items[1])
      label<-"Benefit"
      
      
    }
    
    #if display of quantity of sold items is selected at runtime
    if(input$shownItems == 'Quantity'){
      #Prepare data for top item table --> sum up ordered quantity for each item and sort that descending
      temp<-filterinput_data()%>%
        group_by(OrderItemCardprodId,ProductName,CategoryName, DepartmentName)%>%
        summarise(value=sum(OrderItemQuantity), .groups = "drop")%>%
        arrange(desc(value))%>%
        ungroup()%>%
        #only display number of top items that were selected in slider
        slice(1:input$slider_items[1])
      label<-"Pieces"
    }
    
    #Fill top item table
    plot_ly(
      type = 'table',
      #Format headlines
      header = list(
        values = c(label,"Product ID", "Product Name", "Category", "Department"),
        align = c(rep("center",ncol(temp))),
        line = list(width = 1, color = 'black'),
        fill = list(color = "grey"),
        font = list(family = "Arial", size = 14, color = "white")
      ),
      cells = list(
        #pass prepared data as values
        values = rbind(temp$value,temp$OrderItemCardprodId,temp$ProductName,temp$CategoryName,temp$DepartmentName),
        align = c(rep("center",ncol(temp))),
        line = list(color = "black", width = 1),
        font = list(family = "Arial", size = 12, color = c("black"))
      ))
  })
  
  
}

