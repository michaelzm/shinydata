# DB Connection
con <- dbConnect(RSQLite::SQLite(), "DataCoSupplyChainDataset.db")

# Query String to retrieve data from DB
strSQL <- "SELECT * FROM DataCoSupplyChainDataset"

# Format as tibble and add new column with new Date format
data<-dbGetQuery(con,strSQL)%>%
  tibble::as_tibble() %>%
  mutate(orderDate_clean=as.Date(mdy_hm(`orderdate(DateOrders)`)))

# Check if data was retrieved and inform user 
if(NROW(data) == 0){
  showNotification("Could not find data in DataBase. Please check the provided path and query.", type = "error")
  message("Error: Could not find data in DataBase. Please check the provided path and query.")
} else{
  print("Data imported!")
}

# Disconnect from DB
dbDisconnect(con)
