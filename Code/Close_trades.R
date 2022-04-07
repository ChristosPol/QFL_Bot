# Preamble ---------------------------------------------------------------------
rm(list = ls())

# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)
library("rjson")

csv_path <- paste0("Trading/QFL/trading_table.csv")
orders <- fread(csv_path)

key = API_Key
secret = API_Sign

offset <- 0
url = "https://api.kraken.com/0/private/ClosedOrders"

i <- 1
trades_raw <- list()
while (offset <= 100) {
  
  trades_raw[[i]] <- get_trade_history(url, key, secret, offset)
  
  offset <- offset + 50
  i <- i +1  
  Sys.sleep(5)
  print(offset)
}

myls <- list()
df_list <- list()
for(k in 1:length(trades_raw)){
  
  for (i in 1:length(trades_raw[[k]]$result$closed)){
    
    final_data <- do.call(rbind, trades_raw[[k]]$result$closed[i])
    myls[[i]] <- as.vector(unlist(final_data[, "status"]))
    names(myls[[i]]) <- rownames(final_data)
    
    
  }
  df_list[[k]] <- unlist(myls)
}
closed <- data.table(ids = names(unlist(df_list)), status = unlist(df_list))
closed <- closed[status =="closed"]


i <- 1
for(i in 1:nrow(orders)){
  if(orders$STATUS_SELL[i] == "OPEN" & orders$ORDER_SELL_ID[i] %in% closed$ids){
    orders$STATUS_SELL[i] <- "CLOSED"
  }
  Sys.sleep(1)
}
View(orders)
# A third script "Close up all trades" that checks the status_sell
fwrite(orders, file = paste0("Trading/QFL/trading_table.csv"))
