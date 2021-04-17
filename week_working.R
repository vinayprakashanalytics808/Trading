List_500_companies <- read_excel("List_500_companies.xlsx")
no_of_companies <- c(106:110)
com <- paste0(List_500_companies$symbol[no_of_companies], collapse=",")
## Get the stock prices 
data_ss <- rjson::fromJSON(file = 
                             paste0("https://fmpcloud.io/api/v3/historical-price-full/",com,"?from=2000-01-01&to=",
                                    Sys.Date(),"&apikey=9021664c6f29c04aa567b7e637a3c70c"))


stock_data <- t(as.data.frame(data_ss$historicalStockList))
stock_data <- as.data.frame(stock_data)
stock_data$description <- rownames(stock_data)
stock_data$ticker <- ifelse(grepl("symbol", stock_data$description), stock_data$V1, "")
stock_data[stock_data == ""] <- NA
stock_data <- stock_data %>% tidyr::fill(ticker)
stock_data$date <- ifelse(grepl("date", stock_data$description), stock_data$V1, "")
stock_data <- stock_data[!grepl("symbol",stock_data$description),]
stock_data[stock_data == ""] <- NA
stock_data <- stock_data %>% tidyr::fill(date)
rownames(stock_data) <- NULL
stock_data$description <- gsub("[[:digit:][:punct:]]", "", stock_data$description)
stock_data$description_text = substr(stock_data$description,11,nchar(stock_data$description))
stock_data$description <- NULL
cast.stock <- dcast(stock_data, ticker+date~description_text,value.var = "V1")
cast.stock$date <- as.Date(cast.stock$date)
colnames(cast.stock)[8] <- "date1" 
cast.stock <- cast.stock %>% mutate(across(c(adjClose, change,changeOverTime,changePercent,close,high,low,open,unadjustedVolume,volume,vwap), as.numeric))
# sqlSave(conn, cast.stock, tablename = "infratel", rownames = FALSE ,colnames = FALSE, append=FALSE)
# delete_update(filter_fun(cast.stock, ticker, 'ADANIGREEN.NS'),'ADANIGREEN')





## to generate moving average for 1 week EMA
for (i in no_of_companies){
  
  cast.stock_filter <- filter_fun(cast.stock, ticker, List_500_companies$symbol[i])
  calendar_date_week[paste0(List_500_companies$sql_table[i],"_close")] <- cast.stock_filter$close[match(calendar_date_week$max_date, cast.stock_filter$date)]
  calendar_date_week[paste0(List_500_companies$sql_table[i],"_low")] <- cast.stock_filter$low[match(calendar_date_week$max_date, cast.stock_filter$date)]
  # calendar_date_week[is.na(calendar_date_week)] <- 0
  calendar_date_week <- calendar_date_week[rev(order(as.Date(calendar_date_week$max_date))),]
  # calendar_date_week <- calendar_date_week %>% mutate(paste0(List_500_companies$sql_table[i],"_moving_avg") = rollmean(paste0("`",List_500_companies$sql_table[i],"_close","`"), k = 20, fill = NA, align = "left"))
  calendar_date_week[paste0(List_500_companies$sql_table[i],"_moving_avg")] <- rollmean(calendar_date_week[paste0(List_500_companies$sql_table[i],"_close")], k = 20, fill = NA, align = "left")
  calendar_date_week[paste0(List_500_companies$sql_table[i],"_true_false")] <- ifelse(calendar_date_week[paste0(List_500_companies$sql_table[i],"_low")] > 
                                                                                        calendar_date_week[paste0(List_500_companies$sql_table[i],"_moving_avg")],1,0)
  calendar_date_week_true_false <- calendar_date_week %>% select(max_date,matches("true_false"))
  calendar_date_week_true_false$year <- NULL
  calendar_date_week_true_false$max_date <- NULL
  calendar_date_week_true_false <- as.data.frame(t(table(stack(calendar_date_week_true_false))))
  calendar_date_week_true_false <- dcast(calendar_date_week_true_false,ind~values,value.var = "Freq")
  calendar_date_week_true_false$sum <- calendar_date_week_true_false$`0` + calendar_date_week_true_false$`1`
  calendar_date_week_true_false$no_of_zero <- calendar_date_week_true_false$`0` / calendar_date_week_true_false$sum
  calendar_date_week_true_false$no_of_zero <- calendar_date_week_true_false$`0` * 100 / calendar_date_week_true_false$sum
  calendar_date_week_true_false$no_of_zero <- round((calendar_date_week_true_false$`0` * 100 / calendar_date_week_true_false$sum),1)
  View(calendar_date_week_true_false)
}

