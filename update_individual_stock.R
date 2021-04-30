##select * from [dbo].[DBL]
##select * from [dbo].[HINDCOPPER]

library(jsonlite)
# install.packages("rjson")
library(rjson)
library(stringr)
require(reshape2)
library(dplyr)
library(tidyverse)
require(reshape2)
library(readxl)
library(lubridate)
library(zoo)
source("DButils.R")
List_500_companies <- read_excel("List_500_companies.xlsx")

ini_time <- Sys.time()
stock_data <- NULL
data_ss <- NULL
cast.stock <- NULL
from_Date <- "2021-04-08"
data_ss <- NULL

# company_list_num <- c(8:10)

## old method
# ini_num <- 401
# fin_num <- 410
# ini_company_list_num <- c(ini_num : fin_num)
# asd1 <- as.data.frame(List_500_companies[ini_company_list_num,])
# company_list_num <- c(1: (fin_num - (ini_num-1)))

## new method
ini_num <- c(309:312)
ini_company_list_num <- c(ini_num)
asd1 <- as.data.frame(List_500_companies[ini_company_list_num,])
company_list_num <- c(1: length(ini_num))

com <- c()
for (j in company_list_num){
  # com <- c(com, paste0(List_500_companies$symbol[j], collapse=","))
com[j] <- paste0(asd1$symbol[j], collapse=",")


# com <- paste0(com, collapse = ",")
## Get the stock prices 
data_ss[[j]] <- rjson::fromJSON(file = 
                             paste0("https://fmpcloud.io/api/v3/historical-price-full/",com[j],"?from=",from_Date,"&to=",
                                    Sys.Date(),"&apikey=9021664c6f29c04aa567b7e637a3c70c"))
# print(length(data_ss[[j]]))
# print(str(data_ss[[j]]))
if(length(data_ss[[j]]) > 0){
  data_ss[[j]] <- data_ss[[j]]
} else {
  data_ss[[j]] <- fromJSON(file = "empty.json")
}



# data_ss <- rjson::fromJSON(file = 
#                              paste0("https://fmpcloud.io/api/v3/historical-price-full/ABBOTINDIA.NS,ABCAPITAL.NS,ABFRL.NS,ACC.NS,ADANIENT.NS?from=",
#                                     Sys.Date()-5,"&to=",
#                                     Sys.Date(),"&apikey=9021664c6f29c04aa567b7e637a3c70c"))

# stock_data <- list(data_ss[[1]], data_ss[[2]], data_ss[[3]])

stock_data <- t(as.data.frame(data_ss))
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

## update to New_dates in sql
sqlSave(conn, cast.stock, tablename = "New_dates", rownames = FALSE ,colnames = FALSE, append=TRUE)
sqlQuery(conn, paste0("EXEC update_ticker_table ", "'",asd1$sql_table[j],"'"))
}
final_time <- Sys.time()
final_time - ini_time




## to generate moving average for 1 month EMA

# for (i in no_of_companies){
#   
# cast.stock_filter <- filter_fun(cast.stock, ticker, List_500_companies$symbol[i])
# calendar_date_1_month[paste0(List_500_companies$sql_table[i],"_close")] <- cast.stock_filter$close[match(calendar_date_1_month$final_date, cast.stock_filter$date)]
# calendar_date_1_month[paste0(List_500_companies$sql_table[i],"_low")] <- cast.stock_filter$low[match(calendar_date_1_month$final_date, cast.stock_filter$date)]
# # calendar_date_1_month[is.na(calendar_date_1_month)] <- 0
# calendar_date_1_month <- calendar_date_1_month[rev(order(as.Date(calendar_date_1_month$final_date))),]
# # calendar_date_1_month <- calendar_date_1_month %>% mutate(paste0(List_500_companies$sql_table[i],"_moving_avg") = rollmean(paste0("`",List_500_companies$sql_table[i],"_close","`"), k = 20, fill = NA, align = "left"))
# calendar_date_1_month[paste0(List_500_companies$sql_table[i],"_moving_avg")] <- rollmean(calendar_date_1_month[paste0(List_500_companies$sql_table[i],"_close")], k = 20, fill = NA, align = "left")
# calendar_date_1_month[paste0(List_500_companies$sql_table[i],"_true_false")] <- ifelse(calendar_date_1_month[paste0(List_500_companies$sql_table[i],"_low")] > 
#                                                                                          calendar_date_1_month[paste0(List_500_companies$sql_table[i],"_moving_avg")],1,0)
# calendar_date_1_month_true_false <- calendar_date_1_month %>% select(final_date,matches("true_false"))
# calendar_date_1_month_true_false <- melt(data = calendar_date_1_month_true_false,id.vars = "final_date")
# calendar_date_1_month_true_false <- as.data.frame(table(calendar_date_1_month_true_false$variable,calendar_date_1_month_true_false$value))
# calendar_date_1_month_true_false <- dcast(calendar_date_1_month_true_false, Var1~Var2,value.var = "Freq")
# calendar_date_1_month_true_false$sum <- calendar_date_1_month_true_false$`0` + calendar_date_1_month_true_false$`1`
# calendar_date_1_month_true_false$no_of_zero <- calendar_date_1_month_true_false$`0` / calendar_date_1_month_true_false$sum
# calendar_date_1_month_true_false$no_of_zero <- calendar_date_1_month_true_false$`0` * 100 / calendar_date_1_month_true_false$sum
# calendar_date_1_month_true_false$no_of_zero <- round((calendar_date_1_month_true_false$`0` * 100 / calendar_date_1_month_true_false$sum),1)
# View(calendar_date_1_month_true_false)
# }

## to generate moving average for 1 week EMA
# for (i in company_list_num){
#   
#   cast.stock_filter <- filter_fun(cast.stock, ticker, List_500_companies$symbol[i])
#   calendar_date_week[paste0(List_500_companies$sql_table[i],"_close")] <- cast.stock_filter$close[match(calendar_date_week$max_date, cast.stock_filter$date)]
#   calendar_date_week[paste0(List_500_companies$sql_table[i],"_low")] <- cast.stock_filter$low[match(calendar_date_week$max_date, cast.stock_filter$date)]
#   # calendar_date_week[is.na(calendar_date_week)] <- 0
#   calendar_date_week <- calendar_date_week[rev(order(as.Date(calendar_date_week$max_date))),]
#   # calendar_date_week <- calendar_date_week %>% mutate(paste0(List_500_companies$sql_table[i],"_moving_avg") = rollmean(paste0("`",List_500_companies$sql_table[i],"_close","`"), k = 20, fill = NA, align = "left"))
#   calendar_date_week[paste0(List_500_companies$sql_table[i],"_moving_avg")] <- rollmean(calendar_date_week[paste0(List_500_companies$sql_table[i],"_close")], k = 20, fill = NA, align = "left")
#   calendar_date_week[paste0(List_500_companies$sql_table[i],"_true_false")] <- ifelse(calendar_date_week[paste0(List_500_companies$sql_table[i],"_low")] > 
#                                                                                         calendar_date_week[paste0(List_500_companies$sql_table[i],"_moving_avg")],1,0)
#   calendar_date_week_true_false <- calendar_date_week %>% select(max_date,matches("true_false"))
#   calendar_date_week_true_false$year <- NULL
#   calendar_date_week_true_false$max_date <- NULL
#   calendar_date_week_true_false <- as.data.frame(t(table(stack(calendar_date_week_true_false))))
#   calendar_date_week_true_false <- dcast(calendar_date_week_true_false,ind~values,value.var = "Freq")
#   calendar_date_week_true_false$sum <- calendar_date_week_true_false$`0` + calendar_date_week_true_false$`1`
#   calendar_date_week_true_false$no_of_zero <- calendar_date_week_true_false$`0` / calendar_date_week_true_false$sum
#   calendar_date_week_true_false$no_of_zero <- calendar_date_week_true_false$`0` * 100 / calendar_date_week_true_false$sum
#   calendar_date_week_true_false$no_of_zero <- round((calendar_date_week_true_false$`0` * 100 / calendar_date_week_true_false$sum),1)
#   View(calendar_date_week_true_false)
# }


