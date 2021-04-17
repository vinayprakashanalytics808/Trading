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
# List_500_companies <- read_excel("remaining.xlsx")
no_of_companies <- c(16:20)
com <- paste0(List_500_companies$symbol[no_of_companies], collapse=",")
## Get the stock prices 
data_ss <- rjson::fromJSON(file = 
                             paste0("https://fmpcloud.io/api/v3/historical-price-full/",com,"?from=2000-01-01&to=",
                                    Sys.Date(),"&apikey=9021664c6f29c04aa567b7e637a3c70c"))

# data_ss <- rjson::fromJSON(file = 
#                              paste0("https://fmpcloud.io/api/v3/historical-price-full/ABBOTINDIA.NS,ABCAPITAL.NS,ABFRL.NS,ACC.NS,ADANIENT.NS?from=",
#                                     Sys.Date()-5,"&to=",
#                                     Sys.Date(),"&apikey=9021664c6f29c04aa567b7e637a3c70c"))

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

for (i in no_of_companies){
  delete_update(filter_fun(cast.stock, ticker, List_500_companies$symbol[i]),List_500_companies$sql_table[i])
}




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






calendar_date <- unique(as.data.frame(cast.stock$date))
colnames(calendar_date) <- c("listed_date")

## to get list of calendar dates for 1 month
# calendar_date$listed_mon_day <- str_sub(calendar_date$listed_date,1,7)
# calendar_date$listed_date_date <- str_sub(calendar_date$listed_date,9,10)
# calendar_date$listed_date_date <- as.numeric(calendar_date$listed_date_date)
# calendar_date_1_month <- calendar_date %>% group_by(listed_mon_day) %>% summarise(max_date = max(listed_date_date))
# calendar_date_1_month$final_date <- paste0(calendar_date_1_month$listed_mon_day, "-", calendar_date_1_month$max_date)
# calendar_date_1_month$final_date <- as.Date(calendar_date_1_month$final_date)

## to get list of calendar dates for 1 Week (End of week)
calendar_date$week_num <- lubridate::isoweek(ymd(calendar_date$listed_date))
calendar_date$year <- str_sub(calendar_date$listed_date,1,4)
calendar_date_week <- calendar_date %>% group_by(year,week_num) %>% summarise(max_date = max(listed_date))


#to update for last 5 days
delete_update_last_5_days(cast.stock, "Last_5_Days")





## Get the list of companies
company_list <- rjson::fromJSON(file = 
                                  "https://financialmodelingprep.com/api/v3/stock/list?apikey=9021664c6f29c04aa567b7e637a3c70c")
company_list_df <- t(as.data.frame(company_list))
company_list_df <- as.data.frame(company_list_df)
company_list_df$description <- rownames(company_list_df)
rownames(company_list_df) <- NULL
company_list_df <- company_list_df %>% mutate(number = as.numeric(str_extract_all(company_list_df$description, "[0-9]+")[[1]])[1])
company_list_df$number <- stringr::str_extract(company_list_df$description, '\\d+')
company_list_df$new_description <- gsub("[[:digit:][:punct:]]", "", company_list_df$description)
company_list_df$description <- NULL
company_list_df[is.na(company_list_df)] <- 0
cast.company_list_df <- dcast(company_list_df, number~new_description,value.var = "V1")

