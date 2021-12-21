

# Trading

## Goal : To make profits on a short term basis, this application is built. This application is solely used by Mr. Vinay Prakash for his day to day trading 

## Pre-requistes
1. Database : Microsoft Sql Server and List_500_companies.xlsx [can be viewed in project folder]
2. Programming langauge : R
3. Platform to capture Stocks data : Financial Modeling Prep [It is a new concept that informs you about stock markets information (news, currencies and stock prices)]
4. Main R files to consider : app.R(both ui and server logics are here), union_100_Data_points.R, Dbutils.R . Other files/folders can be ignored unless stated

## Description 
The Application features below parameters
1. Captures nifty 500 stocks data
2. Extracts the data from Financial Modeling Prep based on api call and  stores the data into the database and retrieves it when required.  [Tab : DataBase Last 100 records]
3. Makes relationship between day's high and day's open price (Correlation) [Tab : High Vs Open]
4. Makes Prediction(day's high) based on day's open price [Tab : Prediction]
5. Plots open, close, high and low prices for the last 100 records [Tabs : Open Trend, Close Trend, High Trend, Low Trend]
6. Captures the difference between day's high and day's open (Considered as an important factor to invest in stocks)[Tab : high-open]

## Built by (Owner)
Mr. Vinay Prakash

## Why this Application?
Which stock should I invest today? Is it Company A or Company B. This analysis is purely based on Mr.Vinay Prakash Strategy and is not based on any books or others suggestions. Do not use this application without the concent of the owner

## Special Thanks to https://financialmodelingprep.com/ in getting stock data

## Expenses 
$19/Month for purchasing the API

## Current status of API
Not Active (API Not purchased)


