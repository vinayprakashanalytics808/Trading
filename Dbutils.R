library(RODBC)
library(rlang)
conn <- odbcDriverConnect(connection = paste0("Driver={SQL Server Native client 11.0};
                            server=localhost;database=Nifty;trusted_connection=yes;"))

filter_fun <- function(df, cat, cat_val){
  df %>%
    filter({{cat}} == cat_val)
}

# sqlSave(conn, cast.stock, tablename = "infratel")

delete_update <- function(r_table,sql_table){
  # sqlQuery(conn,paste0("DELETE FROM [dbo].",sql_table))
  sqlQuery(conn,paste0("DROP TABLE  [dbo].[",sql_table,"]"))
  
  sqlQuery(conn,paste0("CREATE TABLE [dbo].[",sql_table,"](
	[ticker] [varchar](100) NULL,
	[date] [date] NULL,
	[adjClose] [int] NULL,
	[change] [int] NULL,
	[changeOverTime] [int] NULL,
	[changePercent] [int] NULL,
	[close] [int] NULL,
	[date1] [varchar](100) NULL,
	[high] [int] NULL,
	[label] [varchar](100) NULL,
	[low] [int] NULL,
	[open] [int] NULL,
	[unadjustedVolume] [int] NULL,
	[volume] [int] NULL,
	[vwap] [int] NULL
) "))
  
  sqlSave(conn, r_table, tablename = sql_table, rownames = FALSE ,colnames = FALSE, append=TRUE)
}


## Last 5 days
delete_update_last_5_days <- function(r_table,sql_table){
  # sqlQuery(conn,paste0("DELETE FROM [dbo].",sql_table))
  sqlQuery(conn,paste0("DROP TABLE  [dbo].[",sql_table,"]"))
  
  sqlQuery(conn,paste0("CREATE TABLE [dbo].[",sql_table,"](
	[ticker] [varchar](100) NULL,
	[date] [date] NULL,
	[adjClose] [int] NULL,
	[change] [int] NULL,
	[changeOverTime] [int] NULL,
	[changePercent] [int] NULL,
	[close] [int] NULL,
	[date1] [varchar](100) NULL,
	[high] [int] NULL,
	[label] [varchar](100) NULL,
	[low] [int] NULL,
	[open] [int] NULL,
	[unadjustedVolume] [int] NULL,
	[volume] [int] NULL,
	[vwap] [int] NULL
) "))
  
  sqlSave(conn, r_table, tablename = sql_table, rownames = FALSE ,colnames = FALSE, append=TRUE)
}




