library(shiny)
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
library(shinyalert)
library(DT)
library(plotly)
source("DButils.R")
source("union_100_Data_points.R")
List_500_companies <- read_excel("List_500_companies.xlsx")

ui <- fluidPage(
  # tags$style('.container-fluid {
  #                            background-color: #CCCCFF;
  #             }'),
  fluidRow(column(width = 4, dateInput("from","From",value = Sys.Date()-15)),br(),
           # column(width = 4, h5("Total Investment Per Share as per Predicted Open"),textOutput("investment")), 
           column(width = 4, h5("Total Returns per Share in %"),textOutput("returns"))),
  fluidRow(column(width = 4, selectInput("com", "Companies", choices = List_500_companies$symbol, multiple = TRUE))
           # ,column(width = 4, rHandsontableOutput('table'))
           ),
  
  # verbatimTextOutput("slno"),
  actionButton("exe","Update Individual tables"),
  actionButton("exe_top_hundred","Update top 100 tables"),
  br(),br(),
  tabsetPanel(
    tabPanel("DataBase Last 100 records",br(), dataTableOutput("top_hundred")),
    tabPanel("DataBase Postitive Open Low",br(), dataTableOutput("positive_openlow")),
    # tabPanel("Current Market Price",br(), dataTableOutput("cmp")),
    tabPanel("Current Market Price", br(),
             actionButton("start", "Start"), plotlyOutput("cmp")),
    tabPanel("Open Vs Previous Close",br(), plotlyOutput("oprec")),
    tabPanel("Predict Open",br(), 
             fluidRow(
               # column(width = 3, selectInput("tic","Ticker",choices = List_500_companies$symbol)),
               uiOutput("tic"),
               column(width = 3,numericInput("close_price","Close Price",value = 3)), 
               # column(width = 3, br(), actionButton("cal","Calculate")),
               column(width = 3, br(), verbatimTextOutput("predicted_open"))
               )),
    tabPanel("Returns",br(), uiOutput("ret"))
  ),
  # dataTableOutput("top_hundred"),
  br(),
  tabsetPanel(
    tabPanel("Open Low High",plotlyOutput("openlow")),
    tabPanel("Invest/Not Invest",plotlyOutput("inv_not_inv")),
    tabPanel("Open Trend",plotlyOutput("open")),
    tabPanel("Low Trend",plotlyOutput("low")),
    tabPanel("High Trend",plotlyOutput("high")),
    tabPanel("Close Trend",plotlyOutput("close"))
  ),
  # plotlyOutput("openlow"),
  # dataTableOutput("positive_openlow"),
  # verbatimTextOutput("mp")
)

server <- function(input, output, session) {
  
  data_table <- reactiveValues()
  x <- reactiveVal()
  values <- reactiveValues(data = data.frame(Description = c("INR","Price Per Share","High"),val = c(0,0,0)))
  
  observe({
    if(!is.null(input$table))
      values$data <- hot_to_r(input$table)
  })
  
  output$table <- renderRHandsontable({
    rhandsontable(values$data)
  })
  
  
  # output$result <- renderText({ 
  #   values$data[["val"]][1] / values$data[["val"]][2]
  # })
  
  observeEvent(input$exe,{
    
    ini_time <- Sys.time()
    stock_data <- NULL
    data_ss <- NULL
    cast.stock <- NULL
    from_Date <- input$from
    data_ss <- NULL
    
    ## new method
    #ini_num <- c(309:312)
    ini_num <- List_500_companies$SL_no[List_500_companies$symbol %in% unlist(strsplit(paste0(input$com,collapse = ","),','))]
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
                                               Sys.Date(),"&apikey=d044d11c5bbbc7c89697083850466e50"))
      
      ## old api key : 9021664c6f29c04aa567b7e637a3c70c
      

      
      if(length(data_ss[[j]]) > 0){
        data_ss[[j]] <- data_ss[[j]]
      } else {
        data_ss[[j]] <- fromJSON(file = "empty.json")
      }
      
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
      
      ## update to New_dates in sql
      sqlSave(conn, cast.stock, tablename = "New_dates", rownames = FALSE ,colnames = FALSE, append=TRUE)
      sqlQuery(conn, paste0("EXEC update_ticker_table ", "'",asd1$sql_table[j],"'"))
    }
    final_time <- Sys.time()
    total_time <- final_time - ini_time
    showNotification("Table updated")
    # shinyalert("Oops!", "Something went wrong.", type = "error")
    
  })
  
  output$slno <- renderPrint({
    List_500_companies$SL_no[List_500_companies$symbol %in% unlist(strsplit(paste0(input$com,collapse = ","),','))]
  })

  output$mp <- renderPrint({
    invalidateLater(1000)
    xnew <- as.data.frame(sqlQuery(conn, "select * from AARTIDRUGS_copy"))
    # xnew <- nrow(xnew)
    # x(c(x(), xnew))
    # rjson::fromJSON(file = "https://financialmodelingprep.com/api/v3/quote-short/INFY.NS?apikey=d044d11c5bbbc7c89697083850466e50")
    nrow(xnew)
  })
  
  observeEvent(input$exe_top_hundred,{
    
    sam_df_All <- update_top_hundred_tables()
    sam_df_All <- sam_df_All %>% distinct(ticker, date, .keep_all=TRUE)
    sam_df_All <- sam_df_All[!is.na(sam_df_All$openlow), ]
    sam_df_All <- as.data.frame(sam_df_All %>% group_by(ticker) %>% mutate(Previous_close = dplyr::lead(close, n = 1, default = NA)))
    sam_df_All <- sam_df_All[complete.cases(sam_df_All), ]
    sam_df_All <- sam_df_All %>% group_by(ticker) %>% mutate(pred_open = round(fitted(lm(open ~ Previous_close)),2)) 
    sam_df_All$`High-pred_open` <- round((sam_df_All$high - sam_df_All$pred_open),2)
    sam_df_All$`Open-pred_open` <- round((sam_df_All$open - sam_df_All$pred_open),2)
    sam_df_All$`Close-pred_open` <- round((sam_df_All$close - sam_df_All$pred_open),2)
    sam_df_All$`pred_open-low` <- round((sam_df_All$pred_open - sam_df_All$low),2)
    sam_df_All <- sam_df_All %>% mutate(First_condition = if_else(openlow < 0 & `Open-pred_open` > 0 & `Close-pred_open` > 0,"Inv","Not Inv"),
                          Second_condition = if_else(openlow < 0 & `Open-pred_open` < 0 & `Close-pred_open` < 0,"Inv","Not Inv"),
                          Third_condition = if_else(openlow < 0 & `Open-pred_open` < 0 & `Close-pred_open` > 0,"Inv","Not Inv"))
    sam_df_All <- sam_df_All %>% mutate(Pre_con = if_else(First_condition == "Inv" | Second_condition == "Inv" | Third_condition == "Inv",
                                            "Inv","Not Inv"))
    sam_df_All$`Pred_open - Low_valid` <-  ifelse(sam_df_All$`pred_open-low` > 0 , "Valid","Not Valid")
    sam_df_All$`High-pred_open_valid` <-  ifelse(sam_df_All$`High-pred_open` > 0 , "Valid","Not Valid")
    sam_df_All <- sam_df_All %>% mutate(Final_Decision = if_else(Pre_con == "Inv" & `Pred_open - Low_valid` == "Valid" & `High-pred_open_valid` == "Valid",
                                                          "Invest","Not Invest"))
    sam_df_All$Final_Decision_0_1 <- ifelse(sam_df_All$Final_Decision == "Invest" , 1, -1)
    sam_df_All <- sam_df_All %>% group_by(ticker) %>% mutate(`Std_Open-pred_open` = sd(`Open-pred_open`))
    sam_df_All$relation <- sam_df_All$open / (sam_df_All$`Std_Open-pred_open` * sam_df_All$pred_open)
    sam_df_All <- sam_df_All[c(1,2,12,11,9,7,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34, 3,4,5,6,8,10,13,14,15)]
    
    sam_df_All_positive <- sam_df_All %>% group_by(ticker) %>% summarise(max_date = max(date))
    # View(sam_df_All_positive)
    sam_df_All_positive$openlow <- sam_df_All$openlow[match(sam_df_All_positive$ticker, sam_df_All$ticker)]
    data_table$sam_df_All_filtered <-  function(){
      sam_df_All[sam_df_All$ticker %in% unlist(strsplit(paste0(input$com,collapse = ","),',')),]
    }
    
    output$top_hundred <- renderDataTable(server = FALSE, {
    data_table$sam_df_All_filtered()
    datatable(data_table$sam_df_All_filtered() ,rownames = F,
              class   = 'cell-border stripe compact hover',
              escape  = F, selection = 'multiple',extensions = 'Buttons',
              options = list(autoWidth = TRUE,columnDefs = list(list(width = '150px', targets = "_all")
                                                                ,list(visible=FALSE, targets=c(15:20,25:33))
                                                                ),
                             scrollX = TRUE,
                             dom = 'Brtip',
                             buttons = c('copy', 'csv', 'excel')
                             ))
    })       
    

    
    sum_predicted_open <- reactive({
      data_table$sam_df_All_filtered()
      sum(data_table$sam_df_All_filtered()[["pred_open"]][data_table$sam_df_All_filtered()[["Final_Decision"]] == "Invest"])
    })
    
    sum_profit <- reactive({
      data_table$sam_df_All_filtered()
      sum(data_table$sam_df_All_filtered()[["High-pred_open"]][data_table$sam_df_All_filtered()[["Final_Decision"]] == "Invest"])
    })
    
    output$returns <- renderText({
      round((sum_profit() * 100/ sum_predicted_open()),2) 
    })
    
    output$ret <- renderUI({
      data_table$sam_df_All_filtered()
      # asd <- data.frame(Companies = input$com)
      asd <- data_table$sam_df_All_filtered() %>% filter(Final_Decision == "Invest") %>% group_by(ticker) %>% summarise(Returns = sum(`High-pred_open`)*100 / sum(`pred_open`))
      output$aa <- renderRHandsontable(rhandsontable(asd))
      rHandsontableOutput("aa")
    })

    output$openlow <- renderPlotly({
      data_table$sam_df_All_filtered()
      # sam_df_All_filtered <-  sam_df_All[sam_df_All$ticker %in% unlist(strsplit(paste0(input$com,collapse = ","),',')),]
      p <- ggplot(data_table$sam_df_All_filtered(), aes(x=date, y=openlow)) +
        geom_line(aes(colour = ticker)) +
        xlab("") +
        theme(axis.text.x=element_text(angle=60, hjust=1))
        ggplotly(p)
    })
    
    output$inv_not_inv <- renderPlotly({
      data_table$sam_df_All_filtered()
      # sam_df_All_filtered <-  sam_df_All[sam_df_All$ticker %in% unlist(strsplit(paste0(input$com,collapse = ","),',')),]
      p <- ggplot(data_table$sam_df_All_filtered(), aes(x=date, y=`Open-pred_open`)) +
        geom_line(aes(colour = ticker)) +
        xlab("") +
        theme(axis.text.x=element_text(angle=60, hjust=1))
      ggplotly(p)
    })
    
    output$open <- renderPlotly({
      data_table$sam_df_All_filtered()
      # sam_df_All_filtered <-  sam_df_All[sam_df_All$ticker %in% unlist(strsplit(paste0(input$com,collapse = ","),',')),]
        p <- ggplot(data_table$sam_df_All_filtered(), aes(x=date, y=open)) +
        geom_line(aes(colour = ticker)) + xlab("") + theme(axis.text.x=element_text(angle=60, hjust=1))
        ggplotly(p)
    })
    
    output$low <- renderPlotly({
      data_table$sam_df_All_filtered()
      # sam_df_All_filtered <-  sam_df_All[sam_df_All$ticker %in% unlist(strsplit(paste0(input$com,collapse = ","),',')),]
      p <- ggplot(data_table$sam_df_All_filtered(), aes(x=date, y=low)) +
        geom_line(aes(colour = ticker)) + xlab("") + theme(axis.text.x=element_text(angle=60, hjust=1))
      ggplotly(p)
    })
    
    output$high <- renderPlotly({
      data_table$sam_df_All_filtered()
      # sam_df_All_filtered <-  sam_df_All[sam_df_All$ticker %in% unlist(strsplit(paste0(input$com,collapse = ","),',')),]
      p <- ggplot(data_table$sam_df_All_filtered(), aes(x=date, y=high)) +
        geom_line(aes(colour = ticker)) + xlab("") + theme(axis.text.x=element_text(angle=60, hjust=1))
      ggplotly(p)
    })
    
    output$close <- renderPlotly({
      data_table$sam_df_All_filtered()
      invalidateLater(1000)
      xnew <- data.frame(sqlQuery(conn, "select * from AARTIDRUGS_copy"))
      xnew <- nrow(xnew)
      # sam_df_All_filtered <-  sam_df_All[sam_df_All$ticker %in% unlist(strsplit(paste0(input$com,collapse = ","),',')),]
      p <- ggplot(data_table$sam_df_All_filtered(), aes(x=date, y=close)) +
        geom_line(aes(colour = ticker)) + 
        geom_point(aes(x = Sys.Date(), y = xnew)) + xlab("") + theme(axis.text.x=element_text(angle=60, hjust=1))
      ggplotly(p)
    })
    
    output$positive_openlow <- renderDataTable({
      datatable(sam_df_All_positive[sam_df_All_positive$openlow > 0,] ,rownames = F,
                class   = 'cell-border stripe compact hover',
                escape  = F, selection = 'multiple',
                options = list(columnDefs = list(list(width = '100px', targets = "_all")),
                               scrollX = TRUE))
    }) 
    
    
    output$oprec <- renderPlotly({
      data_table$sam_df_All_filtered()
      p <- ggplot(data_table$sam_df_All_filtered(), aes(x=open, y=Previous_close)) +
        geom_point(aes(colour = ticker)) + xlab("") +
        theme(axis.text.x=element_text(angle=60, hjust=1))
      ggplotly(p)
    })
    
   
    
    output$predicted_open <- renderPrint({
      model <- data_table$sam_df_All_filtered() %>% group_by(ticker) %>% do(model = lm(open ~ Previous_close , data = .))
      model$model[model$ticker %in% input$pre_tic][[1]]$coefficients[[1]] + (input$close_price * model$model[model$ticker %in% input$pre_tic][[1]]$coefficients[[2]])
    })
    
    
    
})
  
  observeEvent(input$start,{
    output$cmp <- renderPlotly({
      invalidateLater(1000)
      xnew <- sample(1:1000, 1)
      # sqlQuery(conn, paste0("insert into [Current_mp] (mp) values ","(", xnew, ")"))
      sqlQuery(conn, paste0("insert into [Current_mp] (mp, Date_time) values (",xnew,",", "CAST(GETDATE() AS TIME",")",")"))
      cmp <- sqlQuery(conn, "select * from [Current_mp] order by Date_time desc")
      datatable(cmp, rownames = F)
      p <- ggplot(cmp, aes(x=Date_time, y=mp, group=1)) +
        geom_line(aes(colour = "red")) + geom_point()+ xlab("") + theme(axis.text.x=element_text(angle=60, hjust=1))
      ggplotly(p)
    })
  })
  

  
  output$tic <- renderUI({
    column(width = 3, selectInput("pre_tic","Ticker",choices = input$com))
  })

  
}

shinyApp(ui, server)