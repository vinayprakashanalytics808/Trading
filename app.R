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
library(rhandsontable)
source("DButils.R")
source("union_100_Data_points.R")
List_500_companies <- read_excel("List_500_companies.xlsx")

ui <- fluidPage(
  fluidRow(column(width = 4, dateInput("from","From",value = Sys.Date()-15)),br()),
  fluidRow(uiOutput("com1")
    # column(width = 4, selectInput("com", "Companies", choices = List_500_companies$symbol, multiple = TRUE))
          ,column(width = 4, radioButtons("sel", "Portfolio", choices = unique(List_500_companies$Selective), selected =  "Others"))
           ),
  actionButton("exe","Update Individual tables"),
  actionButton("exe_top_hundred","Update top 100 tables"),
  br(),br(),
  tabsetPanel(
    tabPanel("DataBase Last 100 records",br(), dataTableOutput("top_hundred")),
    tabPanel("Relation",br(), uiOutput("ret"),actionButton("sd","Calculate")),
    tabPanel("Prediction",br(), 
             fluidRow(
               # column(width = 3, selectInput("tic","Ticker",choices = List_500_companies$symbol)),
               uiOutput("tic"),
               column(width = 3,numericInput("open_price","Open Price",value = 3)), 
               # column(width = 3, br(), actionButton("cal","Calculate")),
               column(width = 3, br(), div(style="width:500px;padding-left:100px;",verbatimTextOutput("predicted_high")))
             ),
             fluidRow(
               uiOutput("tic1"),
               column(width = 3,numericInput("prev_relation","Relation",value = 3)), 
               column(width = 3, br(), div(style="width:500px;padding-left:100px;",verbatimTextOutput("predicted_relation")))
             )),
    tabPanel("DataBase Postitive Open Low",br(), dataTableOutput("positive_openlow")),
    tabPanel("Current Market Price", br(),
             actionButton("start", "Start"), plotlyOutput("cmp")),
    tabPanel("High Vs Open",br(), plotlyOutput("oprec"))
   ),
  br(),
  tabsetPanel(
    tabPanel("Relation",br(),selectInput("phigh", "Predicted High", choices = c("pred_high", "pred_high_2method")), plotlyOutput("rela")),
    tabPanel("high-open",plotlyOutput("hi_op")),
    tabPanel("Open Trend",plotlyOutput("open")),
    tabPanel("Low Trend",plotlyOutput("low")),
    tabPanel("High Trend",plotlyOutput("high")),
    tabPanel("Close Trend",plotlyOutput("close"))
  )
)

server <- function(input, output, session) {
  
  data_table <- reactiveValues()
  x <- reactiveVal()
  values <- reactiveValues(data = data.frame(Description = c("INR","Price Per Share","High"),val = c(0,0,0)))
  
  
  ## List_500_companies.xlsx has the list of companies that is in scope. The user is selecting based on his preference
  selected_portfolio <- reactive({
    List_500_companies$symbol[List_500_companies$Selective == input$sel]
  })
  

  output$com1 <- renderUI({
    column(width = 4, selectInput(inputId = "com",label = "Companies", choices = as.character(selected_portfolio()),multiple = TRUE))
  })
  
  observe({
    if(!is.null(input$table))
      values$data <- hot_to_r(input$table)
  })
  
  output$table <- renderRHandsontable({
    rhandsontable(values$data)
  })
  
  

  ## Trigget Update Individual tables post selecting the company
  observeEvent(input$exe,{
    
    
    ## if none of the companies are selected then shoot a message
    if(is.null(input$com)){
      showModal(modalDialog(
        title = "Note",
        "Please select atleast one company"
      ))
    } else {
      
      ## Error to handle app from crashing (Error Handling)
      error_message <- function() 'Free plan is limited to US stocks only please visit our subscription page to upgrade your plan at https://financialmodelingprep.com/developer/docs/pricing'
      tryCatch(
        {
          ini_time <- Sys.time()
          stock_data <- NULL
          data_ss <- NULL
          cast.stock <- NULL
          from_Date <- input$from
          data_ss <- NULL
          ini_num <- List_500_companies$SL_no[List_500_companies$symbol %in% unlist(strsplit(paste0(input$com,collapse = ","),','))]
          ini_company_list_num <- c(ini_num)
          asd1 <- as.data.frame(List_500_companies[ini_company_list_num,])
          company_list_num <- c(1: length(ini_num))
          
          com <- c()
          for (j in company_list_num){
            # com <- c(com, paste0(List_500_companies$symbol[j], collapse=","))
            com[j] <- paste0(asd1$symbol[j], collapse=",")
            
            ## Getting the stock prices from Financial Modeling Prep
            data_ss[[j]] <- rjson::fromJSON(file = 
                                              paste0("https://fmpcloud.io/api/v3/historical-price-full/",com[j],"?from=",from_Date,"&to=",
                                                     Sys.Date(),"&apikey=9021664c6f29c04aa567b7e637a3c70c"))
            
            ## old api key : 9021664c6f29c04aa567b7e637a3c70c
            
            
            
            if(length(data_ss[[j]]) > 0){
              data_ss[[j]] <- data_ss[[j]]
            } else {
              data_ss[[j]] <- fromJSON(file = "empty.json")
            }
            print("Vinay")
            print(paste0("https://fmpcloud.io/api/v3/historical-price-full/",com[j],"?from=",from_Date,"&to=",
                         Sys.Date(),"&apikey=d044d11c5bbbc7c89697083850466e50"))
            print("Vinay")
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
        },
        error = function(e){
          showModal(modalDialog(
            error_message()
          ))
        }
      )
    # shinyalert("Oops!", "Something went wrong.", type = "error")
    }
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
  
  
  
  ## To update top 100 values/data points to the table
  observeEvent(input$exe_top_hundred,{
    
    if(is.null(input$com)){
      showModal(modalDialog(
        title = "Note",
        "Please select atleast one company"
      ))
      # showModal(ui = "Please select atleast one company")
    } else {
      
    
    
    sam_df_All <- update_top_hundred_tables()
    sam_df_All <- sam_df_All %>% distinct(ticker, date, .keep_all=TRUE)
    sam_df_All <- sam_df_All[!is.na(sam_df_All$openlow), ]
    sam_df_All <- as.data.frame(sam_df_All %>% group_by(ticker) %>% mutate(Previous_close = dplyr::lead(close, n = 1, default = NA)))
    sam_df_All <- sam_df_All[complete.cases(sam_df_All), ]
    sam_df_All <- sam_df_All %>% group_by(ticker) %>% mutate(pred_high = fitted(lm(high ~ open))) 
    sam_df_All$`High-pred_high` <- sam_df_All$high - sam_df_All$pred_high
    sam_df_All <- sam_df_All %>% group_by(ticker) %>% mutate(`Std_High-pred_high` = sd(`High-pred_high`))
    sam_df_All$relation_based_on_open <- sam_df_All$open / (sam_df_All$`Std_High-pred_high` * sam_df_All$pred_high)
    sam_df_All$based_on_high <- sam_df_All$relation_based_on_open / sam_df_All$high
    sam_df_All <- sam_df_All %>% group_by(ticker) %>% mutate(lead_based_on_high = dplyr::lead(based_on_high, n = 1, default = NA))
    # sam_df_All <- sam_df_All %>% mutate(lead_based_on_high = as.numeric(format(lead_based_on_high, scientific = FALSE, big.mark = ",")),
    #                                     based_on_high = as.numeric(format(based_on_high, scientific = FALSE, big.mark = ",")))
    sam_df_All <- sam_df_All %>% filter(!is.na(lead_based_on_high)) %>% group_by(ticker) %>% mutate(pred_based_on_high = fitted(lm(based_on_high ~ lead_based_on_high))) 
    sam_df_All$pred_high_2method <- sam_df_All$relation_based_on_open / sam_df_All$pred_based_on_high
    sam_df_All <- sam_df_All %>% group_by(ticker) %>% mutate(open_nor = (open - min(open)) / (max(open) - min(open)))
    sam_df_All$`high-open` <- sam_df_All$high - sam_df_All$open
    sam_df_All <- sam_df_All %>% group_by(ticker) %>% mutate(`high-open_nor` = (`high-open` - min(`high-open`)) / (max(`high-open`) - min(`high-open`)))
    sam_df_All <- sam_df_All[c(1,2,7,9,11,12,20,21,22,23,24,25,26,27,28,29,30,3:6,8,10,13:19)]
    sam_df_All_positive <- sam_df_All %>% group_by(ticker) %>% summarise(max_date = max(date))
    # View(sam_df_All_positive)
    sam_df_All_positive$openlow <- sam_df_All$openlow[match(sam_df_All_positive$ticker, sam_df_All$ticker)]
    data_table$sam_df_All_filtered <-  function(){
      sam_df_All[sam_df_All$ticker %in% unlist(strsplit(paste0(input$com,collapse = ","),',')),]
    }
    
    output$top_hundred <- renderDataTable(server = FALSE, {
    data_table$sam_df_All_filtered()
      
    datatable(data_table$sam_df_All_filtered() ,rownames = F,
              class   = "compact nowrap hover row-border",
              escape  = F ,extensions = 'Buttons',
              options = list(paging = FALSE, pageLength = nrow(data_table$sam_df_All_filtered()), scrollY = "400px", autoWidth = TRUE,columnDefs = list(list(width = '150px', targets = "_all")
                                                                ,list(visible=FALSE, targets=c(7:9,17:29))
                                                                ),
                             scrollX = TRUE, 
                             dom = "tB",
                             buttons = c('copy', 'csv', 'excel')
                             ))
    })       
    

    output$ret <- renderUI({
      data_table$sam_df_All_filtered()
      # asd <- data.frame(Companies = input$com)
      # asd <- as.data.frame(data_table$sam_df_All_filtered() %>% filter(!is.na(lead_based_on_high)) %>% group_by(ticker) %>% summarise(open = first(open), pred_high = first(pred_high), `relation_based_on_open / pred_high / STD` = mean(relation_based_on_open), STD = mean(`Std_High-pred_high`)
      #                                                                                          ,correlation = cor(based_on_high, lead_based_on_high)
      #                                                                                          ))
      asd <- as.data.frame(data_table$sam_df_All_filtered() %>% filter(!is.na(lead_based_on_high)) %>% 
                             group_by(ticker) %>% 
                             summarise(variation = (sum(pred_high) - sum(high)) * 100 /sum(high), open = first(open), pred_high = first(pred_high),STD = first(`Std_High-pred_high`), 
                                       `relation_based_on_open / pred_high / STD` = open / (pred_high * STD), 
                                       correlation = cor(based_on_high, lead_based_on_high),
                                       based_on_high = first(based_on_high), high = `relation_based_on_open / pred_high / STD` / based_on_high))
      values <<- reactiveVal(value = asd)
      output$aa <- renderRHandsontable({
        values() %>% mutate(`relation_based_on_open / pred_high / STD` = open / (pred_high * STD),
                            high = `relation_based_on_open / pred_high / STD` / based_on_high
                            ) %>% rhandsontable(digits = 25) %>% hot_cols(format = "0.0000000000000000000000000")
      })
      rHandsontableOutput("aa",width = 1300)
      })
    
    observeEvent(input$sd,{
      if(!is.null(input$aa))
        values(hot_to_r(input$aa))
    })


    output$rela <- renderPlotly({
      data_table$sam_df_All_filtered()
      # sam_df_All_filtered <-  sam_df_All[sam_df_All$ticker %in% unlist(strsplit(paste0(input$com,collapse = ","),',')),]
      p <- ggplot(data_table$sam_df_All_filtered(), aes(x=date, y=high)) +
        geom_line(aes(colour = ticker)) + geom_line(mapping = aes_string(x = "date", y = input$phigh, colour = "ticker"))
        xlab("") +
        theme(axis.text.x=element_text(angle=60, hjust=1))
        ggplotly(p)
    })
    
    output$hi_op <- renderPlotly({
      data_table$sam_df_All_filtered()
      p <- ggplot(data_table$sam_df_All_filtered(), aes(x=date, y=`high-open_nor`)) +
        geom_line(aes(colour = ticker)) + geom_point(aes(y=`high-open_nor`)) +
        xlab("") +
        theme(axis.text.x=element_text(angle=60, hjust=1))
      ggplotly(p)
    })
    
    # output$hi_op <- renderPrint({
    #   # class(data_table$sam_df_All_filtered())
    #   cor(unstack(as.dataframe(data_table$sam_df_All_filtered())[c("high-open_nor", "ticker")]))
    #   # cor(unstack(iris[c("Sepal.Length", "Species")]))
    # })
    
    output$open <- renderPlotly({
      data_table$sam_df_All_filtered()
      # sam_df_All_filtered <-  sam_df_All[sam_df_All$ticker %in% unlist(strsplit(paste0(input$com,collapse = ","),',')),]
        p <- ggplot(data_table$sam_df_All_filtered(), aes(x=date, y=open_nor)) +
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
      p <- ggplot(data_table$sam_df_All_filtered(), aes(x=high, y=open)) +
        geom_point(aes(colour = ticker)) + xlab("") +
        theme(axis.text.x=element_text(angle=60, hjust=1))
      ggplotly(p)
    })
    
   
    
    output$predicted_high <- renderPrint({
      # model <- data_table$sam_df_All_filtered() %>% group_by(ticker) %>% do(model = lm(open ~ Previous_close , data = .))
      model <- data_table$sam_df_All_filtered() %>% group_by(ticker) %>% do(model = lm(high ~ open , data = .))
      paste0("Predicted High : ",format(model$model[model$ticker %in% input$pre_tic][[1]]$coefficients[[1]] + (input$open_price * model$model[model$ticker %in% input$pre_tic][[1]]$coefficients[[2]]),scientific = F,digits = 20))
    })

    output$predicted_relation <- renderPrint({
      model1 <- data_table$sam_df_All_filtered() %>% group_by(ticker) %>% do(model = lm(based_on_high ~ lead_based_on_high , data = .))
      paste0("Predicted Relation : ",format(model1$model[model1$ticker %in% input$pre_tic1][[1]]$coefficients[[1]] + (input$prev_relation * model1$model[model1$ticker %in% input$pre_tic1][[1]]$coefficients[[2]]),scientific = F,digits = 20))
    })
    }    
    
})
  
  observeEvent(input$start,{
    output$cmp <- renderPlotly({
      invalidateLater(1000)
      # xnew <- sample(1:1000, 1)
      company_cmp <- rjson::fromJSON(file = "https://financialmodelingprep.com/api/v3/quote-short/VENKEYS.NS?apikey=d044d11c5bbbc7c89697083850466e50")
      xnew <- company_cmp[[1]]$price
      # sqlQuery(conn, paste0("insert into [Current_mp] (mp) values ","(", xnew, ")"))
      # print(paste0("insert into [Current_mp] (mp, Date_time,[date], company) values (",xnew,",", "CAST(GETDATE() AS TIME",")",",convert(date, GETDATE(), 101),'c')"))
      sqlQuery(conn, paste0("insert into [Current_mp] (mp, Date_time,[date], company) values (",xnew,",", "CAST(GETDATE() AS TIME",")",",convert(date, GETDATE(), 101),'c')"))
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

  output$tic1 <- renderUI({
    column(width = 3, selectInput("pre_tic1","Ticker",choices = input$com))
  })
  
}

shinyApp(ui, server)