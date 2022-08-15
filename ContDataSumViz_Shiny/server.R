#
# This is the server logic of ContDataSumViz Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

##if(!require(ContDataQC)){source("./install_packages_for_app.R")}  #install if not yet

library("readxl")        # to read excel files
library("writexl")
library("data.table")
library("DT")
library("tidyverse")
library("tibbletime")
library("shiny")
library("shinydashboard")
library("shinyjs")
library("shinyBS")
library("shinythemes")
library("shinyalert")
library("conflicted")
library("dataRetrieval")
library("doBy")
library("knitr")
library("htmltools")
library("rmarkdown")
library("highr")
library("survival")
library("shinyFiles")
library("plotly")
library("zip")
library("reshape2")
library("ContDataQC")
library("StreamThermal")
library("shinycssloaders")# EWL

source("./import_raw_data.R")
source("./update_ContDataQC/config.R")
source("./update_ContDataQC/CompSiteCDF.updated.R")
source("./update_ContDataQC/SumStats.updated.R")
source("./update_ContDataQC/ReportMetaData.R")

options(shiny.maxRequestSize = 100*1024^2)

function(input, output, session) {
  
  useShinyjs()
  conflict_prefer("box", "shinydashboard")
  conflict_prefer("dataTableOutput", "DT")
  loaded_data <- reactiveValues()
  raw_data_columns<-reactiveValues()
  selected_to_plot <- reactiveValues(all_selected=data.frame())
  processed <- reactiveValues(processed_dailyStats=list(),ST.freq=data.frame(),
                              ST.mag=data.frame(),ST.roc=data.frame(),ST.tim=data.frame(),ST.var=data.frame())
  
  ######################################## server code for tabPanel "Upload Data"##############################
  
  if (file.exists("File_Format.rds")) file.remove("File_Format.rds")
  do.call(file.remove, list(list.files("Selected_Files", full.names = TRUE)))
  
  
  
  uploaded_data<-eventReactive(c(input$uploaded_data_file),{
                                loaded_data$name <- input$uploaded_data_file$name
                               if(grepl("csv$",input$uploaded_data_file$datapath)){
                                 my_data<-import_raw_data(input$uploaded_data_file$datapath,"csv",has_header=TRUE)
                               }else if(grepl("xlsx$",input$uploaded_data_file$datapath)){
                                 my_data<-import_raw_data(input$uploaded_data_file$datapath,"xlsx",has_header=TRUE) 
                               }else{
                                 shinyalert("Warning","not valid data format",closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                                            confirmButtonText="OK",inputId = "alert_data_not_valid")
                               }
                                 my_data
                               })
  observeEvent(input$alert_data_not_valid,{
    shinyjs::runjs("swal.close();")
  })
  
  ## Copy uploaded files to local folder
  observeEvent(input$uploadId,{
    
    ## this part is for copying multiple files and saving them in a reactive datasetlist()
    # if (!file.exists("./Selected_Files")) dir.create(file.path("./","Selected_Files"),showWarnings = FALSE, recursive = TRUE)
    # #print(is.null(input$uploaded_data_file))
    # if (is.null(input$uploaded_data_file) ) {    return(NULL)  }  
    # file.copy(from = input$uploaded_data_file$datapath, to =  paste0('Selected_Files/',input$uploaded_data_file$name )  )
    # df <- list(file = input$uploaded_data_file$name , header= TRUE,
    #            sep = ",",dec = input$dec,
    #            quote = '',
    #            index = input$uploadId)
    # if(input$uploadId > 1){
    #   old_df <- readRDS("File_Format.rds")
    #   df <- sapply(names(old_df),function(n){c(old_df[[n]],df[[n]])},simplify=FALSE)
    # }
    # saveRDS2 <- function(object,file){str(object);saveRDS(object,file)}
    # saveRDS2(df, "File_Format.rds")
    ## update the line choices in the raw time series plot
    my_data <- uploaded_data()
    
    output$display_raw_ts <- renderUI({
      
      if (length(my_data) > 0 ) {
        ## this part is to find any column name related to Date or Time
        all_date_related_keys <- c("Date.Time","DATE.TIME","Year","YEAR","Date","DATE","MonthDay","MONTHDAY","Time","TIME","Month","Day","RAW.Date.Time")
        date_keys_in_favor_order <- c("Date.Time","DATE.TIME","Year","YEAR","Date","DATE","MonthDay")
        my_colnames <- colnames(my_data)
        
        ## get possible date columns in order according to "date_keys_in_favor_order"
        possible_date_columns <- date_keys_in_favor_order[date_keys_in_favor_order %in% my_colnames]
        all_date_columns <- all_date_related_keys[all_date_related_keys %in% my_colnames]
        
        print(possible_date_columns)
        
        ## send out alert message if the date column cannot be identified
        alert_message_no_date_column = paste0("We assume the dataset you uploaded contains at lease one date time column, but no date time column is identified, please check.")
        if (identical(length(possible_date_columns),integer(0))){
          print("inside shinyalert loop now...")
          shinyalert("Warning",alert_message_no_date_column,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                     confirmButtonText="OK",inputId = "alert_no_date")
        }else{
          raw_data_columns$date_column_name <- possible_date_columns[1]
        }
        ## this part is to find any column name related to "ID" or "Flag"
        idx_no_ID_Flag <- !str_detect(my_colnames,"ID") & !str_detect(my_colnames,"SITE") & !str_detect(my_colnames,"Flag") & !str_detect(my_colnames,"Comment")
        not_ID_or_Flag_cols <- my_colnames[idx_no_ID_Flag]
        parameters_cols_best_guess <- not_ID_or_Flag_cols[!not_ID_or_Flag_cols %in% all_date_columns]
        print(parameters_cols_best_guess)
        
        sidebarLayout(
          sidebarPanel(h4(id="big-heading","Plot Raw Data"),width=2,
                       div(style="display: inline-block;vertical-align:top; width: 95%;",selectInput("line1_1sec",label="","choose a column")),  
                       div(style="display: inline-block;vertical-align:top; width: 95%;",selectInput("line2_1sec",label="","choose a column")), 
                       div(style="display: inline-block;vertical-align:top; width: 95%;",selectInput("line3_1sec",label="","choose a column")), 
                       div(style="display: inline-block;vertical-align:top; width: 95%;",selectInput("line4_1sec",label="","choose a column")),  
                       hr(),
                       radioButtons("raw_datetime_format", "Select datetime format", choices = c("%Y-%m-%d %H:%M:%S"="%Y-%m-%d %H:%M:%S",
                                                                                                 "%Y-%m-%d %H:%M"="%Y-%m-%d %H:%M",
                                                                                                 "%d-%m-%Y %H:%M:%S"="%d-%m-%Y %H:%M:%S",
                                                                                                 "%Y-%m-%d"="%Y-%m-%d",
                                                                                                 "%d-%m-%Y"="%d-%m-%Y",
                                                                                                 "%m%d"="%m%d"),
                                    selected = "%Y-%m-%d %H:%M:%S"),
                       hr(),
                       actionButton(inputId="showrawTS", label="Display time series",style="color:cornflowerblue;background-color:black;font-weight:bold"),
                       hr(),
                       selectizeInput("parameters_to_process",label ="Select parameters to process",
                                      choices=parameters_cols_best_guess,
                                      multiple = TRUE,
                                      options = list(hideSelected = FALSE,plugins=list('remove_button'))
                                      ), # selectizeInput close
                       br(),
                       actionButton(inputId="runQS", label="Run meta summary",style="color:cornflowerblue;background-color:black;font-weight:bold"),
                       
          ), # sidebarPanel close
          mainPanel(width=10,
                    tags$head(tags$style(HTML(".radio-inline {margin-left: 10px;}"))),
                    tags$head(tags$style("#calculateDailyStatistics,#saveDailyStatistics{
                                                                                    font-size: 14px;
                                                                                    }
                                                                                    ")),
                    tags$head(tags$style(HTML("#quick_summary_table {
                                              text-align: center;
                                              font-size: 16px;
                                              font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;}
                                              "))),
                    tags$head(tags$style("#quick_summary_table_footnote{
                                        text-align:center;
                                        color: black;
                                        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
                                        font-size: 16px;
                                        font-weight: bold;}
                                        ")),
                    fluidRow(column(width=12,uiOutput("display_all_raw_ts"),
                                    hr()
                                     ), # column close
                                     ), #fluidRow end
                    fluidRow(column(width=12,uiOutput("display_quick_summary_table"),
                                     uiOutput("display_footnote_text")), # column close
                                     ), #fluidRow end
                    
                            fluidRow(column(width=8,style="padding:20px;",
                                                    uiOutput("display_radioButtons_dailyStats_1"),
                                                    uiOutput("display_radioButtons_dailyStats_2")
                                           ), # column close
                                     column(width=4,style="padding:25px;",uiOutput("display_actionButton_calculateDailyStatistics")
                                           ), # column close
                                     column(width=4,div(style="margin-bottom:20px")),
                                     column(width=4,style="padding:30px;",uiOutput("display_actionButton_saveDailyStatistics")
                                     ), # column close
                                   ) #fluidRow end
                                   ) # mainPanel end
        ) # sidebarLayout end
        
      }
      
    })
    
    my_data_continuous <- my_data %>% select(where(is.numeric))
    all_continuous_col_names <- colnames(my_data_continuous)
   
    updateSelectInput(session,"line1_1sec",choices = c("",all_continuous_col_names),selected=NULL) 
    updateSelectInput(session,"line2_1sec",choices = c("",all_continuous_col_names),selected=NULL) 
    updateSelectInput(session,"line3_1sec",choices = c("",all_continuous_col_names),selected=NULL) 
    updateSelectInput(session,"line4_1sec",choices = c("",all_continuous_col_names),selected=NULL) 
    updateSelectInput(session,"line5_1sec",choices = c("",all_continuous_col_names),selected=NULL) 
    updateSelectInput(session,"line6_1sec",choices = c("",all_continuous_col_names),selected=NULL) 
    
  })  # observeEvent end
  
  observeEvent(input$alert_no_date,{
    #print(input$alert_no_date)
    shinyjs::runjs("swal.close();")
  })
  
  ## Load all the uploaded files to a list, this feature will be activated in the future
  # datasetlist <- eventReactive(input$uploadId,{
  #   
  #   Selected_Files <- list.files("Selected_Files/")
  #   Sys.sleep(2)
  #   File_Format <- readRDS("File_Format.rds")
  #   datalist <- list()
  #   datalist <- lapply(1:length(File_Format[[1]]), function(d) read.csv(paste0("Selected_Files/",File_Format$file[d] ),
  #                                                                       header = File_Format$header[d],
  #                                                                       sep = File_Format$sep[d],
  #                                                                       dec = File_Format$dec[d],
  #                                                                       check.names = FALSE,
  #                                                                       quote = File_Format$quote[d]))
  #   names(datalist) <- paste(File_Format$index, File_Format$file,sep = ". ")
  #   return(datalist)
  # 
  # })
  
  output$manage <- renderUI({
    data <- uploaded_data() ## datasetlist()
    print(length(data))
    selectInput("dataset", "Dataset", choices = loaded_data$name, selected = loaded_data$name)  ## names(data) if use datasetlist()
   
  })
  
  output$siteType <- renderUI({
    data <- uploaded_data() ## datasetlist()
    selectInput("siteType_input",label="Single site or multiple sites",
                choices = c("Single site","Multiple sites"),
                selected = "Single site")
  })
  
  output$select <- renderUI({
    data <- uploaded_data() ## datasetlist()
    radioButtons("disp", "Display", choices = c(Head = "head",Tail="tail",ColumnNames="Column names"),
                 selected = "head")
  })
  
  output$display_button <- renderUI({
    data <- uploaded_data() ## datasetlist()
    if (length(data) > 0 ) {
    actionButton(inputId = "displayid",label = "Display file contents",style="color:cornflowerblue;background-color:black;font-weight:bold")
    }
    
  })
  
  observeEvent(input$displayid, {
    
    output$contents <- renderTable({
      
     # data <- datasetlist()
     # sub_df <- data[[paste0(input$dataset)]]
      sub_df <- uploaded_data()
      if (isolate(input$disp == "head")) {
        return(head(sub_df))
      }
      else if (isolate(input$disp == "tail")) {
        return(tail(sub_df))
      } else {
        return(colnames(sub_df))
      }
    },type="html",bordered = TRUE,striped=TRUE,align="c")
  })
  
  myQuickSummary <- function(myDf){
    all.days <- seq.Date(min(myDf$Date),max(myDf$Date),by="day")
    N.missing.days <- (length(all.days)-sum(all.days %in% myDf$Date))+sum(myDf$sumNA>0)
    if (ncol(myDf)>2){
    N.days.flagged.fail <- sum(myDf$sumFail>0)
    N.days.flagged.suspect <- sum(myDf$sumSuspect>0)
    }else{
      N.days.flagged.fail <- "No flag field found"
      N.days.flagged.suspect <- "No flag field found"
    }
    mySummary <- c(N.missing.days,N.days.flagged.fail,N.days.flagged.suspect)
    return(mySummary)
  }
  
  observeEvent(input$showrawTS,{
    
    output$display_all_raw_ts <- renderUI({
      withSpinner(plotlyOutput(outputId="all_raw_ts",width="90%",height="500px"))
    })
    
    isolate(input$raw_datetime_format)
    raw_data <- uploaded_data()
    ## gather all the columns were selected for time series
    my_raw_choices = c(input$line1_1sec,input$line2_1sec,input$line3_1sec,input$line4_1sec,input$line5_1sec,input$line6_1sec)
    ## remove those choices not initiated 
    my_raw_choices = my_raw_choices[!grepl("choose a column",my_raw_choices)]
    print(my_raw_choices)
    print(is.null(my_raw_choices))
    if (!is.null(my_raw_choices)){
     all_raw_selected =data.frame(cbind(raw_data[,input$line1_1sec==colnames(raw_data)],raw_data[,input$line2_1sec==colnames(raw_data)],
                                     raw_data[,input$line3_1sec==colnames(raw_data)],raw_data[,input$line4_1sec==colnames(raw_data)]
                                     ))
      
      colnames(all_raw_selected) <- my_raw_choices
      raw_data_columns$to_plot_raw_ts <- my_raw_choices
      
      all_raw_selected$TimeStamp <- raw_data[,(names(raw_data) %in% raw_data_columns$date_column_name)]
      
     ## save(all_raw_selected,file="./test_all_raw_selected.RData")
      
      all_raw_selected <- all_raw_selected[!duplicated(as.list(all_raw_selected))]
      
     
      if (input$raw_datetime_format=="%m%d" & is.null(input$get_the_year)){
        alert_message_to_get_year = "The date/time in this file only provides month and day, please provide the year"
        
        shinyalert("",alert_message_to_get_year,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                   confirmButtonText="Submit",inputId = "get_the_year",type="input")
      } # the first if end
      
    } 
    
    output$all_raw_ts <- renderPlotly({
      
      if (ncol(all_raw_selected)>1) {
        x_date_label = "%Y-%m"
        print(input$raw_datetime_format)
        myBreaks = paste0(2," months")
        
        all_raw_selected_to_plot <- reshape2::melt(all_raw_selected,"TimeStamp")
        ##save(all_raw_selected_to_plot,file="./test_selected_raw_data.RData")
          
          if (input$raw_datetime_format=="%m%d" & !is.null(input$get_the_year)){
            print(input$get_the_year)
            all_raw_selected_to_plot$TimeStamp = paste0(input$get_the_year,"-0",substr(all_raw_selected_to_plot$TimeStamp,1,1),"-",substr(all_raw_selected_to_plot$TimeStamp,2,3))
            my_datetime_format = "%Y-%m-%d"
            all_raw_ts_plot <- ggplot(data=all_raw_selected_to_plot)+
              geom_point(mapping = aes(x=as.POSIXct(TimeStamp,format=my_datetime_format),y=value,color=variable),size=0.5)+
              labs(x="Date",y=paste0(" "))+
              scale_x_datetime(date_labels=x_date_label,date_breaks="2 months")+
              theme(text=element_text(size=14,face = "bold", color="cornflowerblue"),
                    axis.text.x=element_text(angle=45, hjust=1), panel.grid.major.y=element_blank())
            all_raw_ts_plot <- ggplotly(all_raw_ts_plot,height=500,width=1200,dynamicTicks = TRUE)
           
          }else if(input$raw_datetime_format!="%m%d"){
            print("inside else loop now...")
            all_raw_ts_plot <- ggplot(data=all_raw_selected_to_plot)+
            geom_point(mapping = aes(x=as.POSIXct(TimeStamp,format=paste0(input$raw_datetime_format)),y=value,color=variable),size=0.5)+
            labs(x="Date",y=paste0(" "))+
            scale_x_datetime(date_labels=x_date_label,date_breaks="2 months")+
            theme(text=element_text(size=14,face = "bold", color="cornflowerblue"),
                  axis.text.x=element_text(angle=45, hjust=1), panel.grid.major.y=element_blank())
            all_raw_ts_plot <- ggplotly(all_raw_ts_plot,height=500,width=1200,dynamicTicks = TRUE)
          
          }  ## else end
    } ## outer if loop end 
    })  ## renderPlot end
  })  ## observeEvent end 
  
  observeEvent(input$runQS,{
    
    raw_data <- uploaded_data()
    save(raw_data,file="./test_raw_data.RData")
    output$display_quick_summary_table <- renderUI({
      column(12,align="center",withSpinner(tableOutput("quick_summary_table")))
    })
    
    output$display_footnote_text <- renderUI({
     verbatimTextOutput("quick_summary_table_footnote")
    })
    
    ## create a quick metadata summary regarding the raw data file
    dailyCheck <- ReportMetaData(fun.myFile=NULL
                                 ,fun.myDir.import=NULL
                                 ,fun.myParam.Name=input$parameters_to_process
                                 ,fun.myDateTime.Name=raw_data_columns$date_column_name
                                 ,fun.myDateTime.Format=input$raw_datetime_format
                                 ,fun.myThreshold=20
                                 ,fun.myConfig=""
                                 ,df.input=raw_data
    )
    save(dailyCheck, file="./test_dailyCheck.RData")
    
    getQuickSummary <- lapply(dailyCheck,myQuickSummary)
    
    toReport <- as.data.frame(matrix(nrow=length(dailyCheck),ncol=4))
    colnames(toReport) <- c("Parameters","Number of days with missing data","Number of days with data flagged as fail","Number of days with data flagged as suspect")
    toReport$Parameters <- names(dailyCheck)
    for (n in 1:length(dailyCheck)){
      toReport[n,2:4] <-getQuickSummary[[n]]
    }
    
    output$quick_summary_table <- renderTable({
      toReport
      
    },align="c") # #renderTable end
    
    date_column <- raw_data[,raw_data_columns$date_column_name]
    max_date <- max(as.POSIXct(date_column,format=input$raw_datetime_format),na.rm = TRUE)
    min_date <- min(as.POSIXct(date_column,format=input$raw_datetime_format),na.rm = TRUE)
    total_N_days <- as.integer(difftime(max_date,min_date,units="days"))
    
    
    if (is.na(total_N_days)){
      shinyalert("Warning","the selected datetime format does not match what's in the data file, please check.",
                 closeOnClickOutside = TRUE,closeOnEsc = TRUE,confirmButtonText="OK",inputId = "alert_datatime_format")
    }
    
    output$quick_summary_table_footnote <- renderText({
      Note_text_line1= paste0("Period of record: ",min_date," to ", max_date)
      Note_text_line2= paste0("Total number of days in this period: ",total_N_days," days")
      paste(Note_text_line1,Note_text_line2,sep="\n")
    })
    
    check_no_flags <- all(toReport[,3]=="No flag field found") && all(toReport[,4]=="No flag field found")
    print(paste0("check flags is:",check_no_flags))
    
    if (!check_no_flags){
    output$display_radioButtons_dailyStats_1 <- renderUI({
      radioButtons("exclude_flagged"
                   ,"Select data points to be excluded"
                   ,choices = c("fail"="fail","suspect"="suspect","not available"="not available")
                   ,selected = "fail"
                   ,inline=TRUE)
    }) # renderUI close
    } # if loop close
    
    output$display_radioButtons_dailyStats_2 <- renderUI({
      radioButtons("how_to_save"
                   ,"How to save daily statistics"
                   ,choices = c("Per site Per parameter"="save1","Per site with all parameters"="save2","Multiple sites together"="save3")
                   ,selected = "save2"
                   ,inline=FALSE)
    })
    
    output$display_actionButton_calculateDailyStatistics <- renderUI({
      actionButton(inputId="calculateDailyStatistics"
                   ,label="Calculate daily statistics"
                   ,style="color:cornflowerblue;background-color:black;font-weight:bold")
    })
    
    output$display_actionButton_saveDailyStatistics <- renderUI({
    actionButton(inputId="saveDailyStatistics"
                   ,label="Save daily statistics"
                   ,style="color:cornflowerblue;background-color:black;font-weight:bold;padding-left:15px;padding-right:15px;")
    })
    
  })  ## observeEvent end 
  
  ## close the warning message inside the above oberveEvent
  
  observeEvent(input$alert_datatime_format,{
    shinyjs::runjs("swal.close();")
  })
  
  
  observeEvent(input$get_the_year,{
    #print(input$alert_no_date)
    shinyjs::runjs("swal.close();")
  })
  
  ### when user clicked actionButton "calculateDailyStatistics"
  
  observeEvent(input$calculateDailyStatistics,{
    
    raw_data <- uploaded_data()
    showModal(modalDialog("Calculating the daily statistics now...",footer=NULL))
    
    variables_to_calculate <- input$parameters_to_process
    
    dailyStats <- SumStats.updated(fun.myFile=NULL
                                   ,fun.myDir.import=NULL
                                   ,fun.myParam.Name=variables_to_calculate
                                   ,fun.myDateTime.Name=raw_data_columns$date_column_name
                                   ,fun.myDateTime.Format=input$raw_datetime_format
                                   ,fun.myThreshold=20
                                   ,fun.myConfig=""
                                   ,df.input=raw_data
                                   )
    
    save(dailyStats, file="./test_dailyStats.RData")
    processed$processed_dailyStats <- dailyStats
    
    removeModal()
    
  })
  
  
  ### when user clicked actionButton "saveDailyStatistics"
  observeEvent(input$saveDailyStatistics,{
    
    if (!file.exists("./Output")) dir.create(file.path("./","Output"),showWarnings = FALSE, recursive = TRUE)
    name_in_file <- loaded_data$name
    if (endsWith(loaded_data$name,".csv")) name_in_file <- sub(".csv$","",loaded_data$name)
    if (endsWith(loaded_data$name,".xlsx")) name_in_file <- sub(".xlsx$","",loaded_data$name)
    
    if (input$how_to_save == "save2"){
       filename = paste0("./Output/",name_in_file,"_dailyStats.csv")
       combined_data <- Reduce(full_join,processed$processed_dailyStats)
       write.csv(combined_data,file=filename,row.names=FALSE)
    }else if(input$how_to_save == "save1"){
      for (i in 1:length(processed$processed_dailyStats)){
        name_i <- names(processed$processed_dailyStats)[i]
        filename = paste0("./Output/",name_in_file,"_",name_i,"_dailyStats.csv")
        write.csv(processed$processed_dailyStats[[i]],file=filename,row.names=FALSE)
      }
    }
  
  })
  
  
  ############################################################################################################################
  ############################################ server code start for tabPanel "Data Exploration"##############################
  ############################################################################################################################
  observeEvent(input[["tabset"]], {
    
    
    ############ set up the UI for subtab "summary table" << All parameters ############
    
    output$summary_table_input_1 <- renderUI({
      variables_avail <- names(processed$processed_dailyStats)
      selectizeInput("summarise_variable_name",label ="Select variable name",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=variables_avail[1],
                     options = list(hideSelected = FALSE))
    })
    
    output$summary_table_input_2 <- renderUI({
      
      radioButtons("summarise_by", "Summarise by", choices = c("year/month"="year/month"
                                                               ,"year"="year"
                                                               ,"year/season"="year/season"
                                                               ,"season"="season"),
                   selected = "year/month")
    })
    
    output$summary_table_input_3 <- renderUI({
      
      selectizeInput("summarise_metrics",label ="Select daily statistics metrics",
                     choices=c("mean","median","min", "max","range","sd","var","cv","n"),
                     multiple = FALSE,
                     selected="mean",
                     options = list(hideSelected = FALSE))
    })
    
    output$summary_table_input_4 <- renderUI({
      actionButton(inputId="display_table", label="Summarise",style="color:cornflowerblue;background-color:black;font-weight:bold")
    })
    
    
    ############ set up the UI for subtab "time series plot" << All parameters ############
    
    output$time_series_input_1 <- renderUI({
      variables_avail <- names(processed$processed_dailyStats)
      selectizeInput("dailyStats_ts_variable_name",label ="Select variable name",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=variables_avail[1],
                     options = list(hideSelected = FALSE))
    })
    
    output$time_series_input_2 <- renderUI({
      
      selectizeInput("dailyStats_ts_metrics",label ="Select daily statistics metrics",
                     choices=c("mean","median","min", "max","range","sd","var","cv","n"),
                     multiple = FALSE,
                     selected="mean",
                     options = list(hideSelected = FALSE))
    })
    
    output$time_series_input_3 <- renderUI({
      div(br(),
          radioButtons("dailyStats_shading", "Add shading with", choices = c("25th & 75th percentiles"="quantiles",
                                                                           "minimum & maximum"="minMax",
                                                                           "newData"="newData"),
                       selected = "quantiles"))
      
    })
    
    output$time_series_input_4 <- renderUI({
      textInput(inputId="dailyStats_ts_title", label="Plot title",value="")
    })
    
    output$time_series_input_5 <- renderUI({
      actionButton(inputId="display_ts", label="Display",style="color:cornflowerblue;background-color:black;font-weight:bold")
    })
    
    output$time_series_input_6 <- renderUI({
      actionButton(inputId="add_more_ts", label="Add more...")
    })
    
    ############ set up the UI for subtab "time series - annual overlays" << All parameters ############
    
    output$time_series_overlay_input_1 <- renderUI({
      variables_avail <- names(processed$processed_dailyStats)
      selectizeInput("dailyStats_ts_overlay_variable_name",label ="Select variable name",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=variables_avail[1],
                     options = list(hideSelected = FALSE))
    })
    
    output$time_series_overlay_input_2 <- renderUI({
      
      selectizeInput("dailyStats_ts_overlay_metrics",label ="Select daily statistics metrics",
                     choices=c("mean","median","min", "max","range","sd","var","cv","n"),
                     multiple = FALSE,
                     selected="mean",
                     options = list(hideSelected = FALSE))
    })
    
    output$time_series_overlay_input_3 <- renderUI({
      textInput(inputId="dailyStats_ts_overlay_title", label="Plot title",value="")
    })
    
    output$time_series_overlay_input_4 <- renderUI({
          radioButtons("overlay_shading", "Add shading with", choices = c("none"="none"
                                                                          ,"overall averages(all years)"="overall"
                                                                          ,"newData"="newData"),
                       selected = "none")
      
    })
    
    output$time_series_overlay_input_5 <- renderUI({
      actionButton(inputId="display_ts_overlay", label="Display",style="color:cornflowerblue;background-color:black;font-weight:bold")
    })
    
    ############ set up the UI for subtab "box plots" << All parameters ############
    
    output$box_input_1 <- renderUI({
      variables_avail <- names(processed$processed_dailyStats)
      selectizeInput("boxplot_variable_name",label ="Select variable name",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=variables_avail[1],
                     options = list(hideSelected = FALSE))
    })
    
    output$box_input_2 <- renderUI({
      
      selectizeInput("boxplot_metrics",label ="Select daily statistics metrics",
                     choices=c("mean","median","min", "max","range","sd","var","cv","n"),
                     multiple = FALSE,
                     selected="mean",
                     options = list(hideSelected = FALSE))
    })
    
    output$box_input_3 <- renderUI({
      div(br(),
          radioButtons("box_group", "Group by", choices = c("month"="month"
                                                            ,"month(years side by side)"="month2"
                                                            ,"year"="year"
                                                            ,"season"="season"
                                                            ,"season(years side by side)"="season2"),
                       selected = "month"))
      
    })
    
    output$box_input_4 <- renderUI({
      textInput(inputId="box_title", label="Plot title",value="")
    })
    
    
    output$box_input_5 <- renderUI({
      actionButton(inputId="display_box", label="Display",style="color:cornflowerblue;background-color:black;font-weight:bold")
    })
    
    
    ############ set up the UI for subtab "CompSiteCDF" << All parameters ############
    
    output$CDF_input_1 <- renderUI({
      variables_avail <- names(processed$processed_dailyStats)
      selectizeInput("CDF_variable_name",label ="Select variable name",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=variables_avail[1],
                     options = list(hideSelected = FALSE))
    })
    
    output$CDF_input_2 <- renderUI({
      div(br(),
          radioButtons("CDF_shading", "Add shading with", choices = c("25th & 75th percentiles"="quantiles",
                                                                    "minimum & maximum"="minMax",
                                                                    "newData"="newData"),
                       selected = "minMax"))
      
    })
    
    output$CDF_input_3 <- renderUI({
          myList <- processed$processed_dailyStats
          variable_to_plot <- input$CDF_variable_name
          myData.all <- myList[[which(names(myList)==variable_to_plot)]]
          myData.all[,"year"] <- format(myData.all[,"Date"],"%Y")
          
          selectizeInput("CDF_select_year",label ="Select year",
                         choices=c("All", unique(myData.all$year)),
                         multiple = FALSE,
                         selected = "All",
                         options = list(hideSelected = FALSE))
    })
    
    output$CDF_input_4 <- renderUI({
      
      selectizeInput("CDF_select_season",label ="Select season",
                     choices=c("All","Fall", "Winter", "Spring","Summer" ),
                     multiple = FALSE,
                     selected = "All",
                     options = list(hideSelected = FALSE))
    })
    
    output$CDF_input_5 <- renderUI({
    
      textInput(inputId="CDF_title", label="Plot title",value="")
      
    }) 
    
    output$display_CDF_button <- renderUI({
      actionButton(inputId="run_CDF", label="Run and display",style="color:cornflowerblue;background-color:black;font-weight:bold")
    })
    
    ############ set up the UI for subtab "raster graphs" << All parameters ############
    
    output$raster_input_1 <- renderUI({
      variables_avail <- names(processed$processed_dailyStats)
      selectizeInput("dailyStats_raster_variable_name",label ="Select variable name",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=variables_avail[1],
                     options = list(hideSelected = FALSE))
    })
    
    output$raster_input_2 <- renderUI({
      
      selectizeInput("dailyStats_raster_metrics",label ="Select daily statistics metrics",
                     choices=c("mean","median","min", "max","range","sd","var","cv","n"),
                     multiple = FALSE,
                     selected="mean",
                     options = list(hideSelected = FALSE))
    })
    
    output$raster_input_3 <- renderUI({
      textInput(inputId="dailyStats_raster_title", label="Plot title",value="")
      
    })
    
    output$raster_input_4 <- renderUI({
      numericInput(inputId="raster_plot_aspect_ratio", label="Adjust plot aspect ratio",0.5,min=0,max=10,step=0.1)
      
    })
    
    output$raster_input_5 <- renderUI({
      radioButtons(inputId="raster_plot_color",label ="Color palette options",choices=c("hcl","rainbow","heat","terrain","topo"),selected="hcl",inline=FALSE)
    })
    
    output$raster_input_6 <- renderUI({
      actionButton(inputId="run_raster", label="Display",style="color:cornflowerblue;background-color:black;font-weight:bold")
    })
    
    
    
    ############ set up the UI for subtab "Thermal Statistics" << Temperature ############
    output$thermal_input_1 <- renderUI({
      variables_avail <- names(uploaded_data())
      site_keys_in_favor_order <- c("Site","SITE","SiteID","SITEID")
      possible_site_columns <- site_keys_in_favor_order[site_keys_in_favor_order %in% variables_avail]
      if (length(possible_site_columns)==0){
        site_to_select <- variables_avail[grep('site',variables_avail,ignore.case=TRUE)][1]
      }else{
        site_to_select <- possible_site_columns[1]
      }
      selectizeInput("thermal_SiteID_name",label ="Select SiteID Column",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=site_to_select,
                     options = list(hideSelected = FALSE))
    })
    
    output$thermal_input_2 <- renderUI({
      variables_avail <- names(uploaded_data())
      date_keys_in_favor_order <- c("Date.Time","DATE.TIME","Year","YEAR","Date","DATE","MonthDay")
      possible_date_columns <- date_keys_in_favor_order[date_keys_in_favor_order %in% variables_avail]
      selectizeInput("thermal_Date_name",label ="Select Date Column",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=possible_date_columns[1],
                     options = list(hideSelected = FALSE))
    })
    
    output$thermal_input_3 <- renderUI({
      variables_avail <- names(uploaded_data())
      temp_keys_in_favor_order <- c("Water.Temp.C","WATER.TEMP.C","Water_Temp_C",
                                    "WATER_TEMP_C","Air.Temp.C","AIR.TEMP.C","Air_Temp_C","AIR_TEMP_C")
      possible_temp_columns <- temp_keys_in_favor_order[temp_keys_in_favor_order %in% variables_avail]
      if (length(possible_temp_columns)==0){
        temp_to_select <- variables_avail[grep('temp',variables_avail,ignore.case=TRUE)][1]
      }else{
        temp_to_select <- possible_temp_columns[1]
      }
      selectizeInput("thermal_Temp_name",label ="Select Temperature Column",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected= temp_to_select,
                     options = list(hideSelected = FALSE))
    })
    
    output$display_run_thermal_button <- renderUI({
      actionButton(inputId="display_thermal", label="Display streamThermal",style="color:cornflowerblue;background-color:black;font-weight:bold")
    })
    
    output$display_save_thermal_button <- renderUI({
      actionButton(inputId="save_thermal", label="Save thermal statistics to excel",style="color:cornflowerblue;background-color:black;font-weight:bold")
    })
    
    ############ set up the UI for subtab "Air vs water" << Temperature ############
    
    output$air_vs_water_input_1 <- renderUI({
      variables_avail <- names(uploaded_data())
      air_keys_in_favor_order <- c("Air.Temp.C","AIR.TEMP.C","Air_Temp_C","AIR_TEMP_C")
      possible_air_columns <- air_keys_in_favor_order[air_keys_in_favor_order %in% variables_avail]
      if (length(possible_air_columns)==0){
        air_to_select <- variables_avail[grep('air',variables_avail,ignore.case=TRUE)][1]
      }else{
        air_to_select <- possible_air_columns[1]
      }
      selectizeInput("air_temp_name",label ="Select Air Temperature Column",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=air_to_select,
                     options = list(hideSelected = FALSE))
    })
   
    output$air_vs_water_input_2 <- renderUI({
      variables_avail <- names(uploaded_data())
      water_keys_in_favor_order <- c("Water.Temp.C","WATER.TEMP.C","Water_Temp_C","WATER_TEMP_C")
      possible_water_columns <- water_keys_in_favor_order[water_keys_in_favor_order %in% variables_avail]
      if (length(possible_water_columns)==0){
        water_to_select <- variables_avail[grep('water',variables_avail,ignore.case=TRUE)][1]
      }else{
        water_to_select <- possible_water_columns[1]
      }
      selectizeInput("water_temp_name",label ="Select Water Temperature Column",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=water_to_select,
                     options = list(hideSelected = FALSE))
    })
     
    
    output$air_vs_water_input_3 <- renderUI({
      div(br(),
          radioButtons("exclude_data_points", "Exclude data points", choices = c("No"="No","Yes"="Yes"),
                                                                      
                       selected = "No"))
    })
    
    
    air_limit_temp_tooltip_text = paste0("limit the data points with air temperature more than this value (in C).","Default is 0.")
    output$air_vs_water_input_4 <- renderUI({
      tipify(numericInput("air_limit_temp",label ="Limit air temperature >",0,min=-10,max=100,step=1.0),air_limit_temp_tooltip_text,placement="right",trigger="hover")
    })
    
    
    output$display_thermal_sensitivity_button <- renderUI({
      actionButton(inputId="display_thermal_sensitivity", label="Display thermal sensitivity",style="color:cornflowerblue;background-color:black;font-weight:bold")
    })
    
    ############ set up the UI for subtab "IHA" << Hydrology ############
    
    output$IHA_input_1 <- renderUI({
     
    })
    
    
    
    
    
    
    
   
    
    
    
    
    
  }) #observe Event end
  
  
  
  ################# for subtab 1:Summary table << All parameters #################
  
  addSeason <- function(df=myDf){
    df[,"year"] <- format(df[,"Date"],"%Y")
    df[,"monthday"] <- format(df[,"Date"],"%m%d")
    df[,"season"] <- NA
    df[,"season"][as.numeric(df[, "monthday"]) >= as.numeric("0101") & as.numeric(df[
      ,"monthday"])< as.numeric(ContData.env$myTimeFrame.Season.Spring.Start)] <- "Winter"
    df[,"season"][as.numeric(df[,"monthday"]) >= as.numeric(ContData.env$myTimeFrame.Season.Spring.Start) &
                    as.numeric(df[,"monthday"])< as.numeric(ContData.env$myTimeFrame.Season.Summer.Start)] <- "Spring"
    df[,"season"][as.numeric(df[,"monthday"]) >= as.numeric(ContData.env$myTimeFrame.Season.Summer.Start) &
                    as.numeric(df[,"monthday"])< as.numeric(ContData.env$myTimeFrame.Season.Fall.Start)] <- "Summer"
    df[,"season"][as.numeric(df[, "monthday"]) >= as.numeric(ContData.env$myTimeFrame.Season.Fall.Start) &
                    as.numeric(df[,"monthday"])< as.numeric(ContData.env$myTimeFrame.Season.Winter.Start)] <- "Fall"
    df[,"season"][as.numeric(df[, "monthday"]) >= as.numeric(ContData.env$myTimeFrame.Season.Winter.Start) &
                    as.numeric(df[,"monthday"])<= as.numeric("1231")] <- "Winter"
    df[,"yearseason"] <- paste(df[,"year"],df[,"season"], sep="")
    return(df)
  }
  
  mySummarisemore <- function(df=myDf, variable=myVariable,metrics=myMetrics,timeframe=myTimeframe){
       variable_col_name <- paste0(variable,".",metrics)
       names(df)[match(variable_col_name,names(df))] <- "x"
    if (timeframe == "year/month"){
      ## summarise by each year&month first
      df[,"yearmonth"] <- format(df[,"Date"],"%Y%m")
      df.summary <- doBy::summaryBy(x~yearmonth,data=df,FUN=mean,na.rm=TRUE,variable.names=variable)
      df.summary[,"year"] <- substr(df.summary[,"yearmonth"],1,4)
      df.summary[,"month"] <- substr(df.summary[,"yearmonth"],5,6)
      df.summary <- df.summary[2:4]
      df.summary[,"x.mean"] <- formatC(df.summary[,"x.mean"],digits=2,format="f")
      df.summary.wide <- pivot_wider(df.summary,names_from=year,values_from=x.mean)
      ## summarise by each month regardless of the year to get overall mean for each month
      df[,"month"] <- format(df[,"Date"],"%m")
      df.summary.overall <- doBy::summaryBy(x~month,data=df,FUN=mean,na.rm=TRUE,variable.names=variable)
      df.summary.overall[,"x.mean"] <- formatC(df.summary.overall[,"x.mean"],digits=2,format="f")
      names(df.summary.overall)[2] <- "Overall"
      df.summary.all <- merge(df.summary.wide,df.summary.overall,by="month")
      return(df.summary.all)
    }else if(timeframe =="year"){
      df[,"year"] <- format(df[,"Date"],"%Y")
      df.summary <- doBy::summaryBy(x~year,data=df,FUN=mean,na.rm=TRUE,variable.names=variable)
      df.summary[,"x.mean"] <- formatC(df.summary[,"x.mean"],digits=2,format="f")
      df.summary.wide <- pivot_wider(df.summary,names_from=year,values_from=x.mean)
      df.summary.wide[,"Overall"] <- formatC(mean(df$x,na.rm=TRUE),digits=2,format="f")
      return(df.summary.wide)
    }else if(timeframe == "year/season"){
      df <- addSeason(df)
      df.summary <- doBy::summaryBy(x~yearseason,data=df,FUN=mean,na.rm=TRUE,variable.names=variable)
      df.summary[,"year"] <- substr(df.summary[,"yearseason"],1,4)
      df.summary[,"season"] <- substr(df.summary[,"yearseason"],5,nchar(df.summary[,"yearseason"]))
      df.summary <- df.summary[2:4]
      df.summary[,"x.mean"] <- formatC(df.summary[,"x.mean"],digits=2,format="f")
      df.summary.wide <- pivot_wider(df.summary,names_from=year,values_from=x.mean)
      df.summary.overall <- doBy::summaryBy(x~season,data=df,FUN=mean,na.rm=TRUE,variable.names=variable)
      df.summary.overall[,"x.mean"] <- formatC(df.summary.overall[,"x.mean"],digits=2,format="f")
      names(df.summary.overall)[2] <- "Overall"
      df.summary.all <- merge(df.summary.wide,df.summary.overall,by="season")
      return(df.summary.all)
    
    }else if(timeframe =="season"){
      df <- addSeason(df)
      df.summary <- doBy::summaryBy(x~season,data=df,FUN=mean,na.rm=TRUE,variable.names=variable)
      df.summary[,"x.mean"] <- formatC(df.summary[,"x.mean"],digits=2,format="f")
      df.summary.wide <- pivot_wider(df.summary,names_from=season,values_from=x.mean)
      df.summary.wide[,"Overall"] <- formatC(mean(df$x,na.rm=TRUE),digits=2,format="f")
      return(df.summary.wide)
    }else{
      stop("please specify one of the following summarise timeframes:
         'year/month','year','year/season','season'")
    }
  }
  
  
  observeEvent(input$display_table, {
    output$display_summary_table_1 <- renderUI({
      withSpinner(dataTableOutput("summary_table_1"))
    })
    myList <- processed$processed_dailyStats
    variable_to_summarise <- input$summarise_variable_name
    myData <- myList[[which(names(myList)==variable_to_summarise)]]
    summary_df <- mySummarisemore(df=myData,variable=input$summarise_variable_name,metric=input$summarise_metrics,timeframe=input$summarise_by)
    table_title <- paste0(input$summarise_variable_name," ",input$summarise_metrics)
    output$summary_table_1 <- DT::renderDataTable({
      print("inside renderDT now...")
      myTable <- DT::datatable(
                 summary_df,
                 caption = htmltools::tags$caption(table_title,style="color:black;font-size:16px;font-weight:bold;text-align:center;"),
                 extensions ="Buttons",
                 rownames = FALSE,
          options = list(
          scrollX = TRUE, #allow user to scroll wide tables horizontally
          stateSave = FALSE,
          pageLength = 15,
          dom = 'Bt',
          buttons = list('copy','print',list(extend = 'collection',buttons = c('csv','excel','pdf'),text='Download')),
          columnDefs = list(list(className="dt-center",targets="_all"))
          )
      ) # dataTable end
      
      print(myTable)
    })  # renderDT end
  
  }) # observeEvent end
  
  ################# for subtab 2:Time series plot << All parameters #################
  observeEvent(input$display_ts, {
    output$display_time_series <- renderUI({
      withSpinner(plotOutput("plot_dailyStats_ts",height="550px",width="1200px"),type=2)
    })
    myList <- processed$processed_dailyStats
    variable_to_plot <- input$dailyStats_ts_variable_name
    myData <- myList[[which(names(myList)==variable_to_plot)]]
    mean_col <- paste0(input$dailyStats_ts_variable_name,".",input$dailyStats_ts_metrics)
    if (input$dailyStats_shading=="quantiles"){
      upper_col <- paste0(input$dailyStats_ts_variable_name,".q.75%")
      lower_col <- paste0(input$dailyStats_ts_variable_name,".q.25%")
      shading_text <- paste0(input$dailyStats_ts_variable_name, " between daily 25th percentiles and 75th percentiles")
    }else if (input$dailyStats_shading=="minMax"){
      upper_col <- paste0(input$dailyStats_ts_variable_name,".min")
      lower_col <- paste0(input$dailyStats_ts_variable_name,".max")
      shading_text <- paste0(input$dailyStats_ts_variable_name, " between daily minimum and maximum values")
    }
    
    ## dynamically change the "date_breaks" based on the width of the time window
    
    time_range <- difftime(max(as.POSIXct(myData$Date,format="%Y-%m-%d")),min(as.POSIXct(myData$Date,format="%Y-%m-%d")),units="days")
    if (as.numeric(time_range)<365*2){
      myBreaks = paste0(1," months")
      x_date_label = "%Y-%m-%d"
    }else if(as.numeric(time_range)>=365*2&as.numeric(time_range)<365*5){
      myBreaks = paste0(2," months")
      x_date_label = "%Y-%m-%d"
    }else{
      myBreaks = paste0(6," months")
      x_date_label = "%Y-%m"
    }
    
    if (input$dailyStats_ts_metrics=="mean"){
       cols_selected = c("Date",mean_col,lower_col,upper_col)
       data_to_plot <- myData[cols_selected]
       if (!all(is.na(data_to_plot[,mean_col]))){
       
         output$plot_dailyStats_ts <- renderPlot({
         p1 <- ggplot(data_to_plot)+
         geom_line(aes(y=!!sym(mean_col),x=as.POSIXct(Date,format="%Y-%m-%d"),colour=mean_col),size=0.8)+
         geom_ribbon(aes(ymin=!!sym(lower_col),ymax=!!sym(upper_col),x=as.POSIXct(Date,format="%Y-%m-%d"),fill=shading_text),alpha=0.5)+
         scale_x_datetime(date_labels=x_date_label,date_breaks=myBreaks)+
         labs(title=isolate(input$dailyStats_ts_title),x = "Date",y = mean_col)+
         theme_minimal()+
         scale_colour_manual("", values = "blue")+
         scale_fill_manual("", values = "grey12")+
         theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
               ,plot.title = element_text(hjust=0.5)
               ,plot.background = element_rect(color="grey20",size=2)
               ,legend.position = "bottom"
               ,axis.text.x=element_text(angle=45, hjust=1))
        #ggplotly(p1)
         print(p1)
         })  # renderPlot close
       }else{
              shinyalert("Warning","No data available to plot for the selected variable!"
                         ,closeOnClickOutside = TRUE
                         ,closeOnEsc = TRUE
                         ,confirmButtonText="OK"
                         ,inputId = "alert_data_not_avail_for_ts")
       }##inner if else loop close
    
    }else{
      cols_selected = c("Date",mean_col)
      data_to_plot <- myData[cols_selected]
      if (!all(is.na(data_to_plot[,mean_col]))){
        
        output$plot_dailyStats_ts <- renderPlot({
          p1 <- ggplot(data_to_plot)+
            geom_line(aes(y=!!sym(mean_col),x=as.POSIXct(Date,format="%Y-%m-%d"),colour=mean_col),size=0.8)+
            scale_x_datetime(date_labels=x_date_label,date_breaks=myBreaks)+
            labs(title=isolate(input$dailyStats_ts_title), x = "Date",y = mean_col)+
            theme_minimal()+
            scale_colour_manual("", values = "blue")+
            theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                  ,plot.title = element_text(hjust=0.5)
                  ,plot.background = element_rect(color="grey20",size=2)
                  ,legend.position = "bottom"
                  ,axis.text.x=element_text(angle=45, hjust=1))
          #ggplotly(p1)
          print(p1)
        })  # renderPlot close
      }else{
        shinyalert("Warning","No data available to plot for the selected variable!"
                   ,closeOnClickOutside = TRUE
                   ,closeOnEsc = TRUE
                   ,confirmButtonText="OK"
                   ,inputId = "alert_data_not_avail_for_ts")
      }##inner if else loop close
    }
  
  })  # observeEvent end
  
  ## close the alert messages
  observeEvent(input$alert_data_not_avail_for_ts,{
    #print(input$alert_no_date)
    shinyjs::runjs("swal.close();")
  })
  
  
  observeEvent(input$add_more_ts, {
    
    output$another_time_series_UI <- renderUI({
      variables_avail <- names(processed$processed_dailyStats)
      
      sidebarLayout(
        sidebarPanel(width=3,id="ts_sidePanel",
                     h2(strong("UI for another time series plot"),style = "font-size:150%;font-weight:bold;color:#404040;"),
                     hr(),
                     selectizeInput("another_dailyStats_ts_variable_name",label ="Select variable name",
                                    choices=variables_avail,
                                    multiple = FALSE,
                                    selected=variables_avail[length(variables_avail)],
                                    options = list(hideSelected = FALSE)),
                     hr(),
                     selectizeInput("another_dailyStats_ts_metrics",label ="Select daily statistics metrics",
                                    choices=c("mean","median","min", "max","range","sd","var","cv","n"),
                                    multiple = FALSE,
                                    selected="mean",
                                    options = list(hideSelected = FALSE)),
                     hr(),
                     radioButtons("another_dailyStats_shading", "Add shading with", choices = c("25th & 75th percentiles"="quantiles",
                                                                                        "minimum & maximum"="minMax",
                                                                                        "newData"="newData"),
                                  selected = "quantiles"),
                     hr(),
                     textInput(inputId="another_dailyStats_ts_title", label="Plot title",value=""),
                     hr(),
                     fluidRow(column(width=6,
                                     actionButton(inputId="display_another_ts", label="Display",style="color:cornflowerblue;background-color:black;font-weight:bold")
                                     ), # column close
                              column(width=6,align="right",
                                     actionButton("toggleLayout",label="Hide")
                                     ) # column close
                     )
        ),
        mainPanel(width=9,id="ts_mainPanel",
                  fluidRow(column(width=9,uiOutput("display_another_time_series"))),
                  br(),
                  br()
                  
        ) # mainPanel end
        
      ) # sidebarLayout end
     
    })  # renderUI close
    
  })  # observeEvent end
  
  observeEvent(input$toggleLayout,{
    shinyjs::toggle(id="ts_sidePanel")
    shinyjs::toggle(id="ts_mainPanel")
  })
  
  
  observeEvent(input$display_another_ts, {
    output$display_another_time_series <- renderUI({
      withSpinner(plotOutput("plot_another_dailyStats_ts",height="550px",width="1200px"),type=2)
    })
    myList <- processed$processed_dailyStats
    variable_to_plot <- input$another_dailyStats_ts_variable_name
    myData <- myList[[which(names(myList)==variable_to_plot)]]
    mean_col <- paste0(input$another_dailyStats_ts_variable_name,".",input$another_dailyStats_ts_metrics)
    if (input$another_dailyStats_shading=="quantiles"){
      upper_col <- paste0(input$another_dailyStats_ts_variable_name,".q.75%")
      lower_col <- paste0(input$another_dailyStats_ts_variable_name,".q.25%")
      shading_text <- paste0(input$another_dailyStats_ts_variable_name, " between daily 25th percentiles and 75th percentiles")
    }else if (input$another_dailyStats_shading=="minMax"){
      upper_col <- paste0(input$another_dailyStats_ts_variable_name,".min")
      lower_col <- paste0(input$another_dailyStats_ts_variable_name,".max")
      shading_text <- paste0(input$another_dailyStats_ts_variable_name, " between daily minimum and maximum values")
    }
    
    ## dynamically change the "date_breaks" based on the width of the time window
    
    time_range <- difftime(max(as.POSIXct(myData$Date,format="%Y-%m-%d")),min(as.POSIXct(myData$Date,format="%Y-%m-%d")),units="days")
    if (as.numeric(time_range)<365*2){
      myBreaks = paste0(1," months")
      x_date_label = "%Y-%m-%d"
    }else if(as.numeric(time_range)>=365*2&as.numeric(time_range)<365*5){
      myBreaks = paste0(2," months")
      x_date_label = "%Y-%m-%d"
    }else{
      myBreaks = paste0(6," months")
      x_date_label = "%Y-%m"
    }
    
    if (input$another_dailyStats_ts_metrics=="mean"){
      cols_selected = c("Date",mean_col,lower_col,upper_col)
      data_to_plot <- myData[cols_selected]
      if (!all(is.na(data_to_plot[,mean_col]))){
        
        output$plot_another_dailyStats_ts <- renderPlot({
          p1 <- ggplot(data_to_plot)+
            geom_line(aes(y=!!sym(mean_col),x=as.POSIXct(Date,format="%Y-%m-%d"),colour=mean_col),size=0.8)+
            geom_ribbon(aes(ymin=!!sym(lower_col),ymax=!!sym(upper_col),x=as.POSIXct(Date,format="%Y-%m-%d"),fill=shading_text),alpha=0.5)+
            scale_x_datetime(date_labels=x_date_label,date_breaks=myBreaks)+
            labs(title=isolate(input$another_dailyStats_ts_title),x = "Date",y = mean_col)+
            theme_minimal()+
            scale_colour_manual("", values = "blue")+
            scale_fill_manual("", values = "grey12")+
            theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                  ,plot.title = element_text(hjust=0.5)
                  ,plot.background = element_rect(color="grey20",size=2)
                  ,legend.position = "bottom"
                  ,axis.text.x=element_text(angle=45, hjust=1))
          #ggplotly(p1)
          print(p1)
        })  # renderPlot close
      }else{
        shinyalert("Warning","No data available to plot for the selected variable!"
                   ,closeOnClickOutside = TRUE
                   ,closeOnEsc = TRUE
                   ,confirmButtonText="OK"
                   ,inputId = "alert_data_not_avail_for_ts")
      }##inner if else loop close
      
    }else{
      cols_selected = c("Date",mean_col)
      data_to_plot <- myData[cols_selected]
      if (!all(is.na(data_to_plot[,mean_col]))){
        
        output$plot_another_dailyStats_ts <- renderPlot({
          p1 <- ggplot(data_to_plot)+
            geom_line(aes(y=!!sym(mean_col),x=as.POSIXct(Date,format="%Y-%m-%d"),colour=mean_col),size=0.8)+
            scale_x_datetime(date_labels=x_date_label,date_breaks=myBreaks)+
            labs(title=isolate(input$another_dailyStats_ts_title), x = "Date",y = mean_col)+
            theme_minimal()+
            scale_colour_manual("", values = "blue")+
            theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                  ,plot.title = element_text(hjust=0.5)
                  ,plot.background = element_rect(color="grey20",size=2)
                  ,legend.position = "bottom"
                  ,axis.text.x=element_text(angle=45, hjust=1))
          #ggplotly(p1)
          print(p1)
        })  # renderPlot close
      }else{
        shinyalert("Warning","No data available to plot for the selected variable!"
                   ,closeOnClickOutside = TRUE
                   ,closeOnEsc = TRUE
                   ,confirmButtonText="OK"
                   ,inputId = "alert_data_not_avail_for_ts")
      }##inner if else loop close
    }
    
  })  # observeEvent end
  
  
  ################# for subtab 3:Time series - Annual overlays << All parameters #################
  observeEvent(input$display_ts_overlay, {
    
    output$display_time_series_overlay <- renderUI({
      withSpinner(plotlyOutput("plot_dailyStats_ts_overlay",height="550px",width="1200px"),type=2)
    })
    
    myList <- processed$processed_dailyStats
    variable_to_plot <- input$dailyStats_ts_overlay_variable_name
    myData <- myList[[which(names(myList)==variable_to_plot)]]
    mean_col <- paste0(input$dailyStats_ts_overlay_variable_name,".",input$dailyStats_ts_overlay_metrics)
    cols_selected <- c("Date",mean_col)
    data_to_plot <- myData[cols_selected]
    data_to_plot[,"year"] <- format(data_to_plot[,"Date"],"%Y")
    ## dynamically change the "date_breaks" based on the width of the time window
    
    time_range <- max(yday(data_to_plot$Date))-min(yday(data_to_plot$Date))
    if (as.numeric(time_range)<180){
      myBreaks = paste0(1," week")
    }else{
      myBreaks = paste0(2," weeks")
    }
    
    if (!all(is.na(data_to_plot[,mean_col]))){
      
      output$plot_dailyStats_ts_overlay <- renderPlotly({
        p1 <- ggplot(data_to_plot,aes(x=as.Date(yday(Date),"2000-01-01"),y=!!sym(mean_col)))+
          geom_line(aes(colour=year),size=0.8)+
          scale_x_date(date_breaks=myBreaks,date_labels = "%m%d")+
          labs(title=isolate(input$dailyStats_ts_overlay_title),x = "MonthDay",y =mean_col)+
          theme_classic()+
          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                ,panel.border = element_rect(colour="black",fill=NA, size=0.5)
                ,plot.title = element_text(hjust=0.5)
                ,plot.background = element_rect(color="grey20",size=2)
                ,legend.position = "right"
                ,axis.text.x=element_text(angle=45, hjust=1))
        #p1 <- ggplotly(p1,dynamicTicks = TRUE)
        print(p1)
      })  # renderPlot close
    }else{
      shinyalert("Warning","No data available to plot for the selected variable!"
                 ,closeOnClickOutside = TRUE
                 ,closeOnEsc = TRUE
                 ,confirmButtonText="OK"
                 ,inputId = "alert_data_not_avail_for_ts")
    }##inner if else loop close
    
  }) ##observeEvent end
  
  ################# for subtab 4:Boxplots << All parameters #################
  
  observeEvent(input$display_box, {
    
    output$display_box_plots <- renderUI({
      withSpinner(plotOutput("plot_dailyStats_box",height="600px",width="1200px"),type=2)
    })
    
    myList <- processed$processed_dailyStats
    variable_to_plot <- input$boxplot_variable_name
    myData <- myList[[which(names(myList)==variable_to_plot)]]
    mean_col <- paste0(input$boxplot_variable_name,".",input$boxplot_metrics)
    if(input$box_group=="year"){
    myData[,input$box_group] <- format(myData[,"Date"],"%Y")
    cols_selected = c("Date",input$box_group,mean_col)
    }else if(input$box_group=="month"){
    myData[,input$box_group] <- format(myData[,"Date"],"%m")
    cols_selected = c("Date",input$box_group,mean_col)
    }else if(input$box_group=="season"){
    myData <- addSeason(myData)
    cols_selected = c("Date",input$box_group,mean_col)
    }else if(input$box_group=="month2"){
    myData[,"year"] <- format(myData[,"Date"],"%Y")
    myData[,"month"] <- format(myData[,"Date"],"%m")
    cols_selected = c("Date","year","month",mean_col)
    }else if(input$box_group=="season2"){
    myData <- addSeason(myData)
    cols_selected = c("Date","year","season",mean_col)
    }
    
    data_to_plot <- myData[cols_selected]
    if (!all(is.na(data_to_plot[,mean_col]))&input$box_group!="month2"&input$box_group!="season2"){
    output$plot_dailyStats_box <- renderPlot({
      
      p2 <- ggplot(data=data_to_plot,aes(x=!!sym(isolate(input$box_group)),y=!!sym(isolate(mean_col)))) +
        geom_boxplot()+
        labs(title=isolate(input$box_title),x = isolate(input$box_group),y = isolate(input$boxplot_variable_name))+
        theme_bw()+
        theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
              ,plot.title = element_text(hjust=0.5)
              ,axis.text.x = element_text(angle=0, hjust=1))
      #p2 <- ggplotly(p2)
      print(p2)
    })
    } else if(!all(is.na(data_to_plot[,mean_col]))&input$box_group=="month2"){
      output$plot_dailyStats_box <- renderPlot({
        
        p2 <- ggplot(data=data_to_plot,aes(x=month,y=!!sym(isolate(mean_col)),fill=year)) +
          geom_boxplot()+
          labs(title=isolate(input$box_title),x = "month",y = isolate(input$boxplot_variable_name))+
          theme_bw()+
          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                ,plot.title = element_text(hjust=0.5)
                ,axis.text.x = element_text(angle=0, hjust=1))
        #p2 <- ggplotly(p2)
        print(p2)
      })
    } else if(!all(is.na(data_to_plot[,mean_col]))&input$box_group=="season2"){
      output$plot_dailyStats_box <- renderPlot({
        
        p2 <- ggplot(data=data_to_plot,aes(x=season,y=!!sym(isolate(mean_col)),fill=year)) +
          geom_boxplot()+
          labs(title=isolate(input$box_title),x = "season",y = isolate(input$boxplot_variable_name))+
          theme_bw()+
          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                ,plot.title = element_text(hjust=0.5)
                ,axis.text.x = element_text(angle=0, hjust=1))
        #p2 <- ggplotly(p2)
        print(p2)
      }) 
    }else{
      shinyalert("Warning","No data available to plot for the selected variable!",closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                 confirmButtonText="OK",inputId = "alert_data_not_avail_for_box")
    }
  
  })  #observeEvent end
  
  ## close the alert messages
  observeEvent(input$alert_data_not_avail_for_box,{
    #print(input$alert_no_date)
    shinyjs::runjs("swal.close();")
  })
  
  
  ################# for subtab 5:CDF << All parameters  #################
  
  observeEvent(input$run_CDF, {
    
    output$display_plot_CDF <- renderUI({
      withSpinner(plotOutput("plot_CDF",height="600px",width="1200px"),type=2)
    })
    
    
    myList <- processed$processed_dailyStats
    variable_to_plot <- input$CDF_variable_name
    myData.all <- myList[[which(names(myList)==variable_to_plot)]]
    
    if (input$CDF_select_year=="All"){
      myData <- myData.all
    }else{
      myData.all[,"year"] <- format(myData.all[,"Date"],"%Y")
      myData <- myData.all[myData.all$year==input$CDF_select_year,]
    }
    mean_col <- paste0(input$CDF_variable_name,".mean")
    if (input$CDF_shading=="quantiles"){
      upper_col <- paste0(input$CDF_variable_name,".q.75%")
      lower_col <- paste0(input$CDF_variable_name,".q.25%")
    }else if (input$CDF_shading=="minMax"){
      lower_col <- paste0(input$CDF_variable_name,".min")
      upper_col <- paste0(input$CDF_variable_name,".max")
    }
    cols_selected = c("Date",mean_col,lower_col,upper_col)
    data.plot <- myData[cols_selected]
    
    if (input$CDF_select_season=="All"){
      season.choice = NULL
    }else{
      season.choice = input$CDF_select_season
    }
    
    output$plot_CDF <- renderPlot({
      
      # g <- ggplot(data=data.plot,aes(x=!!sym(mean_col)))+
      #      geom_step(stat="ecdf")
      # inside <- ggplot_build(g)
      # matched <- merge(inside$data[[1]],data.frame(x=data.plot[,names(data.plot)==mean_col]
      #                                              ,data.plot[,names(data.plot)==lower_col]
      #                                              ,data.plot[,names(data.plot)==upper_col]),by=("x"))
      # names(matched)[ncol(matched)] <- "data.plot.max"
      # names(matched)[ncol(matched)-1] <-"data.plot.min"
      # CDF_plot <- g+geom_ribbon(data=matched,aes(x=x,ymin=ecdf(data.plot.min)(x),ymax=ecdf(data.plot.max)(x)),alpha=0.5,fill="green")
     
      CDF_plot <- CompSiteCDF.updated(file.input = NULL
                                     , dir.input = getwd()
                                     , dir.output = getwd()
                                     , Param.Name = mean_col
                                     , Shaded.Names = c(lower_col,upper_col)
                                     , Plot.title = isolate(input$CDF_title)
                                     , Plot.season = isolate(season.choice)
                                     , hist.columnName = NULL
                                     , df.input = data.plot)
      if (is.null(CDF_plot)){
        shinyalert("Warning","No data available to plot for the selected variable/year/season!",closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                   confirmButtonText="OK",inputId = "alert_data_not_avail_for_CDF")
      }
      print(CDF_plot)
    }) 
    
    
  }) # observeEvent close
  
  ## close the alert messages
  observeEvent(input$alert_data_not_avail_for_CDF,{
    #print(input$alert_no_date)
    shinyjs::runjs("swal.close();")
  })
  
  
  ################# for subtab 6:Raster graphs << All parameters #################
  observeEvent(input$run_raster, {
    output$display_raster_graphs <- renderUI({
      withSpinner(plotOutput("plot_dailyStats_raster",height="550px",width="1200px"),type=2)
    })
    
    myMonth <- seq(as.Date("2020-01-01"),as.Date("2020-12-31"),by="1 month")
    month_numeric <- lubridate::yday(myMonth)/365*52+1
    month_label <- lubridate::month(myMonth,label=TRUE)
    myList <- processed$processed_dailyStats
    variable_to_plot <- input$dailyStats_raster_variable_name
    myData <- myList[[which(names(myList)==variable_to_plot)]]
    mean_col <- paste0(input$dailyStats_raster_variable_name,".",input$dailyStats_raster_metrics)
    cols_selected <- c("Date",mean_col)
    data_to_plot <- myData[cols_selected]
    data_to_plot[,"year"] <- format(data_to_plot[,"Date"],"%Y")
    if (input$raster_plot_color=="hcl"){
      colorV <- hcl.colors(12)
    }else if(input$raster_plot_color=="rainbow"){
      colorV <- rainbow(12)
    }else if(input$raster_plot_color=="terrain"){
      colorV <- terrain.colors(12)
    }else if (input$raster_plot_color=="heat"){
      colorV <- heat.colors(12)
    }else if (input$raster_plot_color=="topo"){
      colorV <- topo.colors(12)
    }
    #data_to_plot[,"yday"] <- lubridate::yday(as.Date(data_to_plot[,"Date"],format="%Y-%m-%d"))
    if (!all(is.na(data_to_plot[,mean_col]))){
    output$plot_dailyStats_raster <- renderPlot({
         p1 <- ggplot(data_to_plot,aes(x=as.Date(yday(Date),"2000-01-01"),y=year))+
           geom_raster(aes(fill=!!sym(mean_col)))+
           coord_equal()+
           scale_fill_gradientn(name=mean_col,na.value="white",colours=colorV)+
           scale_x_date(date_breaks="1 month",date_labels = "%b")+
           scale_colour_manual(values=NA)+
           labs(title=isolate(input$dailyStats_raster_title), x = "month",y = "year")+
           guides(color=guide_legend("No data",override.aes = list(fill="white")))+
           theme_classic()+
           theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                 ,panel.border = element_rect(colour="black",fill=NA, size=0.5)
                 ,plot.title = element_text(hjust=0.5)
                 ,aspect.ratio = isolate(input$raster_plot_aspect_ratio)
                 ,plot.background = element_rect(color="grey20",size=2)
                 ,legend.position = "right"
                 )
         #ggplotly(p1)
         print(p1)
       })  # renderPlot close
    
  }else{
    shinyalert("Warning","No data available to plot for the selected variable!"
               ,closeOnClickOutside = TRUE
               ,closeOnEsc = TRUE
               ,confirmButtonText="OK"
               ,inputId = "alert_data_not_avail_for_ts")
  }##inner if else loop close
    
  }) ##observeEvent end
  
  ################# for subtab 1: Thermal Statistics << Temperature  ################# 
  
  observeEvent(input$display_thermal, {
  output$display_thermal_table_1 <- renderUI({
    withSpinner(dataTableOutput("thermal_statistics_table_1"))
  })
  
  output$display_thermal_table_2 <- renderUI({
    dataTableOutput("thermal_statistics_table_2")
  })
  
  output$display_thermal_table_3 <- renderUI({
    dataTableOutput("thermal_statistics_table_3")
  })
  
  output$display_thermal_table_4 <- renderUI({
    dataTableOutput("thermal_statistics_table_4")
  })
  
  output$display_thermal_table_5 <- renderUI({
    withSpinner(dataTableOutput("thermal_statistics_table_5"))
  })
  
  myData <- uploaded_data()
  
  streamThermal_exported <- Export.StreamThermal(myData
                                                 ,fun.col.SiteID = input$thermal_SiteID_name
                                                 ,fun.col.Date = input$thermal_Date_name
                                                 ,fun.col.Temp = input$thermal_Temp_name
                                                 )
  
  ##save(streamThermal_exported, file="./test_streamThermal_exported.RData")
  ST.freq <- T_frequency(streamThermal_exported) %>% mutate_if(is.numeric,round,digits=2)
  ST.mag  <- T_magnitude(streamThermal_exported) %>% mutate_if(is.numeric,round,digits=2)
  ST.roc  <- T_rateofchange(streamThermal_exported) %>% mutate_if(is.numeric,round,digits=2)
  ST.tim  <- T_timing(streamThermal_exported) %>% mutate_if(is.numeric,round,digits=2)
  ST.var  <- T_variability(streamThermal_exported) %>% mutate_if(is.numeric,round,digits=2)
  
  processed$ST.freq <- ST.freq
  processed$ST.mag <- ST.mag
  processed$ST.roc <- ST.roc
  processed$ST.tim <- ST.tim
  processed$ST.var <- ST.var
  
  thermal.statistics.table.options <- list(
    scrollX = TRUE, #allow user to scroll wide tables horizontally
    stateSave = FALSE,
    pageLength = 15,
    dom = 'Bt',
    buttons = list('copy','print',list(extend = 'collection',buttons = c('csv','excel','pdf'),text='Download')),
    columnDefs = list(list(className="dt-center",targets="_all"))
  )
  
  output$thermal_statistics_table_1 <- DT::renderDataTable({
   table.title.1 <- "Frequency"
    myTable <- DT::datatable(
      ST.freq,
      caption = htmltools::tags$caption(table.title.1,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
      extensions ="Buttons",
      rownames = FALSE,
      options = thermal.statistics.table.options
    ) # dataTable end
    print(myTable)
  })  # renderDT end
  
  
  output$thermal_statistics_table_2 <- DT::renderDataTable({
    table.title.2 <- "Magnitude"
    myTable <- DT::datatable(
      ST.mag,
      caption = htmltools::tags$caption(table.title.2,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
      extensions ="Buttons",
      rownames = FALSE,
      options = thermal.statistics.table.options
    ) # dataTable end
    print(myTable)
  })  # renderDT end
  
  output$thermal_statistics_table_3 <- DT::renderDataTable({
    table.title.3 <- "Rate of Change"
    myTable <- DT::datatable(
      ST.roc,
      caption = htmltools::tags$caption(table.title.3,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
      extensions ="Buttons",
      rownames = FALSE,
      options = thermal.statistics.table.options
    ) # dataTable end
    print(myTable)
  })  # renderDT end
  
  output$thermal_statistics_table_4 <- DT::renderDataTable({
    table.title.4 <- "Timing"
    myTable <- DT::datatable(
      ST.tim,
      caption = htmltools::tags$caption(table.title.4,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
      extensions ="Buttons",
      rownames = FALSE,
      options = thermal.statistics.table.options
    ) # dataTable end
    print(myTable)
  })  # renderDT end
  
  output$thermal_statistics_table_5 <- DT::renderDataTable({
    table.title.5 <- "Variability"
    myTable <- DT::datatable(
      ST.var,
      caption = htmltools::tags$caption(table.title.5,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
      extensions ="Buttons",
      rownames = FALSE,
      options = thermal.statistics.table.options
    ) # dataTable end
    print(myTable)
  })  # renderDT end
  
  }) #observeEvent end
  
  observeEvent(input$save_thermal, {
    require(XLConnect)
    
    # Descriptions
    #
    Desc.freq <- "Frequency metrics indicate numbers of days in months or seasons
that key events exceed user-defined temperatures. "
    #
    Desc.mag <- "Magnitude metrics characterize monthly and seasonal averages and
the maximum and minimum from daily temperatures as well as 3-, 7-, 14-, 21-,
and 30-day moving averages for mean and maximum daily temperatures."
    #
    Desc.roc <- "Rate of change metrics include monthly and seasonal rate of
change, which indicates the difference in magnitude of maximum and minimum
temperatures divided by number of days between these events."
    #
    Desc.tim <- "Timing metrics indicate Julian days of key events including
mean, maximum, and minimum temperatures; they also indicate Julian days of
mean, maximum, and minimum values over moving windows of specified size."
    #
    Desc.var <- "Variability metrics summarize monthly and seasonal range in
daily mean temperatures as well as monthly coefficient of variation of daily
mean, maximum, and minimum temperatures. Variability metrics also include
moving averages for daily ranges and moving variability in extreme
temperatures, calculated from differences in average high and low
temperatures over various time periods"
    #
    Group.Desc <- c(Desc.freq, Desc.mag, Desc.roc, Desc.tim, Desc.var)
    df.Groups <- as.data.frame(cbind(c("freq","mag","roc","tim","var")
                                     ,Group.Desc))
    SiteID <- processed$ST.freq[1,1]
    myDate <- format(Sys.Date(),"%Y%m%d")
    myTime <- format(Sys.time(),"%H%M%S")
    Notes.User <- Sys.getenv("USERNAME")
    
    Notes.Names <- c("Dataset (SiteID)", "Analysis.Date (YYYYMMDD)"
                     , "Analysis.Time (HHMMSS)", "Analysis.User")
    Notes.Data <- c(SiteID, myDate, myTime, Notes.User)
    df.Notes <- as.data.frame(cbind(Notes.Names, Notes.Data))
    ## New File Name
    if (!file.exists("./Output")) dir.create(file.path("./","Output"),showWarnings = FALSE, recursive = TRUE)
    name_in_file <- loaded_data$name
    myFile.XLSX <- paste("./Output/StreamThermal"
                         , name_in_file
                         , SiteID
                         , myDate
                         , myTime
                         , "xlsx"
                         , sep=".")
    ## Copy over template with Metric Definitions
    file.copy(file.path(path.package("ContDataQC")
                        ,"extdata"
                        ,"StreamThermal_MetricList.xlsx")
              , myFile.XLSX)
    ## load workbook, create if not existing
    wb <- loadWorkbook(myFile.XLSX, create = TRUE)
    # create sheets
    createSheet(wb, name = "NOTES")
    createSheet(wb, name = "freq")
    createSheet(wb, name = "mag")
    createSheet(wb, name = "roc")
    createSheet(wb, name = "tim")
    createSheet(wb, name = "var")
    # write to worksheet
    writeWorksheet(wb, df.Notes, sheet = "NOTES", startRow=1)
    writeWorksheet(wb, df.Groups, sheet="NOTES", startRow=10)
    writeWorksheet(wb, processed$ST.freq, sheet = "freq")
    writeWorksheet(wb, processed$ST.mag, sheet = "mag")
    writeWorksheet(wb, processed$ST.roc, sheet = "roc")
    writeWorksheet(wb, processed$ST.tim, sheet = "tim")
    writeWorksheet(wb, processed$ST.var, sheet = "var")
    # save workbook
    saveWorkbook(wb, myFile.XLSX)
    
  }) # observeEvent close
  
  
  ################# for subtab 2: Thermal Sensitivity << Temperature  ################# 
  
  observeEvent(input$display_thermal_sensitivity, {
    output$display_thermal_sensitivity_plot_1 <- renderUI({
      withSpinner(plotOutput("thermal_sensitivity_plot_1"))
    })
    
    myList <- processed$processed_dailyStats
    ## check if both of "Air.Temp.C" and "Water.Temp.C" are available
    if(all(names(myList) %in% c(input$air_temp_name,input$water_temp_name))){
      myData.Air <- myList[[which(names(myList)==input$air_temp_name)]]
      myData.Water <- myList[[which(names(myList)==input$water_temp_name)]]
      mean_col_air <- paste0(input$air_temp_name,".mean")
      mean_col_water <- paste0(input$water_temp_name,".mean")
      data_air_to_plot <- myData.Air[c("Date",mean_col_air)]
      data_water_to_plot <- myData.Water[c("Date",mean_col_water)]
      data_to_plot <- merge(data_air_to_plot,data_water_to_plot,by="Date")
      if (input$exclude_data_points=="Yes"){
        data_to_plot <- data_to_plot[data_to_plot$Air.Temp.C.mean>input$air_limit_temp,]
      }
      data_to_model <- data_to_plot
      names(data_to_model)[match(mean_col_water,names(data_to_model))] <- "y"
      names(data_to_model)[match(mean_col_air,names(data_to_model))] <- "x"
      myModel <- lm(y ~ x,data_to_model,na.action=na.exclude)
      myEquation <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                               list(a = format(unname(coef(myModel)[1]), digits = 2),
                                    b = format(unname(coef(myModel)[2]), digits = 2),
                                    r2 = format(summary(myModel)$r.squared, digits = 3)))
      
      output$thermal_sensitivity_plot_1 <- renderPlot({
        p1 <- ggplot(data_to_plot,aes(x=!!sym(mean_col_air),y=!!sym(mean_col_water)))+
          geom_point(alpha=0.5,size=1.5)+
          geom_smooth(method="loess",se=FALSE,color="black")+
          geom_smooth(method="lm",se=FALSE,color="cornflowerblue",linetype="dashed",size=2)+
          geom_text(x=(min(data_to_plot[,mean_col_air],na.rm=TRUE)+5)
                    ,y=(max(data_to_plot[,mean_col_water],na.rm=TRUE)-1.5)
                    ,label=as.character(as.expression(myEquation))
                    ,color="cornflowerblue"
                    ,size=8
                    ,parse= TRUE)+
          labs(x = "Air Temperature",y = "Water Temperature")+
          theme_minimal()+
          theme(text=element_text(size=16,face = "bold", color="cornflowerblue"),
                plot.background = element_rect(color="grey20",size=2),
                legend.position = "right",
          )
        #ggplotly(p1)
        print(p1)
      })  # renderPlot close
      
    }else{
      shinyalert("Warning","We need both of air temperature and water temperature data to run thermal sensitivity. Please check."
                 ,closeOnClickOutside = TRUE,closeOnEsc = TRUE,confirmButtonText="OK",inputId = "alert_data_not_val_for_thermal")
    } ## outer if else loop close
  
  }) ## observeEvent end
  
  observeEvent(input$alert_data_not_val_for_thermal,{
    shinyjs::runjs("swal.close();")
  })
  
}