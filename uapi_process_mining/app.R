library(shiny)
library(bupaR)
library(readr)
library(dplyr)
library(tidyverse)
library(processmapR)
library(processanimateR)
library(eventdataR)
library(visNetwork)
library(DiagrammeR)
library(httr)
library(jsonlite)
library(shinydashboard)
library(shinyTime)
library(DiagrammeRsvg)
library(rsvg)
library(DT)
library(ggplot2)
library(edeaR)
library(shinymanager)

r <- GET("http://172.31.50.15:8094/api/Agencies/Get?ordered=1")
Agencies <-  fromJSON(fromJSON(content(r, "text")))
#AllActivities <- c('OptimizedLowFareSearch','BookingStart','BookingAirSegment','BookingTraveler','BookingPricing','BookingPnrElement','BookingDisplay','BookingTerminal','BookingEnd','BookingAirPnrElement','AirTicketing','UniversalRecordRetrieve','AirRetrieveDocument')

isDebug <- TRUE

set_labels(
  language = "en",
  "Please authenticate" = "Please log in"
)

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "uAPI Workflow Analysis",
                  titleWidth = 350),
  
  
  dashboardSidebar(
    width = 350,
    fluidRow(column(12, div(
      selectInput(
        "Agency_ID",
        label = "Agency",
        choices = c(unique(as.character(Agencies$name))),
        width = '100%',
        multiple = FALSE
      )
    ))),
    
    fluidRow(column(12, div(style = "height:10px"))),
    
    fluidRow(column(12, div(
      selectInput(
        "PCC",
        label = "PCC",
        choices = "All",
        width = '100%',
        multiple = F
      )
    ))),
    
    fluidRow(column(12, div(style = "height:10px"))),
    
    
    fluidRow(column(12, div(
      checkboxInput(
        "includeLFS",
        label = "Include Shop Requests",
        value = FALSE,
        width = "100%"
      )
    ))),
    
    fluidRow(column(12, div(style = "height:10px"))),
    
    
    fluidRow(column(12, div(
      dateInput(
        inputId = "Date",
        label = 'Date',
        width = "100%"
      )
    ))),
    
    fluidRow(column(12, div(
      column(
        6,
        align = "center",
        timeInput(
          "FromTime",
          "From Time",
          value = strptime("00:00", "%R"),
          seconds = FALSE
        )
      ),
      column(
        6,
        align = "center",
        timeInput(
          "ToTime",
          "To Time",
          value =  strptime("23:59", "%R"),
          seconds = FALSE
        )
      )
    ))),
    
    
    fluidRow(column(
      12,
      align = "center",
      actionButton(
        inputId = "PLOT",
        label = "PLOT",
        width = "40%",
        height = "40%",
        style = "color: #fff; background-color: #337ab7;border-color: #2e6da4"
      )
      
    )),
    
    fluidRow(column(12, div(
      selectInput(
        "ExclTraceIDs",
        label = "Excluded Trace IDs",
        choices = NULL,
        width = '100%',
        multiple = TRUE
      )
    ))),
    
    
    fluidRow(column(12, div(style = "height:50px"))),
    
    
    fluidRow(column(12, div(
      sliderInput(
        "frequency",
        "Frequency",
        min = 0.1,
        max = 1,
        value = 0.4,
        step = 0.01,
        width = "100%"
        
      )
    ))),
    
    
    
    fluidRow(column(
      12,
      align = "center",
      downloadButton(
        outputId = "downloadProcessMap",
        label = "Download Map",
        width = "40%",
        height = "40%",
        style = "color: #fff; background-color: #337ab7;border-color: #2e6da4"
      ),
      downloadButton(
        outputId = "downloadRawData",
        label = "Download Data",
        width = "40%",
        height = "40%",
        style = "color: #fff; background-color: #337ab7;border-color: #2e6da4"
      )
      
    )),
    
    
    fluidRow(column(12, div(style = "padding:15px", strong(
      em(
        "This app has been designed and developed by Ziad Habchi and Stephanos Kykkotis from Techonlgy Optimization and Bookability teams - Dubai, UAE."
      )
    ))))
  ),
  
  dashboardBody(tags$head(tags$style(
    HTML(
      '.main-header .logo {
            font-family: "Georgia", Times,
            "Times New Roman",
            font-weight: bold;
            font-size: 24px;
            font-style: italic;
            }
            '
    )
  )),
  
  
  fluidRow(mainPanel(
    width = 12,
    
    tabsetPanel(
      type = "tabs",
      
      
      tabPanel(
        title = "Workflow Visualization",
        icon = icon("project-diagram", class = "fas fa-project-diagram"),
        br(),
        shinycssloaders::withSpinner(processanimaterOutput(height = "800px", "process") , type = 1)
      ),
      
      tabPanel(
        title = "Trace ID Insights",
        icon = icon("fingerprint"),
        br(),
        fluidRow(
          box(
            width = 6,
            title = "Identify Hardcoded Trace IDs",
            status = "primary",
            solidHeader = TRUE,
            DT::dataTableOutput("traceID_aggr", height = 400)
          ),
          
          box(
            width = 6,
            title = "Trace ID usage per Request",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("traceId_plot", height = 400)
          )
          
        )
      ),
      
      tabPanel(
        title = "Raw Data Table",
        icon = icon("table"),
        br(),
        DT::dataTableOutput("RawData")
      ),
      
      tabPanel(
        title = "Monitor",
        icon = icon("chart-line"),
        br(),
        valueBoxOutput("loopBox")
      )
    )
  )))
)

ui <- secure_app(ui, enable_admin = TRUE)

server <- function(input, output, session) {


  res_auth <- secure_server(
    check_credentials = check_credentials("../database/users.sqlite",
                    passphrase = "Travelport"
        )
  )
  
  hivedata <- reactive({
    fromDate <-
      paste(input$Date, strftime(input$FromTime, "%R"), sep = " ")
    toDate <-
      paste(input$Date , strftime(input$ToTime, "%R"), sep = " ")
    
    
    url <-
      "http://cvcpluapiss0059.tvlport.net:9000/queryWarehouse"
    param1 <- "{\"agency\":\""
    param2 <-  "\", \"txType\":\"\", \"startDate\":\""
    param3 <-   " 00:00\", \"endDate\":\""
    param4 <-   " 00:00\", \"successVal\":\"All\",
                        \"fieldsList\": \"log_id,log_ts,request_type_desc,success_ind,agency_name,pseudo_city_code,traceid,session_key,\",
                        \"rowLimit\":\"10000\",
                        \"outputFormat\":\"json\",
                        \"orderBy\":\"\",
                        \"ascendDescend\":\"ASC\",
                        \"author\":\"\","
    
    
    
    if (input$includeLFS == FALSE)
      param5 <-
      "\"restrictions\": \"request_type_desc not-contains OptimizedLowFareSearch"
    
    
    if (input$PCC != "")
    {
      if (exists("param5"))
      {
        param5 <-
          paste(param5 ,
                ", pseudo_city_code equals ",
                input$PCC,
                "\"," ,
                sep = "")
      }
      else
        param5 <-
          paste("\"restrictions\": \"pseudo_city_code equals ",
                input$PCC,
                "\",",
                sep = "")
    }
    else if (exists("param5"))
      param5 <- paste(param5 ,  "\"," , sep = "")
    else
      param5 <- ""
    
    param6 <-
      paste("\"userAuth\":\"", res_auth$user , "\", \"email\": \"api_user@donotreply.travelport.com\",\"password\":\"4293ea3b8517c015c542643439b26fe46a694baf9f2598dbd196fd98aac0cdaa\"}" , sep ="")
    
   
    jsonargs <-
      paste(
        param1,
        input$Agency_ID,
        param2,
        fromDate ,
        param3,
        toDate,
        param4,
        param5,
        param6,
        sep = ""
      )
    print(jsonargs)
    parambody <- list(json = jsonargs)
    ##
    msgId <-
      showNotification("Retreiving data from HIVE server." ,
                       type = "message" ,
                       duration = NULL)
    
    if(isDebug)
    {
      removeNotification(msgId)
      hivedata <- read_csv("Workflow from Website.csv")
      hivedata$log_ts <-
        as.POSIXct(hivedata$log_ts, format = "%Y-%m-%dT%H:%M:%OS", tz = 'UTC')
      
      hivedata <- hivedata %>%
        filter(!(traceid  %in% input$ExclTraceIDs))
      
      if (input$PCC != "All")
      {
        hivedata <- hivedata  %>%  filter(pseudo_city_code == input$PCC)
      }
      
      hivedata
    }
    else if(!isDebug)
    {
      res = POST(url, body =  parambody , encode = "form")


      ##check response code
      if (res$status_code == 200)
      {
        ##
        removeNotification(msgId)
        hivedata = fromJSON(rawToChar(res$content))
        
        ##check if return is empty content
        if (rawToChar(res$content) != "[]")
        {
          hivedata$log_ts <-
            as.POSIXct(hivedata$log_ts, format = "%Y-%m-%dT%H:%M:%OS", tz = 'UTC')
          
          #filtering top 100000 records for performance reasons
          hivedata <- head(hivedata, 30000)
          hivedata <- hivedata %>%
            filter(!(traceid  %in% input$ExclTraceIDs))
          
          if (input$PCC != "All")
          {
            hivedata <- hivedata  %>%  filter(pseudo_city_code == input$PCC)
          }
          
          hivedata
        }
        else
        {
          removeNotification(msgId)
          msgId <-
            showNotification("No data returned for selected Agency." ,  type = "warning")
          NULL
        }
      }
      else
      {
        removeNotification(msgId)
        msgId <-
          showNotification("Error connecting to HIVE server." ,  type = "error")
        NULL
      }
    }
  })
  
  eventloghive <- reactive({
    data <- hivedata()

    if(!is.null(data)){
      data %>% #a data.frame with the information in the table above
        filter(!(traceid %in% c("", " "))) %>%
        filter(!(is.na(traceid))) %>%
        mutate(status = NA) %>%
        mutate(lifecycle_id = NA) %>%
        mutate(activity_instance = 1:nrow(.)) %>%
        eventlog(
          case_id = "traceid",
          activity_id = "request_type_desc",
          activity_instance_id = "activity_instance",
          lifecycle_id = "lifecycle_id",
          timestamp = "log_ts",
          resource_id = "pseudo_city_code",
          validate = FALSE
        ) %>%
        filter_activity_frequency(percentage = input$frequency)
    }
    else 
      NULL
  })
  
  output$process <- NULL
  
  observeEvent(input$PLOT, {
    if (input$Agency_ID == "")
    {
      showNotification("Please choose an Agency to proceed" ,
                       type = "warning")
    }
    else
    {
        output$traceID_aggr <- DT::renderDataTable({
          data <- hivedata()
          if(!is.null(data))
          {
            hivedataaggr <-  data %>%
            group_by(traceid) %>%
            summarise(CountTrace = n()) %>%
            arrange(desc(CountTrace))
             hivedataaggr
          }
          else
            NULL
        })
        
        
        output$traceId_plot <-  renderPlot({
          data <- hivedata()
          if(!is.null(data))
          {
            totalReq <- nrow(hivedata())
            hivedataplot <-
              hivedata() %>% group_by(request_type_desc) %>%
              mutate(emptyTrace = ifelse(is.na(traceid), 1 , 0)) %>%
              summarise(
                count_req =  n() ,
                emptytracecount = sum(emptyTrace) ,
                percUsage = (n() - sum(emptyTrace)) / n()
              )
            
            ggplot(hivedataplot, aes(
              x = reorder(`request_type_desc` , percUsage * 100),
              y = percUsage * 100
            )) +
              geom_bar(stat = 'identity') +
              xlab('Request') +
              ylab('Number of Requests') +
              coord_flip()
          }
        })
        
        
        output$RawData <- DT::renderDataTable({
          hivedata()
        })
        
        
        tempSlcted <- input$ExclTraceIDs
        traceIds <- unique(hivedata()$traceid)
        traceIds <- traceIds[!is.na(traceIds)] #remove NA
        
        updateSelectInput(session,
                          "ExclTraceIDs",
                          choices = traceIds,
                          selected = tempSlcted)
        
        
        tempSlcted <- input$PCC
        Pccs <- unique(hivedata()$pseudo_city_code)
        Pccs <- prepend(Pccs , "All")
        updateSelectInput(session,
                          "PCC",
                          choices = Pccs,
                          selected = tempSlcted)
        
        
        
        
        output$Pr_map <- renderGrViz({
         
          
          if (nrow(hivedata) == 0) {
            showNotification("No data in Hive for the selected parameters." ,
                             type = "warning")
            pp <- NULL
          }
          else
          {
            pp <-
              eventloghive() %>%
              process_map(
                type = frequency("absolute"),
                sec_edges = performance(mean, "mins"),
                rankdir = "TB"
              )
          }
        })
    }
    
    output$process <- renderProcessanimater(expr = {
      
      loghiv <- eventloghive()
      if(!is.null(loghiv))
      {
        graph <-
          process_map(
            loghiv,
            render = F,
            type = frequency("absolute"),
            sec_edges = performance(mean, "mins"),
            rankdir = "TB"
          )
        
        model <- DiagrammeR::add_global_graph_attrs(graph,
                                                    attr = "rankdir",
                                                    value = "TB",
                                                    attr_type = "graph")
        
        animate_process(
          loghiv,
          model,
          mode = "relative",
          mapping = token_aes(color = token_scale("red")),
          duration = 20,
          initial_state = "paused"
        )
      }
      else 
        NULL
      
      
    })
    
    output$downloadRawData <- downloadHandler(
      filename = function() {
        paste(input$Agency_ID , Sys.Date(), "data.csv", sep = "")
      },
      content = function(file) {
        write.csv(hivedata() , file)
      }
    )
    
    output$loopBox <- renderValueBox({
      
      SL <- number_of_selfloops(eventloghive())
      print(SL)
      
      valueBox(value = SL, "Approval", icon = icon("thumbs-up", lib = "glyphicon"))
               
               
    })    
    
    
    output$downloadProcessMap <- downloadHandler(
      filename = function() {
        paste(input$Agency_ID , Sys.Date(), ".svg", sep = "")
      },
      content = function(file) {
        graphExport <-
          eventloghive() %>%
          process_map(
            type = frequency("absolute"),
            sec_edges = performance(mean, "mins"),
            rankdir = "TB",
            render = FALSE
          )
        
        export_graph(graphExport,
                     file_name = file ,
                     file_type = "SVG")
        
      }
    )
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
