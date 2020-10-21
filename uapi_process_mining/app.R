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

r <-
  GET("http://172.31.50.15:8094/api/Agencies/Get?ordered=1") #internal API to retrieve all agencies list in alphabetical order
Agencies <-  fromJSON(fromJSON(content(r, "text")))
#AllActivities <- c('OptimizedLowFareSearch','BookingStart','BookingAirSegment','BookingTraveler','BookingPricing','BookingPnrElement','BookingDisplay','BookingTerminal','BookingEnd','BookingAirPnrElement','AirTicketing','UniversalRecordRetrieve','AirRetrieveDocument')

isDebug <-
  TRUE # when set to true, the dataframe will load from a hardcoded file on the server, otherwise will load from Hive API.

set_labels(#set the label for login screen in shinymanager lib
  language = "en",
  "Please authenticate" = "Please log in")

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "uAPI Workflow Analysis Tool",
                  titleWidth = 350),
  
  
  dashboardSidebar(
    width = 350,
    
    sidebarMenu(id="tabs",
                menuItem("Report Parameters",  icon = icon("file-text-o"),
                         menuSubItem("Request Data", tabName = "global", icon = icon("angle-right")),
                         
                               #Dropdown for Agencies, data loaded from API
                               fluidRow(column(12, div(
                                 selectInput(
                                   "Agency_ID",
                                   label = "Agency*",
                                   choices = c(unique(as.character(Agencies$name))),
                                   width = '100%',
                                   multiple = FALSE
                                 )
                               ))),
                               fluidRow(column(12, div(style = "height:10px"))),
                               
                               #Filter PCC dropdown
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
                               
                               #Checkbox to Exclude LFS requests
                               fluidRow(column(12, div(
                                 checkboxInput(
                                   "includeLFS",
                                   label = "Include Shop Requests",
                                   value = FALSE,
                                   width = "100%"
                                 )
                               ))),
                               
                               fluidRow(column(12, div(style = "height:10px"))),
                               
                               #Data Selection
                               fluidRow(column(12, div(
                                 dateInput(
                                   inputId = "Date",
                                   label = 'Date',
                                   width = "100%"
                                 )
                               ))),
                               
                               #Time Selection
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
                         
                         menuSubItem("Filters", tabName = "ui", icon = icon("angle-right")),
                         
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
                )



    )
  ),
  
  #Dashboard will have the tabs of the output
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
      
      #workflow static Map
      tabPanel(
        title = "Workflow Visualization",
        icon = icon("project-diagram", class = "fas fa-project-diagram"),
        br(),
        fluidRow( column(10 , grVizOutput("Pr_map", height = "800px") ),
                  column(2 , 
                                 fluidRow(infoBoxOutput("TimeStamp")),
                                 fluidRow(infoBoxOutput("Records"))))
      ),

      #workflow animation
      tabPanel(
        title = "Workflow Animation",
        icon = icon("project-diagram", class = "fas fa-sync-alt fa-spin"),
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
      )#,
      
      #tabPanel(
      #  title = "Monitor",
      #  icon = icon("chart-line"),
      #  br(),
      #  valueBoxOutput("loopBox")
      #)
    )
  )))
)

#secure app will enable login screen
ui <- secure_app(ui, enable_admin = TRUE)

server <- function(input, output, session) {
  #check if user is authorized against the user.sqlite database
  res_auth <- secure_server(check_credentials = check_credentials("../database/users.sqlite",
                                                                  passphrase = "Travelport"))
  
  hivedata <- reactive({
    fromDate <- isolate({
      paste(input$Date, strftime(input$FromTime, "%R"), sep = " ")})
    toDate <- isolate({
      paste(input$Date , strftime(input$ToTime, "%R"), sep = " ")})
    
    
    url <-
      "http://cvcpluapiss0059.tvlport.net:9000/queryWarehouse"
    
    #constructing the parameters depending on user's input
    param1 <- "{\"agency\":\""
    param2 <-  "\", \"txType\":\"\", \"startDate\":\""
    param3 <-   " 00:00\", \"endDate\":\""
    param4 <-   " 00:00\", \"successVal\":\"All\",
                        \"fieldsList\": \"log_id,log_ts,request_type_desc,success_ind,agency_name,pseudo_city_code,traceid,session_key,\",
                        \"rowLimit\":\"30000\",
                        \"outputFormat\":\"json\",
                        \"orderBy\":\"log_ts\",
                        \"ascendDescend\":\"ASC\",
                        \"author\":\"\","
    
    
    
    if (input$includeLFS == FALSE)
      param5 <-
      "\"restrictions\": \"request_type_desc not-contains OptimizedLowFareSearch"
    
    
    if (input$PCC != "All")
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
      paste(
        "\"userAuth\":\"",
        res_auth$user ,
        "\", \"email\": \"api_user@donotreply.travelport.com\",\"password\":\"4293ea3b8517c015c542643439b26fe46a694baf9f2598dbd196fd98aac0cdaa\"}" ,
        sep = ""
      )
    
    
    jsonargs <-  isolate({
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
      )})
    print(jsonargs)
    parambody <- list(json = jsonargs)
    ##
   
    
    #section will load data from file
    if (isDebug)
    {
       msgId <-
      showNotification("Loading data from file." ,
                       type = "message" ,
                       duration = NULL)
      #hivedata <- read_csv("Workflow from Website.csv")
      hivedata <- read_csv("IBIBO WEB Hierarchy2020-06-01 00_00.csv")
      hivedata$log_ts <-
        as.POSIXct(hivedata$log_ts, format = "%Y-%m-%dT%H:%M:%OS", tz = 'UTC')
      removeNotification(msgId)
      hivedata <- hivedata %>%
        filter(!(traceid  %in% input$ExclTraceIDs))
      
      if (input$PCC != "All")
      {
        hivedata <- hivedata  %>%  filter(pseudo_city_code == input$PCC)
      }
      
      #filtering top x records for performance reasons
      hivedata <- head(hivedata, 30000)
      
      hivedata
    }
    else if (!isDebug) #call hive API for data
    {
       msgId <-
      showNotification("Retreiving data from HIVE server." ,
                       type = "message" ,
                       duration = NULL)

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
          
          #filtering top x records for performance reasons
          hivedata <- head(hivedata, 30000)

          hivedata
        }
        else
          #case where hive do not return data
        {
          removeNotification(msgId)
          msgId <-
            showNotification("No data returned for selected Agency." ,  type = "warning")
          NULL
        }
      }
      else
        #case when connection to server fails.
      {
        removeNotification(msgId)
        msgId <-
          showNotification("Error connecting to HIVE server." ,  type = "error")
        NULL
      }
    }
  })
  
  #generate even log using eventlog function
  eventloghive <- reactive({
    data <- hivedata()

    if ((!is.null(data)) && (nrow(data) > 0)) #check initial data is exist
    {
      #filtering out excluded trace ids
      data <- data %>%
        filter(!(traceid  %in% input$ExclTraceIDs))
            
      #filtering out exculded PCCs
      if (input$PCC != "All")
      {
        data <- data  %>%  filter(pseudo_city_code == input$PCC)
      }
  
      data <- data %>% #a data.frame with the information in the table above
          filter(!(traceid %in% c("", " "))) %>%
          filter(!(is.na(traceid)))
    }
   
    if ((!is.null(data)) && (nrow(data) > 0)) 
    {
      data %>% #a data.frame with the information in the table above        
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
  output$Pr_map <- NULL

  observeEvent(input$PLOT, {
    if (input$Agency_ID == "")
    {
      showNotification("Please choose an Agency to proceed" ,
                       type = "warning")
    }
    else
    {
      #update the dropdown list of trace ID
      tempSlcted <- input$ExclTraceIDs
      traceIds <- unique(hivedata()$traceid)
      traceIds <- traceIds[!is.na(traceIds)] #remove NA
      updateSelectInput(session,
                        "ExclTraceIDs",
                        choices = traceIds,
                        selected = tempSlcted)
      
      #update the dropdown list of PCC
      tempSlcted <- input$PCC
      Pccs <- unique(hivedata()$pseudo_city_code)
      Pccs <- prepend(Pccs , "All")
      updateSelectInput(session,
                        "PCC",
                        choices = Pccs,
                        selected = tempSlcted)

      #display the usage of traceid
      output$traceID_aggr <- DT::renderDataTable({
        data <- hivedata()
        if (!is.null(data))
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
        if (!is.null(data))
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
      
      #display raw data in a table
      output$RawData <- DT::renderDataTable({
        hivedata()
      })
      
      output$Pr_map <- renderGrViz({
        loghiv <- eventloghive()
      
        if (!is.null(loghiv))
        {
          pp <-
            loghiv %>%
            process_map(
              type = frequency("absolute"),
              sec_edges = performance(mean, "mins"),
              rankdir = "TB"
            )
        }
      })
      
      output$TimeStamp <- renderInfoBox({
        data <- hivedata()
        if(!is.null(data)){
        last_ts <-  max(data$log_ts)
        last_ts <- format(last_ts, format="%H:%M:%S")
          infoBox(
            "Last Record Timestamp", paste0(last_ts), icon = icon("calendar"),
            color = "blue", fill = FALSE
          )
        }
        })
      
      
      output$Records <- renderInfoBox({
        data <- hivedata()
        if(!is.null(data)){
          records <-  nrow(data)
          infoBox(
            "Number of Records", paste0(records), icon = icon("list"),
            color = "purple", fill = FALSE
          )
        }
      })
      
      
      output$process <- renderProcessanimater(expr = {
        loghiv <- eventloghive()
       
        if (!is.null(loghiv))
        {
          graph <-
            process_map(
              loghiv,
              render = F,
              type = frequency("absolute"),
              sec_edges = performance(mean, "mins"),
              rankdir = "TB"
            )
          
          model <- DiagrammeR::add_global_graph_attrs(
            graph,
            attr = "rankdir",
            value = "TB",
            attr_type = "graph"
          )
          
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
      
      #output$loopBox <- renderValueBox({
      #  SL <- number_of_selfloops(eventloghive())
      #  print(SL)
      #  valueBox(
      #    value = SL,
      #    "Approval",
      #    icon = icon("thumbs-up", lib = "glyphicon")
      #  )
      #})
      
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
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
