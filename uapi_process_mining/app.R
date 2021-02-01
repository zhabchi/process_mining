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
AllActivities <-
  c(
    "OptimizedLowFareSearch",
    "AirPrice",
    "AirTicketing",
    "AirRetrieveDocument",
    "BookingStart",
    "BookingAirSegment",
    "BookingTraveler",
    "BookingPricing",
    "BookingPnrElement",
    "BookingDisplay",
    "BookingTerminal",
    "BookingEnd",
    "BookingAirPnrElement",
    "UniversalRecordRetrieve"
  )


AllAUx1 <- c('Ignore', 'End') #applies only to BookingEnd

isDebug <-
  FALSE # when set to true, the dataframe will load from a hardcoded file on the server, otherwise will load from Hive API.

set_labels(#set the label for login screen in shinymanager lib
  language = "en",
  "Please authenticate" = "Please log in")

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "uAPI Workflow Analysis Tool",
                  titleWidth = 350),
  
  
  dashboardSidebar(
    width = 350,
    
    sidebarMenu(
      id = "tabs",
      menuItem(
        text = "Report Parameters",
        icon = icon("file-text-o"),
        startExpanded = TRUE,


        menuSubItem(
          "Request Data",
          tabName = "global",
          icon = icon("info-circle")
        ),

        #Dropdown for Agencies, data loaded from API
        fluidRow(column(12, div(
          selectInput(
            "Agency_ID",
            label = "*Agency:",
            choices = c(unique(as.character(Agencies$name))),
            width = "100%",
            multiple = FALSE
          )
        ))),
        fluidRow(column(12, div(style = "height:10px"))),

        #Filter PCC dropdown
        fluidRow(column(12, div(
          textInput(
            "PCC",
            label = "PCC:",
            width = "100%",
          )
        ))),


        fluidRow(column(12, div(style = "height:10px"))),

        #Data Selection
        fluidRow(column(12, div(
          dateInput(
            inputId = "Date",
            label = "*Date:",
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
              "*Start Time:",
              value = strptime("00:00", "%R"),
              seconds = FALSE
            )
          ),
          column(
            6,
            align = "center",
            timeInput(
              "ToTime",
              "*End Time:",
              value =  strptime("23:59", "%R"),
              seconds = FALSE
            )
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

        ))
      ),

      menuItem(
        "Filters",
        icon = icon("filter"),
        menuSubItem(text = ""),

        fluidRow(column(12, div(
          selectInput(
            "ExclTraceIDs",
            label = "Excluded Trace IDs",
            choices = NULL,
            width = "100%",
            multiple = TRUE
          )
        ))),

        fluidRow(column(12, div(
          selectInput(
            "Activities",
            label = "Filter Requests",
            choices = AllActivities,
            width = "100%",
            multiple = TRUE
          )
        ))),

        #Filter PCC dropdown
        fluidRow(column(12, div(
          selectInput(
            "FilterPCC",
            label = "PCC",
            width = "100%",
            choices = NULL,
            multiple = T
          )
        ))),
        
        fluidRow(column(12, div(
          selectInput(
            "Aux1",
            label = "Filter aux1 (for BookingEnd only)",
            choices = AllAUx1,
            width = "100%",
            multiple = TRUE
          )
        )))       
   
      ),
      
      
      menuItem(
        "Export/Import",
        icon = icon("download"),
        menuSubItem(text = "Export"),
        fluidRow(column(
          12,
          align = "center",
          downloadButton(
            outputId = "downloadProcessMap",
            label = "Download Map",
            width = "40%",
            style = "color: #fff; background-color: #337ab7;border-color: #2e6da4"
          ),
          downloadButton(
            outputId = "downloadRawData",
            label = "Download Data",
            width = "40%",
            style = "color: #fff; background-color: #337ab7;border-color: #2e6da4"
          ),
          # Input: Select a file ----
          fileInput("file1", "Choose CSV File",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv"))
          
        ))
      ),
      
       menuItem(
        "Help",
        icon = icon("info"),
                tags$div(tags$p(em("This app has been designed and developed by", tags$br(), strong( "Ziad Habchi
                and Stephanos Kykkotis from") , tags$br(),"Technology Optimization and 
                Bookability teams" , tags$br(), "Dubai, UAE."))
      )),

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
  
  fluidRow(column(12, div(
    sliderInput(
      "Records",
      "Number of Records",
      min = 1000,
      max = 250000,
      value = 30000,
      step = 500,
      width = "100%"
      
    )
    )))
      
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
  
  
  
  fluidRow(
    column(width = 10,
           mainPanel(
             width = 12,
             
             tabsetPanel(
               type = "tabs",
               
               #workflow static Map
               tabPanel(
                 title = "Workflow Visualization",
                 icon = icon("project-diagram", class = "fas fa-project-diagram"),
                 br(),
                 
                 fluidRow(column(
                   12 , grVizOutput(outputId = "Pr_map",
                                    height = "800px")
                 ))
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
                     DT::dataTableOutput("traceID_aggr", height = 600)
                   ),
                   
                   box(
                     width = 6,
                     title = "Trace ID usage per Request",
                     status = "primary",
                     solidHeader = TRUE,
                     plotOutput("traceId_plot", height = 600)
                   )
                   
                 )
               ),
               
               tabPanel(
                 title = "Raw Data Table",
                 icon = icon("table"),
                 br(),
                 fluidRow(column(
                          width=12,
                          DT::dataTableOutput(
                            outputId = "RawData",
                            height = 800
                            )
                          )
                          )
               )#,
               
               #tabPanel(
               #  title = "Monitor",
               #  icon = icon("chart-line"),
               #  br(),
               #  valueBoxOutput("loopBox")
               #)
             )
           )),
    column(
      width = 2,
      br(),
      br(),
      fluidRow(strong("Report Parameters")),
      br(),
      fluidRow(icon("user"), "Agency:"),
      fluidRow(textOutput(outputId = "SelectedAgency")),
      br(),
      fluidRow(icon("clock"), "Last Timestamp:"),
      fluidRow(textOutput(outputId = "TimeStamp")),
      br(),
      fluidRow(icon("list-ol"), "Number of Records loaded:"),
      fluidRow(textOutput(outputId = "Records"))
    )
    
  ))
)

#secure app will enable login screen
ui <- secure_app(ui,
                 tags_bottom = tags$div(
                   tags$p(
                     "For new users or password reset, send your request to the following ",
                     tags$a(href = "mailto:ziad.habchi@travelport.com;Stephanos.Kykkotis@travelport.com;Michael.Wilson@travelport.com?Subject=uAPI%20Workflow%20Analysis%20Tool%20-%20Login%20Details",
                            target = "_top", "email address")
                   )
                 ),
                 enable_admin = TRUE)

server <- function(input, output, session) {
  options(shiny.maxRequestSize=300*1024^2)
  #check if user is authorized against the user.sqlite database
  res_auth <-
    secure_server(check_credentials = check_credentials("../database/users.sqlite",
                                                        passphrase = "Travelport"))
  
  
  hivedata <- reactive({
    x <- input$PLOT #necessary to re-evaluate the fnc on button press
    
    fromDate <- isolate({
      paste(input$Date, strftime(input$FromTime, "%R"), sep = " ")
    })
    toDate <- isolate({
      paste(input$Date , strftime(input$ToTime, "%R"), sep = " ")
    })
    
    
    url <-   "http://cvcpluapiss0010.tvlport.net:8083/queryWarehouse"
    # "http://cvcpluapiss0059.tvlport.net:9000/queryWarehouse"
    
    #constructing the parameters depending on user's input
    param1 <- "{\"agency\":\""
    param2 <-  "\", \"txType\":\"\", \"startDate\":\""
    param3 <-   " 00:00\", \"endDate\":\""
    param4 <-   " 00:00\", \"successVal\":\"All\",
                        \"fieldsList\": \"log_id,log_ts,request_type_desc,success_ind,agency_name,pseudo_city_code,traceid,session_key,aux1\",
                        \"rowLimit\":\"30000\",
                        \"outputFormat\":\"json\",
                        \"orderBy\":\"log_ts\",
                        \"ascendDescend\":\"ASC\",
                        \"author\":\"\","
    
    
    
    if (input$includeLFS == FALSE)
      param5 <-
      "\"restrictions\": \"request_type_desc not-contains OptimizedLowFareSearch"
    
    filterPCC <- isolate(input$PCC)

    if (filterPCC != "")
    {
      if (exists("param5"))
      {
        param5 <-
          paste(param5 ,
                ", pseudo_city_code equals ",
                filterPCC,
                "\"," ,
                sep = "")
      }
      else
        param5 <-
          paste("\"restrictions\": \"pseudo_city_code equals ",
                filterPCC,
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
      )
    })
    #print(jsonargs)
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
      hivedata <- read_csv("IBIBO WEB Hierarchy2020-10-25data.csv")
      hivedata$log_ts <-
        as.POSIXct(hivedata$log_ts, format = "%Y-%m-%dT%H:%M:%OS", tz = 'UTC')
      removeNotification(msgId)
      hivedata <- hivedata %>%
        filter(!(traceid  %in% input$ExclTraceIDs))
      
      if (!is.null(filterPCC) && filterPCC != "")
      {
        hivedata <- hivedata  %>%  filter(pseudo_city_code == filterPCC)
      }
      

      #filtering top x records for performance reasons
      hivedata <- head(hivedata, 30000)
      
      hivedata
    }
    else if (!isDebug)
      #call hive API for data
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
  
  
  filedata <- reactive({
    
        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch(
          {
            msgId <- showNotification("Reading data content from file." ,
                                      type = "message" ,
                                      duration = NULL)
            
            hivedata <- read.csv(input$file1$datapath,
                                 header = TRUE,
                                 sep = ",",
                                 quote = '"')
            hivedata$log_ts <-
              as.POSIXct(hivedata$log_ts, format = "%Y-%m-%dT%H:%M:%OS", tz = 'UTC')
            removeNotification(msgId)
            
            #filtering top x records for performance reasons
            hivedata <- head(hivedata, 30000)
          },
          error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
          }
        )
  })
  
  
  #generate event log using eventlog function
  eventloghive <- reactive({
    data <- hivedata()
    
    if ((!is.null(data)) &&
        (nrow(data) > 0))
      #check initial data is exist
    {
      #filtering out excluded trace ids
      data <- data %>%
        filter(!(traceid  %in% input$ExclTraceIDs))
      
      #filtering out PCCs
      if (!is.null(input$FilterPCC))
      {
        data <- data  %>%  filter(pseudo_city_code %in% input$FilterPCC)
      }
      
      #filtering on selected Activities
      if (!is.null(input$Activities))
      {
        data <- data  %>%  filter(request_type_desc %in% input$Activities)
      }
      
      #filtering on selected aux1 - Applies only to BookingEnd (always include NA aux1)
      if (!is.null(input$Aux1))
      {
        data <- data  %>%  filter((is.na(aux1)) | (aux1 == "") | (aux1 %in% input$Aux1))
      }
      
      data <-
        data %>% #a data.frame with the information in the table above
        filter(!(traceid %in% c("", " "))) %>%
        filter(!(is.na(traceid)))
    }
    
    if ((!is.null(data)) && (nrow(data) > 0))
    {
      data <- head(data,input$Records)      
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
  
  #generate event log using eventlog function
  eventlogfile <- reactive({
    data <- filedata()
    
    if ((!is.null(data)) &&
        (nrow(data) > 0))
      #check initial data is exist
    {
      #filtering out excluded trace ids
      data <- data %>%
        filter(!(traceid  %in% input$ExclTraceIDs))
      
      #filtering out PCCs
      if (!is.null(input$FilterPCC))
      {
        data <- data  %>%  filter(pseudo_city_code %in% input$FilterPCC)
      }
      
      #filtering on selected Activities
      if (!is.null(input$Activities))
      {
        data <- data  %>%  filter(request_type_desc %in% input$Activities)
      }
      
      #filtering on selected aux1 - Applies only to BookingEnd (always include NA aux1)
      if (!is.null(input$Aux1))
      {
        data <- data  %>%  filter((is.na(aux1)) | (aux1 == "") | (aux1 %in% input$Aux1))
      }
      
      data <-
        data %>% #a data.frame with the information in the table above
        filter(!(traceid %in% c("", " "))) %>%
        filter(!(is.na(traceid)))
    }
    
    if ((!is.null(data)) && (nrow(data) > 0))
    {
      data <- head(data,input$Records)      
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
      fillchartsFromHive()
    }
  })
  
  
  
  observeEvent(input$file1, {
   
    fillchartsFromFile()
    
  })
  
   fillchartsFromHive <- reactive({

    data <- hivedata()
     
     
    #update the dropdown list of trace ID
    tempSlcted <- input$ExclTraceIDs
    traceIds <- unique(data$traceid)
    traceIds <- traceIds[!is.na(traceIds)] #remove NA
    updateSelectInput(session,
                      "ExclTraceIDs",
                      choices = traceIds,
                      selected = tempSlcted)
    
    #update the dropdown list of PCC
    tempSlcted <- input$FilterPCC
    Pccs <- unique(data$pseudo_city_code)
    updateSelectInput(session,
                      "FilterPCC",
                      choices = Pccs,
                      selected = tempSlcted)
    
    #display the usage of traceid
    output$traceID_aggr <- DT::renderDataTable({
      #data <- hivedata()
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
      #data <- hivedata()
      if (!is.null(data))
      {
        totalReq <- nrow(data)
        hivedataplot <-
          data %>% group_by(request_type_desc) %>%
          mutate(emptyTrace = ifelse(is.na(traceid), 1, ifelse(traceid == "",1,0))) %>%
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
      DT::datatable(data = data,
                    fillContainer = TRUE,
                    class = "display nowrap",
                    options = list(pageLength = 25)
      )
    })
    
    
    output$Pr_map <- renderGrViz({
      loghiv <- eventloghive()
      
      if (!is.null(loghiv))
      {
        msgId <-
          showNotification("Rendering Chart ..." ,  type = "default")
        pp <-
          loghiv %>%
          process_map(
            type = frequency("absolute"),
            sec_edges = performance(mean, "mins"),
            rankdir = "TB"
          )
        removeNotification(msgId)
        pp
      }
    })
    
    output$TimeStamp <- renderText({
      
      if (!is.null(data)) {
        last_ts <-  max(data$log_ts)
        last_ts <- format(last_ts, format = "%H:%M:%S")
      }
    })
    
    
    output$Records <- renderText({
      #data <- hivedata()
      if (!is.null(data)) {
        records <-  format(nrow(data),
                           big.mark = ",",
                           scientific = FALSE)
      }
    })
    
    output$SelectedAgency <- renderText({
      #data <- hivedata()
      if (!is.null(data) && nrow(data) > 0) {
        agencies <- data$agency_name
        selectedAgency <-  agencies[1]
        selectedAgency
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
          #   mode = "relative",
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
        write.csv(data , file)
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
})
   
   fillchartsFromFile <- reactive({
     
     
     data <- filedata()
     
     
     
     #update the dropdown list of trace ID
     tempSlcted <- input$ExclTraceIDs
     traceIds <- unique(data$traceid)
     traceIds <- traceIds[!is.na(traceIds)] #remove NA
     updateSelectInput(session,
                       "ExclTraceIDs",
                       choices = traceIds,
                       selected = tempSlcted)
     
     #update the dropdown list of PCC
     tempSlcted <- input$FilterPCC
     Pccs <- unique(data$pseudo_city_code)
     updateSelectInput(session,
                       "FilterPCC",
                       choices = Pccs,
                       selected = tempSlcted)
     
     #display the usage of traceid
     output$traceID_aggr <- DT::renderDataTable({
       #data <- hivedata()
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
       #data <- hivedata()
       if (!is.null(data))
       {
         totalReq <- nrow(data)
         hivedataplot <-
           data %>% group_by(request_type_desc) %>%
           mutate(emptyTrace = ifelse(is.na(traceid), 1, ifelse(traceid == "",1,0))) %>%
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
       DT::datatable(data = data,
                     fillContainer = TRUE,
                     class = "display nowrap",
                     options = list(pageLength = 25)
       )
     })
     
     
     output$Pr_map <- renderGrViz({
       loghiv <-  eventlogfile()
       
       if (!is.null(loghiv))
       {
         msgId <-
           showNotification("Rendering Chart ..." ,  type = "default")
         pp <-
           loghiv %>%
           process_map(
             type = frequency("absolute"),
             sec_edges = performance(mean, "mins"),
             rankdir = "TB"
           )
         removeNotification(msgId)
         pp
       }
     })
     
     output$TimeStamp <- renderText({
       
       if (!is.null(data)) {
         last_ts <-  max(data$log_ts)
         last_ts <- format(last_ts, format = "%H:%M:%S")
       }
     })
     
     
     output$Records <- renderText({
       #data <- hivedata()
       if (!is.null(data)) {
         records <-  format(nrow(data),
                            big.mark = ",",
                            scientific = FALSE)
       }
     })
     
     output$SelectedAgency <- renderText({
       #data <- hivedata()
       if (!is.null(data) && nrow(data) > 0) {
         agencies <- data$agency_name
         selectedAgency <-  agencies[1]
         selectedAgency
       }
     })
     
     
     output$process <- renderProcessanimater(expr = {
       loghiv <- eventlogfile()()
       
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
           #   mode = "relative",
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
         write.csv(data , file)
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
           eventlogfile() %>%
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
