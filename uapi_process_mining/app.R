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

r <- GET("http://172.31.50.15:8094/api/Agencies/Get?ordered=1")
Agencies <-  fromJSON(fromJSON(content(r, "text")))
#AllActivities <- c('OptimizedLowFareSearch','BookingStart','BookingAirSegment','BookingTraveler','BookingPricing','BookingPnrElement','BookingDisplay','BookingTerminal','BookingEnd','BookingAirPnrElement','AirTicketing','UniversalRecordRetrieve','AirRetrieveDocument')

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
          seconds = FALSE,
        )
      ),
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
        value = 0.1,
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
  
  # fluidRow(
  #   tabBox(
  #     height = "100%",
  #     width = "100%",
  #
  #
  #    tabPanel(
  #       title = tagList(
  #         icon("project-diagram", class = "fas fa-project-diagram"),
  #         "Workflow Visualization"
  #       ),
  #
  #       box(
  #         grVizOutput("Pr_map", height = "800px"),
  #         #status = "primary",
  #         solidHeader = TRUE,
  #
  #         #title = "Workflow",
  #         width = "100%",
  #         height = "100%"
  #         #collapsible = TRUE
  #       )
  #     ),
  #
  #    tabPanel(
  #      title = tagList(icon("cogs"), "Animation"),
  #
  #      box(
  #        shinycssloaders::withSpinner(processanimaterOutput("process", height = "750px")),
  #        width = "100%",
  #        height = "100%"
  #      )
  #    ),
  #
  #     tabPanel(
  #       title = tagList(icon("fingerprint"),
  #                       "Trace Id Usage"),
  #
  #       box(
  #         DT::dataTableOutput("traceID_aggr"),
  #         #status = "primary",
  #         solidHeader = TRUE,
  #
  #         width = "33%",
  #         height = "100%"
  #       ),
  #       box(
  #         plotOutput("traceId_plot", height = "400px"),
  #         #status = "primary",
  #         solidHeader = TRUE,
  #
  #         width = "33%",
  #         height = "100%"
  #       )
  #     ),
  #
  #     tabPanel(
  #       title = tagList(icon("table"), "Raw data"),
  #
  #       box(DT::dataTableOutput("RawData"),
  #           width = "100%")
  #     )
  #
  #
  #   )
  # )),
  
  fluidRow(mainPanel(
    width = 12,
    
    tabsetPanel(
      type = "tabs",
      
      
      tabPanel(
        title = "Workflow Visualization",
        icon = icon("project-diagram", class = "fas fa-project-diagram"),
        br(),
        shinycssloaders::withSpinner(processanimaterOutput(height = "850px", "process") , type = 1)
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
            DT::dataTableOutput("traceID_aggr", height = 400),
          ),
          
          box(
            width = 6,
            title = "Trace ID usage per Request",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("traceId_plot", height = 400),
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





server <- function(input, output, session) {
  #filterHiveDate <- reactive({
  
  #})
  
  output$process <- NULL
  
  observeEvent(input$PLOT, {
    if (input$Agency_ID == "")
    {
      showNotification("Please choose an Agency to proceed" ,
                       type = "warning")
    }
    else
    {
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
        "\"email\":\"stephanos.kykkotis@travelport.com\",
                        \"password\":\"e4992f9e0b0d130fa5b71456810f441c02de99b779a2d18db19f21290a25cff1\"}"
      
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
      #print(jsonargs)
      parambody <- list(json = jsonargs)
      
      ##
      msgId <-
        showNotification("Retreiving data from HIVE server." ,
                         type = "message" ,
                         duration = NULL)
      
      #res = POST(url, body =  parambody , encode = "form")
      
      
      ##check response code
      #if (res$status_code == 200)
      #{
      ##
      #  removeNotification(msgId)
      #  hivedata = fromJSON(rawToChar(res$content))
      
      ##remove for non-hardcoded data from file
      hivedata <- read_csv("Workflow from Website.csv")
      removeNotification(msgId)
      ########
      
      ##check if return is empty content
      #if (rawToChar(res$content) != "[]")
      {
        output$traceID_aggr = DT::renderDataTable({
          hivedataaggr <-  hivedata %>%
            group_by(traceid) %>%
            summarise(CountTrace = n()) %>%
            arrange(desc(CountTrace))
          hivedataaggr
        })
        
        
        output$traceId_plot <-  renderPlot({
          totalReq <- nrow(hivedata)
          hivedataplot <-
            hivedata %>% group_by(request_type_desc) %>%
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
        })
        
        #hivedata <- hivedata %>%
        #  filter(!(is.na(traceid)))
        
        hivedata$log_ts <-
          as.POSIXct(hivedata$log_ts, format = "%Y-%m-%dT%H:%M:%OS", tz = 'UTC')
        
        #filtering top 100000 records for performance reasons
        hivedata <- head(hivedata, 100000)
        
        output$RawData = DT::renderDataTable({
          hivedata
        })
        
        
        tempSlcted <- input$ExclTraceIDs
        traceIds <- unique(hivedata$traceid)
        updateSelectInput(session,
                          "ExclTraceIDs",
                          choices = traceIds,
                          selected = tempSlcted)
        
        
        tempSlcted <- input$PCC
        Pccs <- unique(hivedata$pseudo_city_code)
        Pccs <- prepend(Pccs , "All")
        updateSelectInput(session,
                          "PCC",
                          choices = Pccs,
                          selected = tempSlcted)
        
        
        
        
        output$Pr_map <- renderGrViz({
          hivedata <- hivedata %>%
            filter(!(traceid %in% c("", " "))) %>%
            filter(!(is.na(traceid))) %>%
            filter(!(traceid  %in% input$ExclTraceIDs))
          
          if (input$PCC != "All")
          {
            hivedata <- hivedata  %>%  filter(pseudo_city_code == input$PCC)
          }
          
          if (nrow(hivedata) == 0) {
            showNotification("No data in Hive for the selected parameters." ,
                             type = "warning")
            pp <- NULL
          }
          else
          {
            pp <-
              hivedata %>% #a data.frame with the information in the table above
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
              
              filter_activity_frequency(percentage = input$frequency) %>%
              process_map(
                type = frequency("absolute"),
                sec_edges = performance(mean, "mins"),
                rankdir = "TB"
              )
          }
        })
        
        
      }
      #else {
      #  removeNotification(msgId)
      #  msgId <-
      #    showNotification("No data returned for selected Agency." ,  type = "warning")
      #}
    }
    
    #  else
    #  {
    #    removeNotification(msgId)
    #    msgId <-
    #      showNotification("Error connecting to HIVE server." ,  type = "error")
    #  }
    #}
    
    
    output$process <- renderProcessanimater(expr = {
      #filtering top 5000 records for performance reasons
      hivedataAnimate <- head(hivedata, 10000)
      
      eventloghive <-
        hivedataAnimate %>% #a data.frame with the information in the table above
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
      
      graph <-
        process_map(
          eventloghive,
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
        eventloghive,
        model,
        mode = "relative",
        mapping = token_aes(color = token_scale("red")),
        duration = 20,
        initial_state = "paused",
      )
      
      
    })
    
    
    output$downloadRawData <- downloadHandler(
      filename = function() {
        paste(input$Agency_ID , Sys.Date(), "data.csv", sep = "")
      },
      content = function(file) {
        write.csv(hivedata , file)
      }
    )
    
    output$loopBox <- renderValueBox({
      valueBox(
        value = number_of_selfloops(eventlog),
        "Approval",
        icon = icon("thumbs-up", lib = "glyphicon"),
        color = "yellow"
      )
    })
    
    
    
    output$downloadProcessMap <- downloadHandler(
      filename = function() {
        paste(input$Agency_ID , Sys.Date(), ".svg", sep = "")
      },
      content = function(file) {
        graphExport <-
          hivedata %>% #a data.frame with the information in the table above
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
          
          filter_activity_frequency(percentage = input$frequency) %>%
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
