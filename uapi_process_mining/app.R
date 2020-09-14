




#

library(shiny)
library(bupaR)
library(readr)
library(dplyr)
library(tidyverse)
library(processmapR)
library(visNetwork)
library(DiagrammeR)
library(httr)
library(jsonlite)
library(shinydashboard)
library(shinyTime)
library(DiagrammeRsvg)
library(rsvg)

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
        timeInput(
          "FromTime",
          "From Time",
          value = strptime("00:00", "%R"),
          seconds = FALSE
        )
      ),
      column(
        6,
        timeInput(
          "ToTime",
          "To Time",
          value =  strptime("23:59", "%R"),
          seconds = FALSE
        )
      ),
    ))),
    
    
    
    fluidRow(column(12, div(style = "height:20px"))),
    
    
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
  
  fluidRow(
    tabBox(
      height = "100%",
      width = "100%",
      
      tabPanel(
        title = tagList(
          icon("project-diagram", class = "fas fa-project-diagram"),
          "Workflow Visualization"
        ),
        
        box(
          grVizOutput("Pr_map", height = "800px"),
          #status = "primary",
          solidHeader = TRUE,
          
          #title = "Workflow",
          width = "100%",
          height = "100%",
          #collapsible = TRUE
        )
      ),
      
      tabPanel(
        title = tagList(icon("fingerprint"),
                        "Trace Id Usage"),
        
        box(
          grVizOutput("traceId_plot", height = "800px"),
          #status = "primary",
          solidHeader = TRUE,
          
          width = "100%",
          height = "100%",
        )
      )
    )
  ))
)



server <- function(input, output, session) {
  #filterHiveDate <- reactive({
  
  #})
  
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
                        \"rowLimit\":\"5000\",
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
      
      res = POST(url, body =  parambody , encode = "form")
      
      
      ##check response code
      if (res$status_code == 200)
      {
        ##
        removeNotification(msgId)
        hivedata = fromJSON(rawToChar(res$content))
      
        #hivedata <- read_csv("Workflow from Website.csv")
        
        ##check if return is empty content
        if (rawToChar(res$content) != "[]") {
          hivedata <-
            hivedata %>% filter(!(traceid %in% c("", " "))) %>%
            filter(!(traceid  %in% input$ExclTraceIDs))
          
          hivedata$log_ts <-
            as.POSIXct(hivedata$log_ts, format = "%Y-%m-%dT%H:%M:%OS", tz = 'UTC')
          
          
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
            hivedata <- hivedata  %>%
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
        else {
          removeNotification(msgId)
          msgId <-
            showNotification("No data returned for selected Agency." ,  type = "warning")
        }
      }
      
      else
      {
        removeNotification(msgId)
        msgId <-
          showNotification("Error connecting to HIVE server." ,  type = "error")
      }
    }
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
  
}

# Run the application
shinyApp(ui = ui, server = server)
