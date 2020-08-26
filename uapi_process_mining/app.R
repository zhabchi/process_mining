#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
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

r <- GET("http://172.31.50.15:8094/api/Agencies/Get?ordered=1")
Agencies <-  fromJSON(fromJSON(content(r, "text")))
#AllActivities <- c('OptimizedLowFareSearch','BookingStart','BookingAirSegment','BookingTraveler','BookingPricing','BookingPnrElement','BookingDisplay','BookingTerminal','BookingEnd','BookingAirPnrElement','AirTicketing','UniversalRecordRetrieve','AirRetrieveDocument')

ui <- dashboardPage(
    skin = "blue",
    
    dashboardHeader(title = "Process Mining"),
    
    
    dashboardSidebar(
        fluidRow(column(
            12, div(
                style = "height:100px",
                
                selectInput(
                    "Agency_ID",
                    h5(strong(em("Agency"))),
                    
                    choices = c(unique(as.character(Agencies$name))),
                   
                    multiple = FALSE
                )
            )
        )),
        
       
        fluidRow(column(
            12, div(
                
                checkboxInput(
                    "includeLFS",
                    h5(strong(("Include Shop Requests"))),
                    
                    value = FALSE,
                    width = "100%"
                )
            )
        )),
        
        fluidRow(column(
            12, div(
                style = "height:150px",
                
                selectInput(
                    "PCCs",
                    h5(strong(em("PCCs"))),
                    choices = c(''),
                    selected = "",
                    multiple = TRUE
                )
            )
        )),
        
        fluidRow(column(
            12, div(
                style = "height:150px",
                
                sliderInput(
                    "frequency",
                    "Frequency:",
                    min = 0,
                    max = 1,
                    value = 0.5,
                    step = 0.1
                )
            )
        )),
        
        fluidRow(column(
            12, div(style = "height:100px, padding:1px",
                    
                    dateRangeInput("TIME", h5(strong(
                        em("DATE RANGE")
                    ))))
        )),
        
        fluidRow(column(
            12,
            offset = 2,
            
            actionButton(
                inputId = "PLOT",
                label = "PLOT",
                width = "40%",
                height = "40%",
                
                style = "color: #fff; background-color: #337ab7;border-color: #2e6da4"
            )
            
        )),
        
        fluidRow(column(12, div(style = "height:50px")
                        
        )),
        
        fluidRow(column(12, div(strong(
            em(
                "This app has been designed and developed by Ziad Habchi and Stephanos Kykkotis from Techonlgy Optimization and Bookability teams - Dubai, UAE."
            )
        ))))
    ),
    
    dashboardBody(tags$head(tags$style(
        HTML('.main-header .logo {
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
                    icon("project-diagram", class = "fas fa-project-diagram")
                    
                    ,
                    "PROCESS SUMMARY"
                ),
                
                box(
                    grVizOutput("Pr_map"),
                    status = "primary",
                    solidHeader = TRUE,
                    
                    title = "PROCESS MAP",
                    width = "100%",
                    height = "100%",
                    collapsible = TRUE
                )
            )
        )
    ))
)



server <- function(input, output, session) {
    observeEvent(input$PLOT, {
        
        fromDate <- input$TIME[1]
        toDate <- input$TIME[2]
        
        
        url <- "http://cvcpluapiss0059.tvlport.net:9000/queryWarehouse"
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
        
        if(input$includeLFS == FALSE)
             param5 <-  "\"restrictions\": \"request_type_desc not-contains OptimizedLowFareSearch\","
        
        
        param6 <-   "\"email\":\"stephanos.kykkotis@travelport.com\",
                    \"password\":\"e4992f9e0b0d130fa5b71456810f441c02de99b779a2d18db19f21290a25cff1\"
                    }"
        
        jsonargs <- paste(param1, input$Agency_ID, param2, fromDate ,param3, toDate, param4, param5, param6, sep = "")
        #print(jsonargs)
        parambody <- list(json = jsonargs)

        res = POST(url, body =  parambody , encode = "form" )


        data = fromJSON(rawToChar(res$content))
        #PCCs <- unique(data$pseudo_city_code)
        #updateSelectInput(session, "PCCs",
        #                  label = "Filter by PCC",
        #                  choices = PCCs
        #)

        data$log_ts <-
            as.POSIXct(data$log_ts, format = "%Y-%m-%dT%H:%M:%OS", tz = 'UTC')


        data$Row_Activity_ID <-
            data %>% group_indices(data$request_type_desc)

         #if(input$PCCs == "All")
         #    selectedPCCs <- PCCs
         #else
         #    selectedPCCs <- input$PCCs

        #print(selectedPCCs)
        
        output$Pr_map <- renderGrViz(({
            pp <- data %>% #a data.frame with the information in the table above
                mutate(status = NA) %>%
                mutate(lifecycle_id = NA) %>%

                eventlog(
                    case_id = "traceid",
                    activity_id = "request_type_desc",
                    activity_instance_id = "Row_Activity_ID",
                    lifecycle_id = "lifecycle_id",
                    timestamp = "log_ts",
                    resource_id = "pseudo_city_code",
                    validate = FALSE
                ) %>%
                #filter_resource(input$PCCs) %>%
                filter_activity_frequency(percentage = input$frequency) %>%
                process_map()
        }))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
