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


ui <- dashboardPage(
    skin = "blue",
    
    dashboardHeader(title = "Process Mining"),
    
    
    dashboardSidebar(
        fluidRow(column(
            12, div(
                style = "height:200px",
                
                selectInput(
                    "Agency_ID",
                    h5(strong(em("Agency"))),
                    
                    choices = c("All", unique(as.character(Agencies$name))),
                    
                    selected = "All",
                    multiple = FALSE
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
            12, div(
                style = "height:150px",
                
                sliderInput(
                    "frequency",
                    "Decimal:",
                    min = 0,
                    max = 1,
                    value = 0.5,
                    step = 0.1
                )
            )
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
        
        
        fluidRow(column(12, div(strong(
            em(
                "This app has been designed and developed by Ziad Habchi and Stephanos Kykkotis from Tech Opt and Bookability teams - Dubai, UAE."
            )
        ))))
    ),
    
    dashboardBody(tags$head(tags$style(
        HTML(
            '
                                                .main-header .logo {
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



server <- function(input, output) {
    observeEvent(input$PLOT, {
        url <- "http://cvcpluapiss0059.tvlport.net:9000/queryWarehouse"
        param1 <- "{\"agency\":\""
        
        param2 <-  "\",
                    \"txType\":\"\",
                    \"startDate\":\"2020-06-30 00:00\",
                    \"endDate\":\"2020-07-01 00:00\",
                    \"successVal\":\"All\",
                    \"fieldsList\": \"log_id,log_ts,request_type_desc,success_ind,schema_version,agency_name,target_branch,pseudo_city_code,ur_locator_code,origin_application_code,provider_code_list,supplier_code_list,total_number_of_travellers,total_segments_booked,total_responsetime_ms,app_server_host,request_bytes,response_bytes,host_locator_code,optional_service_qty,travel_segments,travel_dates,supplier_locator_code,flight_number,class_of_service,availability_source,number_in_party,air_segments_booked,air_segments_cancelled,error_level_code,error_type_code,error_text,soap_fault_text,traceid,aux1,aux2,aux3,uapi_responsetime_ms,session_key,db2_locktime,db2_number_of_locks,ptc_list,\",
                    \"rowLimit\":\"25000\",
                    \"outputFormat\":\"json\",
                    \"orderBy\":\"\",
                    \"ascendDescend\":\"ASC\",
                    \"author\":\"\",
                    \"email\":\"stephanos.kykkotis@travelport.com\",
                    \"password\":\"e4992f9e0b0d130fa5b71456810f441c02de99b779a2d18db19f21290a25cff1\"
                    }"
        
        jsonargs <- paste(param1, input$Agency_ID, param2, sep = "")
        
        parambody <- list(json = jsonargs)
        
        res = POST(url, body =  parambody , encode = "form" , verbose())
        
        
        data = fromJSON(rawToChar(res$content))
        
        data$log_ts <-
            as.POSIXct(data$log_ts, format = "%Y-%m-%dT%H:%M:%OS", tz = 'UTC')
        
        
        data$Row_Activity_ID <-
            data %>% group_indices(data$request_type_desc)
        
        output$Pr_map <- renderGrViz(({
            pp <- data %>% #a data.frame with the information in the table above
                mutate(status = NA) %>%
                mutate(resource = NA) %>%
                mutate(lifecycle_id = NA) %>%
                
                eventlog(
                    case_id = "traceid",
                    activity_id = "request_type_desc",
                    activity_instance_id = "Row_Activity_ID",
                    lifecycle_id = "lifecycle_id",
                    timestamp = "log_ts",
                    resource_id = "resource",
                    validate = FALSE
                ) %>%  filter_activity_frequency(percentage = input$frequency, reverse = T) %>%
                process_map()
        }))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
