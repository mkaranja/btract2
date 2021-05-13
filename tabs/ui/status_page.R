

statuspage <- 
  tabPanel("Status",
  column(10, offset = 1,                     
                       
   tabsetPanel(#well = T, fluid = T, widths = c(2, 8),
                
                tabPanel("Current Status Details", br(),
                         tags$p(style = "font-size: 14px; text-align: left;", 
                                "This section shows the current status of accession in the breeding program. 
     Click on the plot bars to view the detailed information in the data table below. 
     To download the plot, right click on it. Select save as picture."), br(),
                         
                         selectizeInput("current_site",label = "Site", choices = NULL),
                         
                         tags$div(id = "show_current_activities", 
                                  align = 'center', br(),br(),br(),br(),
                                  actionBttn(inputId = "show_current_activities1", 
                                             label = "click to display chart", 
                                             style = "bordered",
                                             color = "success", 
                                             icon = icon("play"), size = "lg")
                         ),
                         
                         conditionalPanel(
                           condition = "input.show_current_activities1",
                         
                             div(id="current_activities_id",
                               panel_div(class_type = "default",
                                    content = tags$div( 
                                       fluidRow( 
                                           highchartOutput("current_Activities", height = "400px") %>%
                                             withSpinner(type = 8, color = "#0dc5c1", size = 1),
                                           br(), br(),
                                           
                                           downloadBttn("download_current_details", size = "xs"),
                                           DT::dataTableOutput("current_Table")
                                         )
                                       ))
                             ))
                ),
                tabPanel("Activities Schedule",br(),
                         
                         selectizeInput("schedule_site",label = "Site:",choices = c('')),
                         
                         tags$div(id = "show_schedule", 
                                  align = 'center', br(),br(),br(),br(),
                                  actionBttn(inputId = "show_schedule1", 
                                             label = "click to view table", 
                                             style = "bordered",
                                             color = "success", 
                                             icon = icon("play"), size = "lg")
                         ),
                         conditionalPanel(
                           condition = "input.show_schedule1",
                         
                           div(id = "show_overdue",
                               panel_div(class_type = "default",
                                         content = tags$div( 
                                      fluidRow(
                                           tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;", textOutput('schedule_site'), " Overdue Accessions"), br(),
                                           highchartOutput("overdue_summary") %>% 
                                             withSpinner(type = 8, color = "#0dc5c1", size = 1),
                                           p(style = "color: #8B0000;", "Click for details"),br(), br(),
                                           uiOutput('overdueTbl')
                                         )
                               )
                           )),
                           div(id = "show_ready",
                               panel_div(class_type = "default",
                                         content = tags$div(
                                           fluidRow(
                                           tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;", textOutput('schedule_site'), " Accessions Ready for Recording"), br(),
                                           highchartOutput("ready_summary") %>% 
                                             withSpinner(type = 8, color = "#0dc5c1", size = 1),
                                           p(style = "color: #006400;", "Click for details"),br(), br(),
                                           
                                           uiOutput('readyTbl')
                                         ))
                           )),
                           div(id = "show_approaching",
                               panel_div(class_type = "default",
                                         content = tags$div(
                                           fluidRow(
                                           tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;", textOutput('schedule_site'), " Accession Approaching Time of Recording"), br(),
                                           highchartOutput("approaching_summary") %>%
                                             withSpinner(type = 8, color = "#0dc5c1", size = 1),
                                           p(style = "color: #00CED1;", "Click for details"),br(), br(), 
                                           uiOutput('approachingTbl')
                                         ))
                           )),
                           div(id = "show_onSchedule",
                               panel_div(class_type = "default",
                                         content = tags$div(
                                           fluidRow(
                                           div(br(), br(), br(),
                                               tags$p(style = "color: #FF8C00; font-size: 18px; text-align: center;", "Nothing To Show"),
                                               tags$p(style = "color: green; font-size: 20px; text-align: center;", "Your Records Are On Schedule")
                                           )
                                         ))
                           )
                         )
                         )
                )
   )
   )
 )