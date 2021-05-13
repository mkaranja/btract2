

statusp <- 
  tabPanel("Status",
    column(10,offset = 1,
           
           tabsetPanel(id="status_tabs",
                       
                       tabPanel("Current Status Details", br(),
                                useShinyalert(),
                                
                                tags$p(style = "font-size: 14px; text-align: left;", 
                                       "This section shows the current status of accession in the breeding program. 
     Click on the plot bars to view the detailed information in the data table below. 
     To download the plot, right click on it. Select save as picture."), br(),
                                
                                selectizeInput("current_site",label = "Site", choices = c("None")),
                                
                                tags$div(id = "show_current_activities", 
                                         align = 'center', br(),br(),br(),br(),
                                         actionBttn(inputId = "show_current_activities1", 
                                                    label = "click to display chart", 
                                                    style = "jelly",
                                                    color = "success", 
                                                    icon = icon("play"), size = "sm")
                                ),
                                
                                conditionalPanel(
                                  condition = "input.show_current_activities1 & input.current_site !='None'",
                                  
                                  div(id="current_activities_id",
                                      panel_div(class_type = "default",
                                                content = tags$div( 
                                                    highchartOutput("current_Activities", height = "300px") %>%
                                                      withSpinner(type = 8, color = "#0dc5c1", size = 1)
                                                    )), br(),
                                      
                                      conditionalPanel(
                                        condition = "input.ClickedInput",
                                              panel_div(class_type = "default",
                                                        content = tags$div( 
                                                            DT::DTOutput("current_Table"),
                                                            downloadBttn("download_current_details", style = "pill",size = "xs"),
                                                          )
                                                        ))
                                          )
                                  )),
                       tabPanel("Activities Schedule", br(),
                                useShinyalert(),
                                tags$p(style = "font-size: 14px; text-align: left;", 
                                       "This section shows the status of plants in different stages. Shows which plants are running late and need immediate attention, which are ready to be recorded and which are approaching"), br(),
                                selectizeInput("schedule_site",label = "Site:",choices = c('None')),
                                
                                tags$div(id = "show_schedule", 
                                         align = 'center', br(),br(),br(),br(),
                                         actionBttn(inputId = "show_schedule1", 
                                                    label =  "click to display chart", 
                                                    style = "jelly",
                                                    color = "success", 
                                                    icon = icon("play"), size = "sm")
                                ),
                                
                                conditionalPanel(
                                  condition = "input.show_schedule1 & input.current_site !='None'",
                                  
                                  div(id="overdue",
                                      panel_div(class_type = "default",
                                                content = tags$div( 
                                                  tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;", textOutput('overdue_site'), " Overdue Plants"), br(),
                                                   highchartOutput("overdue_chart") %>%
                                                     withSpinner(type = 8, color = "#0dc5c1", size = 1),
                                                   p(style = "color: #8B0000;", "Click for details"),br(), br(),
                                                   DTOutput('overdue_Tbl')
                                                ))
                                  ), br(),
                                  
                                  div(id="ready",
                                      panel_div(class_type = "default",
                                                content = tags$div( 
                                                  
                                                  tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;", textOutput('ready_site'), " Ready Plants"), br(),
                                                  highchartOutput("ready_chart") %>%
                                                    withSpinner(type = 8, color = "#0dc5c1", size = 1),
                                                  p(style = "color: #8B0000;", "Click for details"),br(), br(),
                                                  DTOutput('ready_Tbl')
                                                  
                                                ))
                                  ), br(),
                                  
                                  div(id="approaching",
                                      panel_div(class_type = "default",
                                                content = tags$div( 
                                                  
                                                  tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;", textOutput('approaching_site'), " Approaching Plants"), br(),
                                                  highchartOutput("approaching_chart") %>%
                                                    withSpinner(type = 8, color = "#0dc5c1", size = 1),
                                                  p(style = "color: #8B0000;", "Click for details"),br(), br(),
                                                  DTOutput('approaching_Tbl')
                                                  
                                                ))
                                  ), br()
                                )
                       )
                       #          
                       #          
                       #          
                       #          fluidRow(
                       #            div(id = "show_overdue",
                       #                panel_div(class_type = "default",
                       #                          content = tags$div( 
                       #                            tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;", textOutput('schedule_site'), " Overdue Accessions"), br(),
                       #                            highchartOutput("overdue_summary") %>% 
                       #                              withSpinner(type = 8, color = "#0dc5c1", size = 1),
                       #                            p(style = "color: #8B0000;", "Click for details"),br(), br(),
                       #                            uiOutput('overdueTbl')
                       #                          )
                       #                )
                       #            ),
                       #            div(id = "show_ready",
                       #                panel_div(class_type = "default",
                       #                          content = tags$div(
                       #                            tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;", textOutput('schedule_site'), " Accessions Ready for Recording"), br(),
                       #                            highchartOutput("ready_summary") %>% 
                       #                              withSpinner(type = 8, color = "#0dc5c1", size = 1),
                       #                            p(style = "color: #006400;", "Click for details"),br(), br(),
                       #                            
                       #                            uiOutput('readyTbl')
                       #                          ))
                       #            ),
                       #            div(id = "show_approaching",
                       #                panel_div(class_type = "default",
                       #                          content = tags$div(
                       #                            tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;", textOutput('schedule_site'), " Accession Approaching Time of Recording"), br(),
                       #                            highchartOutput("approaching_summary") %>%
                       #                              withSpinner(type = 8, color = "#0dc5c1", size = 1),
                       #                            p(style = "color: #00CED1;", "Click for details"),br(), br(), 
                       #                            uiOutput('approachingTbl')
                       #                          ))
                       #            ),
                       #            div(id = "show_onSchedule",
                       #                panel_div(class_type = "default",
                       #                          content = tags$div(
                       #                            div(br(), br(), br(),
                       #                                tags$p(style = "color: #FF8C00; font-size: 18px; text-align: center;", "Nothing To Show"),
                       #                                tags$p(style = "color: green; font-size: 20px; text-align: center;", "Your Records Are On Schedule")
                       #                            )
                       #                          ))
                       #            )
                       #          )
                       #   
                       # )
                       )
           )
  
)