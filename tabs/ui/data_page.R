

dataPage <- tabPanel("Data",
                     
  fluidRow(
    column(2, br(),
           
           conditionalPanel(
             condition = "input.datatabs=='Data Table' || input.datatabs=='Structure'",
             selectInput("dataset","Select dataset:", 
                         c("Flowering","Crosses","Plantlets","Status","Contamination", "Subcultures", "Rooting", "Weaning 1", "Weaning 2", "Screenhouse", "Hardening", "Open-field"), 
                         selected = 'Crosses'),
             conditionalPanel(
               condition = "input.datatabs=='Structure'",
               shinyWidgets::prettyRadioButtons(inputId = "display",
                                                label = "Display:", 
                                                choices = c("str", "summary"),
                                                icon = icon("check"), 
                                                bigger = F,
                                                status = "info")
             )
           ),
           conditionalPanel(
             condition = "input.datatabs=='Summary Table'", br(),br(),
             #tags$style(type='text/css', css),
             div(id = "large",
                 selectInput('groupby', 'Aggregate by:', 
                             c('Location','CrossID','FemaleGenotype','MaleGenotype',
                               'Year_of_Pollination','Month_of_Pollination'), 
                             multiple=T, width="100%")
             ) 
           )
    ),
    
    column(9,
           tabsetPanel(
             id = "datatabs",
             
             tabPanel("Data Table",br(),
                      
                      tags$div(id = "data_show_id1", 
                               align = 'center', br(),br(),br(),br(),
                               actionBttn(inputId = "data_show1", 
                                          label = "click to view table", 
                                          style = "jelly", 
                                          color = "success", 
                                          icon = icon("play"), size = "sm")
                      ),
                      conditionalPanel(
                        condition = "input.data_show1",
                        wellPanel(
                          fluidRow(
                            column(12,offset = 11, 
                                   downloadBttn("downloadTbl", size = "xs")), br(), br(),
                            column(12,
                                   div(style = 'overflow-x: scroll',
                                       DTOutput("viewdt")%>% 
                                         withSpinner(type = 8, color = "#0dc5c1", size = 1)
                                   )
                            )
                          ), br(), hr(), br()
                        ))   
             ),
             tabPanel("Summary Table",
                      br(),
                      tags$div(id = "data_show_id2", 
                               align = 'center', br(),br(),br(),br(),
                               actionBttn(inputId = "data_show2", 
                                          label = "click to view table", 
                                          style = "jelly", 
                                          color = "success", 
                                          icon = icon("play"), size = "sm")
                      ),
                        
                        conditionalPanel(
                          condition = "input.data_show2",
                          wellPanel(
                          fluidRow(
                            #tags$p(style = "color: #FF8C00; font-size: 18px; text-align: center;","Table displays total number of accessions grouped by Location, Mother and Father"), br(),
                            column(12, offset = 11,
                                   downloadBttn("downloadSummary","Download", size = "xs")), br(),br(),
                            
                            DTOutput("summaryDT")%>% 
                              withSpinner(type = 8, color = "#0dc5c1", size = 1)
                            )
                          )
                      )
             ),
             tabPanel("Structure", br(),
                      tags$div(id = "data_show_id3", 
                               align = 'center', br(),br(),br(),br(),
                               actionBttn(inputId = "data_show3", 
                                          label = "click to view table", 
                                          style = "jelly", 
                                          color = "success", 
                                          icon = icon("play"), size = "sm")
                      ),
                      
                      conditionalPanel(
                        condition = "input.data_show3",
                          uiOutput("structureOUT") %>%
                          withSpinner(type = 8, color = "#0dc5c1", size = 1)
                      )
             )
           )
    )
  )
)

