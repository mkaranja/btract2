

dataPage <- function()tagList(
  div(
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
                 tags$style(type='text/css', css),
                 div(id = "large",
                     selectInput('groupby', 'Aggregate by:', 
                                 c('Location','Crossnumber','FemaleGenotype','FemalePlotName','MaleGenotype',
                                   'FemalePloidy','MalePloidy','Year_of_Pollination','Month_of_Pollination'), 
                                 multiple=T, width="100%")
                 ) 
               )
    ),
    
    column(9,
           tabsetPanel(
             id = "datatabs",
             
             tabPanel("Data Table",
                      wellPanel(
                        fluidRow(
                          column(12,offset = 11, 
                                 downloadBttn("downloadTbl", size = "xs")), br(), br(),
                          column(12,
                                 div(style = 'overflow-x: scroll',
                                   DTOutput("viewdt")
                                 )
                          )
                        ), br(), hr(), br()
                      )   
             ),
             tabPanel("Summary Table",
                      br(),br(),
                      wellPanel(
                        fluidRow(
                          #tags$p(style = "color: #FF8C00; font-size: 18px; text-align: center;","Table displays total number of accessions grouped by Location, Mother and Father"), br(),
                          column(12, offset = 11,
                                 downloadBttn("downloadSummary","Download", size = "xs")), br(),br(),
                          
                           DTOutput("summaryDT")
                        )
                      )
                      
             ),
             tabPanel("Structure",
                      uiOutput("structureOUT")
             )
           )
    )
  )
)

