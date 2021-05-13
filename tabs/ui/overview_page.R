
css <- "
#large .selectize-input { line-height: 40px; }
#large .selectize-dropdown { line-height: 30px; }"

overviewPage <- tabPanel("Overview",
                        includeCSS("www/AdminLTE.css"), # for activating shinydashboard/plus widgets
                         
  tags$head(tags$style(HTML('.modal-sm {width: 80%;}'))),
  fluidRow(
    div(id = "overview_controls",
        column(3, selectInput("site", "Select Site:", choices = c("All"), multiple = F),
               shinyBS::bsTooltip("site", "Select all or a specific site ",
                                  "bottom", options = list(container = "body"))
               ),
        column(3, dateRangeInput("dateRange", "Select Date Range", 
                                 min = "2015-01-01", max = Sys.Date(),  
                                 start = "2015-01-01", end = Sys.Date(),  
                                 startview = "year", separator = " to ",
                                 autoclose = TRUE, width = "100%"),
               shinyBS::bsTooltip("dateRange", "Select the date range to subset data",
                                  "bottom", options = list(container = "body")))
    ),
    column(3, offset = 1,
           shinyjs::useShinyjs(), 
           awesomeCheckbox(
             inputId = "follow_crosses",
             label = "Only crosses done between this date range", 
             value = FALSE,
             status = "info"
           ),
           shinyBS::bsTooltip("follow_crosses", "Shows those crosses done between the dates selected",
                              "bottom", options = list(container = "body"))
           )
  ),
  
  # info boxes
  
  fluidRow(
    column(12,
      tags$div(id = "overview_select_id", 
               align = 'center', br(),br(),br(),br(),
                actionBttn(inputId = "overview_select", 
                  label = "Generate charts", 
                  style = "jelly", 
                  color = "success", 
                  icon = icon("play"), size = "sm"),
               shinyBS::bsTooltip("overview_select", "Click to generate and load the charts",
                                  "bottom", options = list(container = "body"))
     ),
    conditionalPanel(
      condition = "input.overview_select",
        
        fluidRow(
          tags$style("#n_crosses .small-box, #n_totalseeds .small-box, #n_rescued .small-box, #n_germination .small-box, #n_openfield_plantlets .small-box {cursor: pointer;}"),
          
          valueBoxOutput("n_crosses", width = 2),tags$style("#n_crosses"), # tags$style("#n_crosses {width:220px;}")
          valueBoxOutput("n_bunches", width = 2), tags$style("#n_bunches"),
          valueBoxOutput("n_totalseeds", width = 2), tags$style("#n_totalseeds"),
          valueBoxOutput("n_rescued", width = 2), tags$style("#n_rescued"),
          valueBoxOutput("n_germination", width = 2), tags$style("#n_germination"),
          valueBoxOutput("n_openfield_plantlets", width = 2), tags$style("#n_openfield_plantlets"),
          
        ), br(),br(),
        
        # graphs 
        
      fluidRow(
        column(6,
               panel_div(class_type = "default",
                         content = tags$div(
                           HTML("<center><h3>Crosses by Site</h3></center>"),
                           br(),
                           highcharter::highchartOutput("totals_site", height = 300) %>% 
                             withSpinner(type = 8, color = "#0dc5c1", size = 1)
                         ))
        ),
        column(width = 6, 
               panel_div(class_type = "default",
                         content = tags$div(
                           HTML("<center><h3>Number of Crosses</h3></center>"),
                           br(),
                           highcharter::highchartOutput("totals", height = 300)%>% 
                             withSpinner(type = 8, color = "#0dc5c1", size = 1)
                         ))
        )
      ),br(), br(), br(),
        actionBttn('overview_showmore', "show genotypes", block = T, style = "jelly",
                   color = "success", size = "sm"), 
        shinyBS::bsTooltip("overview_showmore", "Click to show most used genotypes",
                         "bottom", options = list(container = "body")),
        br(), br(),br(), br(),
      
      conditionalPanel(
        condition = "input.overview_showmore",
      
            fluidRow(
                column(width = 6,
                       HTML("<center><h4>Female Genotypes</h4></center>"),
                       br(),
                       tags$div(align = 'right',
                                uiOutput("totalFemales")
                       ),
                       highchartOutput("mother", height = 600) %>% 
                         withSpinner(type = 8, color = "#0dc5c1", size = 1)
                ),
                column(width = 6,
                       HTML("<center><h4>Male Genotypes</h4></center>"),
                       br(),
                       tags$div(align = 'right',
                                uiOutput("totalMales")
                       ),
                       highchartOutput("father", height = 600) %>% 
                         withSpinner(type = 8, color = "#0dc5c1", size = 1)
                )
            ))
    ))
  )
)
