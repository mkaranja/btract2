
css <- "
#large .selectize-input { line-height: 40px; }
#large .selectize-dropdown { line-height: 30px; }"

dataserver <- function(env_serv) with(env_serv, local({
  
  observe({
    updateSelectInput(session, "dt_site",label = "Site:",choices = c(unique(as.character(banana$Location))))
  })
  
 
  datasetInput <- reactive({
    req(input$dataset)
    
    switch(input$dataset, 
           "Flowering" = flowering(),
           "Crosses" = banana,# %>% select(-c(`Days in ripening shed`)),
           "Plantlets" = plantlets(),
           "Status" = status(),
           "Contamination" = contamination(),
           "Subcultures" = subcultures(),
           "Rooting" = rooting(),
           "Weaning 1" = weaning1(),
           "Weaning 2" = weaning2(),
           "Screenhouse" = screenhouse(),
           "Hardening" = hardening(),
           "Open-field" = openfield())
  })
  
 
  output$structureOUT <- renderUI({
    result = datasetInput()
    result = janitor::remove_empty(result, "cols")
    
    div(
      conditionalPanel(
        condition = "input.display=='str'",
        div(
          fluidRow(br(),
                   tags$p(style = "color: #FF8C00; font-size: 18px;","Data structure"),
                   verbatimTextOutput("dataStr"), br()
          ),
          fluidRow(
            conditionalPanel(condition = "input.dataset=='Flowering'", includeMarkdown('www/variables/flowering.md')),
            conditionalPanel(condition = "input.dataset=='Crosses'", includeMarkdown('www/variables/bananadata.md')),
            conditionalPanel(condition = "input.dataset=='Plantlets'", includeMarkdown('www/variables/plantlets.md')),
            conditionalPanel(condition = "input.dataset=='Status'", includeMarkdown('www/variables/status.md')),
            conditionalPanel(condition = "input.dataset=='Contamination'", includeMarkdown('www/variables/contamination.md')),br(),hr(), br()
          ))
      ),
      conditionalPanel(
        condition = "input.display=='summary'",
        fluidRow(br(),
                 tags$p(style = "color: #FF8C00; font-size: 18px;","Data summary"), 
                 print(dfSummary(result, graph.magnif = 1.0), 
                       method = 'render',
                       omit.headings = TRUE,
                       bootstrap.css = FALSE),
                 br(),hr(), br()
        )
      )
    )
  })
  
  output$dataStr <- renderPrint({
    result = datasetInput()
    str(result)
  })
  
  # Data Table TAB
  ##################################################
  viewInput <- reactive({
    result = data.frame(datasetInput())
    columns = colnames(result)
    columns = input$showVars
    if(input$dataset=='Crosses'){
       if(!is.null(input$female_bar_clicked)){
         result <- result %>% 
           dplyr::filter(FemaleGenotype == input$female_bar_clicked[1])
       } 
       if(!is.null(input$male_bar_clicked)){
         result = result %>% 
         dplyr::filter(MaleGenotype == input$male_bar_clicked[1])
       }
     }
    
    result <-  result %>%
      janitor::remove_empty("cols") %>%
      dplyr::filter(!is.na(Location))
    colnames(result) = gsub("[.]"," ", names(result))
    colnames(result) = gsub("_"," ", names(result))
    return(result)
  })
  output$viewdt <- renderDT({
    
    DT::datatable(viewInput(), 
                  filter = 'top', 
                  rownames = FALSE, 
                  escape = FALSE, 
                  options = list(pageLength = 10,
                                 lengthMenu = c(10, 50, 100, 500,1000),
                                 searchHighlight=T, 
                                 stateSave = TRUE)
                  )
  })
  
  
  downloadView <- reactive({
    result = data.frame(datasetInput())
    columns = colnames(result)
    columns = input$showVars
    if(input$dataset=='Crosses'){
      if(!is.null(input$female_bar_clicked)){
        result = result %>%
          dplyr::filter(FemaleGenotype %in% 
                          input$female_bar_clicked[1])
      } 
      if(!is.null(input$male_bar_clicked)){
        result = result %>% 
          dplyr::filter(MaleGenotype %in% 
                          input$male_bar_clicked[1])
      }
      result = result %>%
        dplyr::select(-ends_with('Link'))
    }
    result = result[input[["viewdt_rows_all"]],]
    
    if(!is.null(input$viewdt_rows_selected)){
      result = result[input$viewdt_rows_selected,]
    }
    
    colnames(result) = gsub("[.]"," ", names(result))
    result = result %>%
      janitor::remove_empty("cols") %>%
      dplyr::filter(!is.na(Location))
    return(result)
  })
  output$downloadTbl <- downloadHandler(
    filename = function(){paste(input$dataset,'-', Sys.time(), '.csv')},
    content = function(file) {
      write.csv(downloadView(), file, row.names = FALSE) #datasetInput
    }
  )
  ##################################################
  # Summaries
  
  summaryIn <- reactive({
    result = banana %>%
      dplyr::select(Location, Crossnumber,FemaleGenotype,FemalePlotName, MaleGenotype,FemalePloidy,MalePloidy,
                    `First Pollination Date`,`Bunch Harvest Date`,
                    `Total Seeds`, `Good Seeds`,`Number of Embryo Rescued`,`Number of Embryo Germinating`) %>%
      dplyr::filter(FemaleGenotype !='' & MaleGenotype !='') 
    
    result$Bunches = ifelse(!is.na(result$`Bunch Harvest Date`), 1,0)
    result$`Bunch Harvest Date` = NULL
    result$`Year_of_Pollination` = as.integer(lubridate::year(result$`First Pollination Date`))
    result$`Month_of_Pollination` = month.abb[lubridate::month(result$`First Pollination Date`)]
    
    if(length(input$groupby)>0){
      result = result %>% 
        dplyr::group_by(.dots = input$groupby)
    } else{
      result = result %>% 
        dplyr::group_by(Crossnumber)
    }
    
    result = result %>% 
      dplyr::summarise(`Number of Crosses`=n(),
                       Bunches = sum(Bunches), 
                       `Total Seeds`=sum(na.omit(as.integer(`Total Seeds`))),
                       `Good Seeds` = sum(na.omit(as.integer(`Good Seeds`))),
                       `Number of Embryo Rescued`= sum(na.omit(as.integer(`Number of Embryo Rescued`))),
                       `Number of Embryo Germinating`= sum(na.omit(as.integer(`Number of Embryo Germinating`)))
                       , .groups = 'drop') %>%
      ungroup()
  })
  
  output$summaryDT <- renderDT({
    
    DT::datatable(summaryIn(), 
                  filter = 'top', 
                  rownames = FALSE, 
                  escape = FALSE, 
                  options = list(pageLength = 10,
                                 lengthMenu = c(10, 50, 100, 500,1000),
                                 searchHighlight=T, 
                                 stateSave = TRUE)
                  )
    
  })
  
  
  summaryDownload <- reactive({
    result = summaryIn()
    
    if(!is.null(input$summaryDT_rows_selected)){
      result = result[input$summaryDT_rows_selected,]
    }
    
    result = janitor::remove_empty(result, "cols")
    result = result[complete.cases(result[,1]),]
    return(result)
    
  })
  
  output$downloadSummary <- downloadHandler(
    filename = function(){paste0('BTracT Summary-',input$summaryDateRange[1], '-',input$summaryDateRange[2],'.csv')},
    content = function(file) {
      write.csv(summaryDownload(), file, row.names = FALSE) #datasetInput
    }
  )
  
}))
