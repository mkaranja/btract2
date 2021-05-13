
css <- "
#large .selectize-input { line-height: 40px; }
#large .selectize-dropdown { line-height: 30px; }"

dataserver <- function(env_serv) with(env_serv, local({
  
  
  datasetInput <- reactive({
    req(input$dataset)
    
    switch(input$dataset, 
           "Flowering" = tbl(pool, 'flowering') %>% collect(),
           "Crosses" = tbl(pool, 'vw_crosses') %>% collect(),
           "Plantlets" = tbl(pool, 'vw_plantlets') %>% collect(),
           #"Status" = status(),
           #"Contamination" = contamination(),
           "Subcultures" = tbl(pool, 'vw_subculture') %>% collect(),
           "Rooting" = tbl(pool, 'vw_rooting') %>% collect(),
           "Weaning 1" = tbl(pool, 'vw_weaning1') %>% collect(),
           "Weaning 2" = tbl(pool, 'vw_weaning2') %>% collect(),
           "Screenhouse" = tbl(pool, 'vw_screenhouse') %>% collect(),
           "Hardening" = tbl(pool, 'vw_hardening') %>% collect(),
           "Open-field" = tbl(pool, 'vw_openfield') %>% collect()
        )
  })
 
  output$structureOUT <- renderUI({
    result = datasetInput() %>%
    rename_all(~ str_replace_all(., "([a-z])([A-Z])", "\\1 \\2"))
    
    colnames(result) <- capitalize(names(result))
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
  
  observeEvent(input$data_show3,{
    
    shinyjs::hide("data_show_id3")
    
      output$dataStr <- renderPrint({
        result = datasetInput()%>%
          rename_all(~ str_replace_all(., "([a-z])([A-Z])", "\\1 \\2"))
        colnames(result) <- capitalize(names(result))
        
        str(result)
      })
  })
  
  # Data Table TAB

  observeEvent(input$data_show1,{
    
    shinyjs::hide("data_show_id1")
    
  viewInput <- reactive({
    result = data.frame(datasetInput())%>%
      rename_all(~ str_replace_all(., "([a-z])([A-Z])", "\\1 \\2"))
    colnames(result) <- capitalize(names(result))
    
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
      dplyr::filter(!is.na(location))
    colnames(result) = gsub("[.]"," ", names(result))
    
    result[result == '1900-01-01'] <- NA
    result$Location <- as.factor(result$Location)
    
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
    result = data.frame(datasetInput())%>%
      rename_all(~ str_replace_all(., "([a-z])([A-Z])", "\\1 \\2"))
    colnames(result) <- capitalize(names(result))
    
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
      dplyr::filter(!is.na(location))
    return(result)
  })
  output$downloadTbl <- downloadHandler(
    filename = function(){paste(input$dataset,'-', Sys.time(), '.csv')},
    content = function(file) {
      write.csv(downloadView(), file, row.names = FALSE) 
    }
  )
  })
  
  # Summaries
  
  observeEvent(input$data_show2,{ 
    
    shinyjs::hide("data_show_id2")
               
      summaryIn <- reactive({
        result <- tbl(pool, 'vw_crosses') %>% 
          collect() %>%
          dplyr::rename(Location = location,
                        FemaleGenotype = femaleGenotype,
                        MaleGenotype = maleGenotype)
        
        result$Year_of_Pollination = as.integer(lubridate::year(result$pollinationDate))
        result$Month_of_Pollination = month.abb[lubridate::month(result$pollinationDate)]
        
        if(length(input$groupby)>0){
          result <- result %>% 
            dplyr::group_by(.dots = input$groupby)
        } else{
          result = result %>% 
            dplyr::group_by(crossID)
        }
        
        result <- result %>% 
          dplyr::summarise(`Number of Crosses`=n(),
                           #Bunches = sum(Bunches), 
                           `Total Seeds`=sum(na.omit(as.integer(totalSeeds))),
                           `Number of Embryo Rescued`= sum(na.omit(as.integer(numberRescued))),
                           `Number of Embryo Germinating`= sum(na.omit(as.integer(numberGerminating)))
                           ) %>%
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
        filename = function(){paste0('SummaryTable-',input$summaryDateRange[1], '-',input$summaryDateRange[2],'.csv')},
        content = function(file) {
          write.csv(summaryDownload(), file, row.names = FALSE)
        }
      )
  })
}))
