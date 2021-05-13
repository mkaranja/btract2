activities = c("Flowering","First pollination","Repeat pollination","Bunch Harvesting","Harvested bunches","Seed Extraction","Seed extraction","Embryo Rescue","Germination")

#######

statusserver <- function(env_serv) with(env_serv, local({
  
  observe({
    sites <- dbGetQuery(pool, paste('SELECT * FROM location'))
    updateSelectInput("current_site", "Site", choices = c(sites$location) )
  })
  
  output$current_Activities <- renderHighchart({
    result <- dbGetQuery(pool, paste('SELECT * FROM vw_cleanTable')) %>% 
      dplyr::filter(lubridate::year(DateDone) >= 2018) %>%
      setDT()
    if(nrow(data)>0){  
      v = data.table(table(data$Location,data$Activity)) %>% filter(N >0)
      y = data.frame(rowSums(table(v$V1,v$V2)))
      colnames(y) = "Number"
      z = tibble::rownames_to_column(y, var="Location")
      z = tibble(name = z$Location,y = z$Number, drilldown = tolower(name))
      
      for(i in 1:nrow(z)){
        p = data[data$Location == paste0("",z$name[i],""),]
        pp = data.table(table(p$Activity)) %>% filter(N >0)
        ppp <- arrange(tibble(name = pp$V1,value = pp$N),desc(value))
        ppp = left_join(data.frame(name=activities), ppp ,by="name")
        ppp = ppp[complete.cases(ppp),]
        
        assign(paste0("z",i),p)
        assign(paste0("z2",i),pp)
        assign(paste0("z3",i),ppp)
      }
      
      if(nrow(z)==1){
        series = list(
          list(id = paste0(tolower(z$name[1])), data = list_parse2(z31), name="Number")
        )
      } else if(nrow(z)==2){
        series = list(
          list(id = paste0(tolower(z$name[1])), data = list_parse2(z31), name="Number"),
          list(id = paste0(tolower(z$name[2])), data = list_parse2(z32), name="Number")
        )
      } else if(nrow(z)==3){
        series = list(
          list(id = paste0(tolower(z$name[1])), data = list_parse2(z31), name="Number"),
          list(id = paste0(tolower(z$name[2])), data = list_parse2(z32), name="Number"),
          list(id = paste0(tolower(z$name[3])), data = list_parse2(z33), name="Number")
        )
      } else if(nrow(z)==4){
        series = list(
          list(id = paste0(tolower(z$name[1])), data = list_parse2(z31), name="Number"),
          list(id = paste0(tolower(z$name[2])), data = list_parse2(z32), name="Number"),
          list(id = paste0(tolower(z$name[3])), data = list_parse2(z33), name="Number"),
          list(id = paste0(tolower(z$name[4])), data = list_parse2(z34), name="Number")
        )
      } else if(nrow(z)==5){
        series = list(
          list(id = paste0(tolower(z$name[1])), data = list_parse2(z31), name="Number"),
          list(id = paste0(tolower(z$name[2])), data = list_parse2(z32), name="Number"),
          list(id = paste0(tolower(z$name[3])), data = list_parse2(z33), name="Number"),
          list(id = paste0(tolower(z$name[4])), data = list_parse2(z34), name="Number"),
          list(id = paste0(tolower(z$name[5])), data = list_parse2(z35), name="Number")
        )
      }
      
      
      ClickFunction <- JS("function(event) {Shiny.onInputChange('Clicked', event.point.name);}")
      
      highchart() %>%
        hc_chart(type = "column") %>%
        hc_xAxis(type = "category") %>%
        hc_legend(enabled = FALSE) %>%
        hc_yAxis(title = list(text = "Number"), gridLineWidth = 0, tickInterval = 5) %>%
        hc_plotOptions(series = list(column = list(stacking = "normal"), 
                                     borderWidth=0,
                                     dataLabels = list(enabled = TRUE),
                                     events = list(click = ClickFunction)
        )
        ) %>%
        hc_add_series(data=z,name="Number", colorByPoint = TRUE,colors = c("maroon","blue","orange","green")) %>% 
        hc_drilldown(
          allowPointDrilldown = TRUE,
          series = series
        )
    } else {
      return(NULL)
    }
  })
  
  makeReactiveBinding("outputText")
  
  observeEvent(input$Clicked, {
    outputText <<- paste0(input$Clicked)
  })
  
  
  # current details title
  output$currentTitle <- renderUI({
    if(is.null(input$Clicked)){
      div(
        tags$p(style = "color: orange; font-size: 25px; text-align: center;",
               "This plot show the current number of active activities per site"), br()
      )
    } else {
      div(
        tags$p(style = "color: orange; font-size: 25px; text-align: center;",
               paste("This plot show the current number of accessions in every stage in", input$Clicked)), br()
      )
    }
    
  })
  
  current_details <- reactive({
    dbGetQuery(pool, paste('SELECT * FROM vw_cleanTable'))
    
    result = bbDF[lubridate::year(Date) >= 2018]
    result = result[result$Activity !='Status'] # recent records
    if(nrow(result)>0){  
      if(is.null(input$Clicked)){
        v = data.table(table(result$Location,result$Activity)) %>% filter(N >0)
        y = data.frame(rowSums(table(v$V1,v$V2)))
        colnames(y) = "Number"
        z = tibble::rownames_to_column(y, var="Location")
      } else {
        temp <- result
        rowcheck <- temp[temp$Location == input$Clicked,]
        
        if (nrow(rowcheck)!=0) {
          temp <- temp[temp$Location == input$Clicked,]
          Lvl1Click <<- input$Clicked
        } else {
          temp <- temp[temp$Location == Lvl1Click,]
          temp <- temp[temp$Activity == input$Clicked,]
        }
        
        if(input$Clicked=='Seed extraction'){
          temp <- temp[,c("Activity","Accession","Date","Total Seeds")][`Total Seeds`>0]
        } else if(input$Clicked == 'Germination'){
          temp <- temp[,c("Activity", "Accession","Date","Number of Embryo Germinating")][`Number of Embryo Germinating`>0]
        } else if(input$Clicked == 'Embryo rescue'){
          temp = temp[,c("Activity","Accession","Date","Good Seeds","Bad Seeds","Number of Embryo Rescued")][`Good Seeds`>0]
        } else {
          temp = dplyr::select(temp,"Activity","Accession","Date")
        }
        
        if(nrow(table(temp$Activity))>1){
          temp = temp %>%
            dplyr::group_by(Activity) %>%
            dplyr::summarise(`Number of accessions` = n())
          temp = left_join(data.frame(Activity=activities), temp ,by="Activity")
          temp = temp[complete.cases(temp),]
          
        }
        return (temp)
      }
    } else {
      return(NULL)
    }
  })
  
  # Current details Table
  output$current_Table <- DT::renderDataTable({
    current_details()
  })
  
  output$download_current_details <- downloadHandler(filename = function(){
    #result = filter(bbDF, lubridate::year(Date) >= 2018)
    result = data.table(bbDF)
    result = result[lubridate::year(result$Date) >= 2018 & result$Activity !='Status'] # recent records
    
    if(is.null(input$Clicked)){
      paste0("Number of active activities per site",'-',Sys.time(), '.csv')
    } else if(!is.null(input$Clicked)){
      temp <- result
      rowcheck <- temp[temp$Location == input$Clicked,]
      if (nrow(rowcheck)!=0) {
        Lvl1Click <<- input$Clicked
        paste0(input$Clicked,"- active activities",'-',Sys.time(), '.csv')
        
      } else {
        paste0(Lvl1Click, "_",input$Clicked,'-',Sys.time(), '.csv')
      }
      
    }
  },
  content = function(file) {
    write.csv(current_details(), file, row.names = FALSE)
  }
  )
  
  
  # Schedule INFORMATION
  
  overdue <- reactive({
    req(input$schedule_site)
    result = scheduler
    
    if(nrow(schedulerdata)>0){
      result = result %>%
        dplyr::filter(status=='Overdue')
      
      if(input$schedule_site !=''){
        result = result %>%
          dplyr::filter(Location==input$schedule_site)
      }
    } else {
      result = data.frame()
    }
    
    result
  })
  
  ready <- reactive({
    req(input$schedule_site)
    result = scheduler
    
    if(nrow(schedulerdata)>0){
      result = result %>%
        dplyr::filter(status=='Ready')
      
      if(input$schedule_site !=''){
        result = result %>%
          dplyr::filter(Location==input$schedule_site)
      }
    } else {
      result = data.frame()
    }
    
    result
  })
  
  approaching <- reactive({
    req(input$schedule_site)
    result = scheduler
    
    if(nrow(schedulerdata)>0){
      result = result %>%
        dplyr::filter(status=='Approaching')
      
      if(input$schedule_site !=''){
        result = result %>%
          dplyr::filter(Location==input$schedule_site)
      }
    } else {
      result = data.frame()
    }
    result
  })
  
  output$schedulerOut <- renderUI({
    
  })
  
  
  js_overdue_bar_clicked <- JS("function(event) {Shiny.onInputChange('overdue_bar_clicked', [event.point.category]);}")
  output$overdue_summary <- renderHighchart({
    result <- overdue() %>%
      group_by(NextActivity) %>%
      dplyr::tally() %>%
      arrange(desc(n)) %>%
      dplyr::collect() 
    result$NextActivity = as.factor(result$NextActivity)
    result = left_join(data.frame(NextActivity=activities), result,by="NextActivity")
    result = result[complete.cases(result),]
    # plot title
    ptitle = if(input$schedule_site !=""){
      paste(input$schedule_site, " accession late for recording as of ", Sys.Date())
    } else if(input$site ==""){
      paste("Accession late for recording as of ", Sys.Date())
    }
    
    highchart() %>%
      hc_add_series(data = result$n, type = "column", color = '#8B0000', name = paste("Overdue accessions"),  events = list(click = js_overdue_bar_clicked)) %>%
      hc_xAxis(categories = result$NextActivity,tickmarkPlacement="on") %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                 shared = TRUE, borderWidth = 2) %>%
      #hc_title(text=ptitle) %>%
      hc_add_theme(hc_theme_elementary())
    
  })
  
  # display the subsetted data
  output$overdueTbl <- renderUI({
    if(!is.null(input$overdue_bar_clicked)){
      div(
        column(1, offset = 11, downloadBttn('downloadOverdue',size="sm", style = 'jelly')),br(),
        dataTableOutput("overdue_drilldown"))
    }
  })
  
  overdue_drill <- reactive({
    result = overdue()
    if(!is.null(input$overdue_bar_clicked)){
      result = result %>% dplyr::filter(NextActivity %in% input$overdue_bar_clicked[1])
    } 
    result = result %>%
      dplyr::arrange(desc(`Days Elapsed`))
    result[,-6]
  })
  output$overdue_drilldown <- DT::renderDataTable({
    overdue_drill()
  })
  
  # Download
  output$downloadOverdue <- downloadHandler(
    filename = function(){paste0(input$schedule_site,"-",input$overdue_bar_clicked[1],".csv")},
    content = function(file) {
      fwrite(overdue_drill(), file, row.names = FALSE)
    }
  )
  
  #################### READY
  js_ready_bar_clicked <- JS("function(event) {Shiny.onInputChange('ready_bar_clicked', [event.point.category]);}")
  output$ready_summary <- renderHighchart({
    result <- ready() %>%
      group_by(NextActivity) %>%
      dplyr::tally() %>%
      arrange(desc(n)) %>%
      dplyr::collect() 
    result$NextActivity = as.factor(result$NextActivity)
    
    result = left_join(data.frame(NextActivity=activities), result ,by="NextActivity")
    result = result[complete.cases(result),]
    
    # plot title
    ptitle = if(input$schedule_site !=""){
      paste(input$schedule_site, " accessions ready for recording as of ", Sys.Date())
    } else if(input$site ==""){
      paste("Accessions ready for recording as of ", Sys.Date())
    }
    
    highchart() %>%
      hc_add_series(data = result$n, type = "column", color = '#006400', name = paste("Ready accessions"),  events = list(click = js_ready_bar_clicked)) %>%
      hc_xAxis(categories = result$NextActivity,tickmarkPlacement="on") %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                 shared = TRUE, borderWidth = 2) %>%
      hc_title(text=ptitle) %>%
      hc_add_theme(hc_theme_elementary())
    
  })
  
  # display the subsetted data
  output$readyTbl <- renderUI({
    if(!is.null(input$ready_bar_clicked)){
      div(
        column(1, offset = 11, downloadBttn('downloadReady',size="sm", style = 'jelly')),br(),
        dataTableOutput("ready_drilldown")
      )
    }
  })
  ready_drill <- reactive({
    result = ready()
    if(!is.null(input$ready_bar_clicked)){
      result = result %>% dplyr::filter(NextActivity %in% input$ready_bar_clicked[1])
    } 
    result = result %>%
      dplyr::arrange(desc(`Days Elapsed`))
    result[,-6]
  })
  output$ready_drilldown <- DT::renderDataTable({
    ready_drill()
  })
  
  # Download
  output$downloadReady <- downloadHandler(
    filename = function(){paste0(input$schedule_site,"-",input$ready_bar_clicked[1],".csv")},
    content = function(file) {
      fwrite(ready_drill(), file, row.names = FALSE)
    }
  )
  #################### APPROACHING
  js_approaching_bar_clicked <- JS("function(event) {Shiny.onInputChange('approaching_bar_clicked', [event.point.category]);}")
  output$approaching_summary <- renderHighchart({
    result <- approaching() %>%
      group_by(NextActivity) %>%
      dplyr::tally() %>%
      arrange(desc(n)) %>%
      dplyr::collect() 
    result$NextActivity = as.factor(result$NextActivity)
    
    result = left_join(data.frame(NextActivity=activities), result ,by="NextActivity")
    result = result[complete.cases(result),]
    
    # plot title
    ptitle = if(input$schedule_site !=""){
      paste(input$schedule_site, " accessions almost ready for recording as of ", Sys.Date())
    } else if(input$site ==""){
      paste("Accessions almost ready for recording as of ", Sys.Date())
    }
    result = setDT(result)
    highchart() %>%
      hc_add_series(data = result[["n"]], type = "column", color = '#00CED1', name = paste("Approaching accessions"),  events = list(click = js_approaching_bar_clicked)) %>%
      hc_xAxis(categories = result[["NextActivity"]],tickmarkPlacement="on") %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                 shared = TRUE, borderWidth = 2) %>%
      #hc_title(text=ptitle) %>%
      hc_add_theme(hc_theme_elementary())
    
  })
  
  # display the subsetted data
  
  output$approachingTbl <- renderUI({
    
    if(!is.null(input$approaching_bar_clicked)){
      div(
        verbatimTextOutput('txt3'),
        column(1, offset = 11, downloadBttn('downloadApproaching', size="sm", style = 'jelly')),br(),
        dataTableOutput("approaching_drilldown")
      )
    }
  })
  output$txt3 <- renderPrint({
    input$approaching_bar_clicked
  })
  approaching_drill <- reactive({
    result = approaching()
    if(!is.null(input$approaching_bar_clicked)){
      result = result %>% dplyr::filter(NextActivity %in% input$approaching_bar_clicked[1])
    } 
    result = result %>%
      dplyr::arrange(desc(`Days Elapsed`))
    result[,-6]
  })
  
  # Data table
  output$approaching_drilldown <- DT::renderDataTable({
    approaching_drill()
  })
  
  # Download
  output$downloadApproaching <- downloadHandler(
    filename = function(){paste0(input$schedule_site,"-",input$approaching_bar_clicked[1],".csv")},
    content = function(file) {
      fwrite(approaching_drill(), file, row.names = FALSE)
    }
  )
})
)


