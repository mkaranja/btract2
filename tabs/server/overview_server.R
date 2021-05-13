


overviewserver <- function(env_serv) with(env_serv, local({
  
  sites <- dbGetQuery(pool, paste('SELECT * FROM location'))
 
  observe({
    updateSelectInput(session, "site", "Select Site:", choices = c("All", sites$location))
  })
  
  observeEvent(input$overview_select,{
    
    shinyjs::hide("overview_select_id") # hide button
               
  crossesBox <- reactive({
    result <- data.table::setDT(
      dbGetQuery(pool, paste('SELECT * FROM firstPollination'))
      )
    
    if(input$site != "All"){
      result <- result[location %in% input$site]
    }
    subset(result, pollinationDate >= input$dateRange[1] & pollinationDate <= input$dateRange[2])
  })

  
  output$n_crosses <- renderValueBox({
    nunique_crosses <- crossesBox()[,.N, by = .(femaleGenotype, maleGenotype)]
    box1<-valueBox(value=nrow(crossesBox()),
                   color = "teal",
                   href="#",
                   subtitle=HTML("<b>Crosses</b><br>", nrow(nunique_crosses)," Unique combinations")
    )
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"button_n_crosses"
    return(box1)
  })
  
  # on click
  observeEvent(input$button_n_crosses, {
  showModal(modalDialog(tags$h2(style="color:#FF6347;text-align:center;","Banana Crosses"),
                        column(12, offset = 10, 
                               downloadBttn("download_crosses",style = "jelly", size = "sm")),
                        DT::dataTableOutput("list_crosses"), br()
                        , easyClose = T, size = "l"
  ))
  })
  observeEvent(input$button_n_crosses, {
    output$list_crosses <- DT::renderDT({
      result <- crossesBox() %>%
        rename_all(~ str_replace_all(., "([a-z])([A-Z])", "\\1 \\2"))
      colnames(result) <- capitalize(names(result))
      
      DT::datatable(result, filter = 'top', rownames = FALSE, escape = FALSE, 
                    options = list(pageLength = 10, lengthMenu = c(5, 10, 50, 100, 500,1000),
                                   searchHighlight=T, stateSave = TRUE))
    })
  })
  # download all or selected rows
  
  downloadCrosses <- 
    reactive({ 
    result <- crossesBox()%>%
      rename_all(~ str_replace_all(., "([a-z])([A-Z])", "\\1 \\2"))
    colnames(result) <- capitalize(names(result))
    
    result = result[input[["list_crosses_rows_all"]],]
    if(!is.null(input$list_crosses_rows_selected)){
      result <- result[input$list_crosses_rows_selected,]
    }
    result = result[complete.cases(result$crossID),]
    result = janitor::remove_empty(result, "cols")
    return(result)
  })
  
  
  output$download_crosses <- downloadHandler(
    filename = function(){
      paste0(input$site,"-","Crosses",Sys.time(),".csv")
    },
    content = function(file) {
      write.csv(downloadCrosses(), file, row.names = FALSE)
    }
  )
  
  # HARVESTED BUNCHES
  bunchesBox <- reactive({
    result <- data.table::setDT(
      dbGetQuery(pool, paste('SELECT * FROM vw_bunches'))
      )
    
    if(input$site != "All"){
      result <- result[location %in% input$site]
    }
    if(input$follow_crosses==TRUE){
       result <- subset(result, pollinationDate >= input$dateRange[1] & pollinationDate <= input$dateRange[2])
    } else {
    result <- subset(result, harvestingDate >= input$dateRange[1] & harvestingDate <= input$dateRange[2])
   }
    result
  })
  
  output$n_bunches <- renderValueBox({
    result <- bunchesBox()[,.N, by = .(femaleGenotype, maleGenotype)]

    box1<-valueBox(value=nrow(bunchesBox()),
                   width=1,
                   color = "teal",
                   href="#",
                   subtitle=HTML("<b>Bunch Harvested</b><br>", nrow(result), " Unique crosses")
    )
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"button_n_bunches"
    return(box1)
  })

  observeEvent(input$button_n_bunches, {
    showModal(modalDialog(tags$h2(style="color:#FF6347;text-align:center;","Bunches Harvested"),
                          
            column(12, offset = 10,
                   downloadBttn("download_bunches",style = "jelly", size = "sm")),
            DT::dataTableOutput("list_bunches"), br(), br(), 
            easyClose = T, size = "l"
    ))
  })
  
  
  output$list_bunches <- DT::renderDataTable({
    result = bunchesBox()[,pollinationDate:=NULL] %>%
      rename_all(~ str_replace_all(., "([a-z])([A-Z])", "\\1 \\2"))
    colnames(result) <- capitalize(names(result))
    
    DT::datatable(result, filter = 'top', rownames = FALSE, escape = FALSE,
                  options = list(pageLength = 10, lengthMenu = c(5, 10, 50, 100, 500,1000),
                                 searchHighlight=T, stateSave = TRUE))
  })

  downloadBunches <- reactive({
    result <- bunchesBox()[,pollinationDate:=NULL] %>%
      rename_all(~ str_replace_all(., "([a-z])([A-Z])", "\\1 \\2"))
    colnames(result) <- capitalize(names(result))
    
    result = result[input[["list_bunches_rows_all"]],]
    if(!is.null(input$list_bunches_rows_selected)){
      result <- result[input$list_bunches_rows_selected,]
    }
    result = result[complete.cases(result$crossID),]
    result = janitor::remove_empty(result, "cols")
    return(result)
  })


  output$download_bunches <- downloadHandler(
    filename = function(){paste0(input$site,"-","Bunch Harvested",Sys.time(),".csv")},
    content = function(file) {
      write.csv(downloadBunches(), file, row.names = FALSE)
    }
  )
  # SEED EXTRACTION
  #########################################################################################

  seedsBox <- reactive({
    result <- data.table::setDT(
      dbGetQuery(pool, paste('SELECT * FROM vw_seedExtraction'))
    )
    if(input$site != "All"){
      result <- result[location %in% input$site]
    }
    if(input$follow_crosses==TRUE){
      result <- subset(result,pollinationDate >= input$dateRange[1] & pollinationDate <= input$dateRange[2])
    } else {
      result <- subset(result, seedExtractionDate >= input$dateRange[1] & seedExtractionDate <= input$dateRange[2])
    }
    result
  })
  

  output$n_totalseeds <- renderValueBox({
    result <- seedsBox()[,.(sum(totalSeeds)), by = .(femaleGenotype, maleGenotype)] 
    box1<-valueBox(value=sum(result$V1),
                   width=1,
                   color = "teal",
                   href="#",
                   subtitle=HTML("<b>Total Seeds Extracted</b><br>", nrow(result)," Unique crosses")
    )
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"button_n_seeds"
    return(box1)
  })

  observeEvent(input$button_n_seeds, {
    showModal(modalDialog(tags$h2(style="color:#FF6347;text-align:center;","Seeds Extracted"),
          column(12, offset = 10,
                 downloadBttn("download_totalseeds",style = "jelly", size = "sm")),
          DT::dataTableOutput("list_totalseeds"), br(), br(),
          easyClose = T, size = "l"
    ))
  })
  output$list_totalseeds <- DT::renderDataTable({
    result <- seedsBox()[,pollinationDate:=NULL] %>%
      rename_all(~ str_replace_all(., "([a-z])([A-Z])", "\\1 \\2"))
    colnames(result) <- capitalize(names(result))
    
    DT::datatable(result, filter = 'top', rownames = FALSE, escape = FALSE,
                  options = list(pageLength = 10, lengthMenu = c(5, 10, 50, 100, 500,1000),
                                 searchHighlight=T, stateSave = TRUE))
  })

  downloadTotalSeeds <- 
    reactive({
    result <- seedsBox()[,pollinationDate:=NULL] %>%
      rename_all(~ str_replace_all(., "([a-z])([A-Z])", "\\1 \\2"))
    colnames(result) <- capitalize(names(result))
    
    result = result[input[["list_totalseeds_rows_all"]],]
    if(!is.null(input$list_totalseeds_rows_selected)){
      result <- result[input$list_totalseeds_rows_selected,]
    }
      result = result[complete.cases(result$crossID),]
      result = janitor::remove_empty(result, "cols")
      return(result)
  })


  output$download_totalseeds <- downloadHandler(
    filename = function(){paste0(input$site,"-","Seed Extraction",Sys.time(),".csv")},
    content = function(file) {
      write.csv(downloadTotalSeeds(), file, row.names = FALSE)
    }
  )

  #  EMBRYO RESCUE
 
  embryoBox <- reactive({
    result <- data.table::setDT(
      dbGetQuery(pool, paste('SELECT * FROM vw_embryoRescue'))
    )
    if(input$site != "All"){
      result <- result[location %in% input$site]
    }
    if(input$follow_crosses==TRUE){
      result <- subset(result, pollinationDate >= input$dateRange[1] & pollinationDate <= input$dateRange[2])
    } else {
      result <- subset(result, embryoRescueDate >= input$dateRange[1] & embryoRescueDate <= input$dateRange[2])
    }
    result
  })

  output$n_rescued <- renderValueBox({
    result <- embryoBox()[,.(sum(numberRescued)), by = .(femaleGenotype, maleGenotype)]

    box1<-valueBox(value=sum(result$V1),
                   width=1,
                   color = "teal",
                   href="#",
                   subtitle=HTML("<b>Embryo rescue</b><br>",nrow(result)," Unique crosses")
    )
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"button_n_rescued"
    return(box1)
  })

  observeEvent(input$button_n_rescued, {
    showModal(modalDialog(tags$h2(style="color:#FF6347;text-align:center;","Embryo Rescue"),
                          column(12, offset = 10,
                                 downloadBttn("download_rescued",style = "jelly", size = "sm")),
                          DT::dataTableOutput("list_rescued"), br(), br(),
                          easyClose = T, size = "l"
    ))
  })
  output$list_rescued <- DT::renderDataTable({
    result <- embryoBox()[,pollinationDate:=NULL] %>%
      rename_all(~ str_replace_all(., "([a-z])([A-Z])", "\\1 \\2"))
    colnames(result) <- capitalize(names(result))
    
    DT::datatable(result, filter = 'top', rownames = FALSE, escape = FALSE,
                  options = list(pageLength = 10, lengthMenu = c(5, 10, 50, 100, 500,1000),
                                 searchHighlight=T, stateSave = TRUE))
  })

  downloadRescued <- 
    reactive({
    result <- embryoBox()[,pollinationDate:=NULL] %>%
      rename_all(~ str_replace_all(., "([a-z])([A-Z])", "\\1 \\2"))
    colnames(result) <- capitalize(names(result))
    
    result = result[input[["list_rescued_rows_all"]],]

    if(!is.null(input$list_rescued_rows_selected)){
      result <- result[input$list_rescued_rows_selected,]
    }

    result <- result[complete.cases(result$`Cross ID`),]
    result <- janitor::remove_empty(result, "cols")

    return(result)
  })

  output$download_rescued <- downloadHandler(
    filename = function(){paste0(input$site,"-","Embryo rescued",Sys.time(),".csv")},
    content = function(file) {
      write.csv(downloadRescued(), file, row.names = FALSE)
    }
  )

  # GERMINATION
   
  germinationBox <- reactive({
    result <- data.table::setDT(
      dbGetQuery(pool, paste('SELECT * FROM vw_germination'))
    )
    
    if(input$site != "All"){
      result <- result[location %in% input$site]
    }
    if(input$follow_crosses==TRUE){
      result <- subset(result, pollinationDate >= input$dateRange[1] & pollinationDate <= input$dateRange[2])
    } else {
      result <- subset(result, germinationDate >= input$dateRange[1] & germinationDate <= input$dateRange[2])
    }
    result
  })

  output$n_germination <- renderValueBox({
    result <- germinationBox()[,.(sum(numberGerminating)), by = .(femaleGenotype, maleGenotype)]

    box1<-valueBox(value=sum(result$V1),
                   width=1,
                   color = "teal",
                   href="#",
                   subtitle=HTML("<b>Embryo Germinating</b><br>",nrow(result)," Unique crosses" )
    )
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"button_n_germination"
    return(box1)
  })

  observeEvent(input$button_n_germination, {
    observeEvent(input$button_n_seeds, {
      showModal(modalDialog(tags$h2(style="color:#FF6347;text-align:center;","Germinating Embryos"),
                            column(12, offset = 10,
                                   downloadBttn("download_germination",style = "jelly", size = "sm")),
                            DT::dataTableOutput("list_germination"), br(), br(),
                            easyClose = T, size = "l"
      ))
    })
  })
  output$list_germination <- DT::renderDataTable({
    result <- germinationBox()[,pollinationDate:=NULL] %>%
      rename_all(~ str_replace_all(., "([a-z])([A-Z])", "\\1 \\2"))
    colnames(result) <- capitalize(names(result))
    
    DT::datatable(result, filter = 'top', rownames = FALSE, escape = FALSE,
                  options = list(pageLength = 10, lengthMenu = c(5, 10, 50, 100, 500,1000),
                                 searchHighlight=T, stateSave = TRUE))
  })
  downloadGermination <- 
    reactive({
    result <- germinationBox()[,pollinationDate:=NULL] %>%
      rename_all(~ str_replace_all(., "([a-z])([A-Z])", "\\1 \\2"))
    colnames(result) <- capitalize(names(result))

    result <- result[input[["list_germination_rows_all"]],]
    if(!is.null(input$list_germination_rows_selected)){
      result <- result[input$list_germination_rows_selected,]
    }
    result <- result[complete.cases(result$`Cross ID`),]
    result <- janitor::remove_empty(result, "cols")
    return(result)
  })
  
  
  output$download_germination <- downloadHandler(
    filename = function(){paste0(input$site,"-","Germination",Sys.time(),".csv")},
    content = function(file) {
      write.csv(germinationBox(), file, row.names = FALSE)
    }
  )

  # OPEN FIELD
  
  openfieldBox <- reactive({
    result <- data.table::setDT(
      dbGetQuery(pool, paste('SELECT * FROM vw_openfield'))
    )
    if(input$site != "All"){
      result <- result[location == input$site]
    }
    if(input$follow_crosses==TRUE){
      result <- subset(result, pollinationDate >= input$dateRange[1] & pollinationDate <= input$dateRange[2])
    } else {
      result <- subset(result, openfieldTransferDate >= input$dateRange[1] & openfieldTransferDate <= input$dateRange[2])
    }
    result
  })
  
  output$n_openfield_plantlets <- renderValueBox({
    result <- openfieldBox()[,.N, by = .(femaleGenotype, maleGenotype)]

    box1<-valueBox(value=sum(result$N),
                   width=1,
                   color = "teal",
                   href="#",
                   subtitle=HTML("<b>Plants in openfield</b><br>",
                                 nrow(result), " Unique plantlets")
    )
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"button_n_openfield"
    return(box1)
  })

  observeEvent(input$button_n_openfield, {
    observeEvent(input$button_n_seeds, {
      showModal(modalDialog(tags$h2(style="color:#FF6347;text-align:center;","Plants in Openfield"),
                            column(12, offset = 10,
                                   downloadBttn("download_openfield",style = "jelly",  size = "sm")),
                            DT::dataTableOutput("list_openfield"), br(), br(),
                            easyClose = T, size = "l"
      ))
    })
  })
  output$list_openfield <- DT::renderDataTable({
    result <- openfieldBox()[,c('crossID','pollinationDate'):=NULL] %>%
      rename_all(~ str_replace_all(., "([a-z])([A-Z])", "\\1 \\2"))
    colnames(result) <- capitalize(names(result))

    DT::datatable(result, filter = 'top', rownames = FALSE, escape = FALSE,
                  options = list(pageLength = 10, lengthMenu = c(5, 10, 50, 100, 500,1000),
                                 searchHighlight=T, stateSave = TRUE))
  })

  downloadOpenfield <- reactive({
    result <- openfieldBox()[,c('crossID','pollinationDate'):=NULL] %>%
      rename_all(~ str_replace_all(., "([a-z])([A-Z])", "\\1 \\2"))
    colnames(result) <- capitalize(names(result))
    
    result = result[input[["list_openfield_rows_all"]],]

    if(!is.null(input$list_openfield_rows_selected)){
      result <- result[input$list_openfield_rows_selected,]
    }
    result = result[complete.cases(result$`Plantlet ID`),]
    result = janitor::remove_empty(result, "cols")

    return(result)
    })

  output$download_openfield <- downloadHandler(
    filename = function(){paste0(input$site,"-","Plantlets_in_openfield",Sys.time(),".csv")},
    content = function(file) {
      write.csv(downloadOpenfield(), file, row.names = FALSE)
    }
  )
  
  # # GRAPHS
  js_bar_clicked <- JS("function(event) {Shiny.onInputChange('bar_clicked', [event.point.category]);}")
  
  output$totals_site <- renderHighchart({
    req(input$site)
    
    result <- crossesBox() %>%
      dplyr::group_by(location) %>%
      dplyr::tally()  %>%
      collect()
    
    colnames(result)[2]="Number of crosses"
    
    # PLOT POLLINATION
    
    highchart() %>%
      hc_add_series(
        data = result$`Number of crosses`,
        type = "column",
        name = result$location,
        events = list(click = js_bar_clicked)) %>%
      hc_xAxis(
        categories = result$location,
        tickmarkPlacement="on")
  })
  
  output$totals <- renderHighchart({
    req(input$site)
    result <- crossesBox() %>%
      dplyr::mutate(day = lubridate::day(pollinationDate),
                    month = lubridate::month(pollinationDate),
                    year = lubridate::year(pollinationDate))
    
    if((input$dateRange[2] - input$dateRange[1]) < 31){
      result = result %>%
        dplyr::group_by(day) %>%
        dplyr::tally()  %>%
        collect()
      
      grp = result$day
      group_name = "Daily"
    } else if((lubridate::year(input$dateRange[2]) == lubridate::year(input$dateRange[1])) & (input$dateRange[2] - input$dateRange[1]) > 31){
      result = result %>%
        dplyr::group_by(month) %>%
        dplyr::tally()  %>%
        collect()
      
      grp = month.abb[result$month]
      group_name = "Monthly"
    }  else if((lubridate::year(input$dateRange[2]) != lubridate::year(input$dateRange[1]))){
      result  = result %>%
        dplyr::group_by(year) %>%
        dplyr::tally() %>%
        collect()
      
      grp = result$year
      group_name = "Yearly"
    }
    
    colnames(result)[2]="Number of crosses"
    
    # PLOT POLLINATION
    
    highchart() %>%
      hc_add_series(
        data = result$`Number of crosses`,
        type = "column",
        name = group_name,
        events = list(click = js_bar_clicked)) %>%
      hc_xAxis(
        categories = grp,
        tickmarkPlacement="on")
  })
  
  output$totalcrosses <- renderUI({
    result <- crossesBox()
    
    if(nrow(result)==0)return(NULL)
    
    result <- result %>%
      dplyr::summarize(crosses = n())
    paste0("N = ", result)
    
  })

# ---------------------- genotypes
  
  observeEvent(input$overview_showmore,{
  js_female_bar_clicked <- JS("function(event) {Shiny.onInputChange('female_bar_clicked', [event.point.category]);}")

   output$mother <- renderHighchart({

     if(is.null(crossesBox())){return(NULL)}
     else {
     result <- crossesBox() %>%
       dplyr::group_by(femaleGenotype) %>%
       dplyr::tally() %>%
       dplyr::arrange(desc(n)) %>%
       dplyr::collect()
     
     # paste0("<a href='",result$FemaleGermplasmDbId,"'>",'photo',"</a>")
     highchart() %>%
       hc_add_series(data = result$n,type = "bar", name = paste("Female plants"), events = list(click = js_female_bar_clicked)) %>%
       hc_xAxis(categories = result$femaleGenotype) %>%
       hc_exporting(enabled = TRUE) %>%
       hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5", valueDecimals=0,
                  shared = TRUE, borderWidth = 2) %>%
       #hc_title(text=ptitle) %>%
       hc_add_theme(hc_theme_elementary())
     }
   })

   output$totalFemales <- renderUI({
     if(is.null(crossesBox())){return(NULL)}
     else {
     result <- crossesBox() %>%
       dplyr::summarize(dplyr::n_distinct(femaleGenotype))
     paste0("N = ", result)
     }
   })

   # on female click, go to 'Data' tab
   observeEvent(input$female_bar_clicked, {
     updateTabsetPanel(session, "inTabset",
                       selected = "Data")
   })


   #.....................................................................................MALES

   js_male_bar_clicked <- JS("function(event) {Shiny.onInputChange('male_bar_clicked', [event.point.category]);}")

   output$father <- renderHighchart({

     if(is.null(crossesBox())){return(NULL)}
     else {
     result <- crossesBox()%>%
       group_by(maleGenotype) %>%
       dplyr::tally() %>%
       arrange(desc(n)) %>%
       dplyr::collect()

     # plot title
     
     highchart() %>%
       hc_add_series(data = result$n, type = "bar",name = paste("Male plants"),  events = list(click = js_male_bar_clicked)) %>%
       hc_xAxis(categories = result$maleGenotype,tickmarkPlacement="on") %>%
       hc_exporting(enabled = TRUE) %>%
       hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                  shared = TRUE, borderWidth = 2) %>%
       #hc_title(text=ptitle) %>%
       hc_add_theme(hc_theme_elementary())
     }
   })


   output$totalMales <- renderUI({
     if(is.null(crossesBox())){return(NULL)}
     else {
     result <- crossesBox() %>%
       dplyr::summarize(dplyr::n_distinct(maleGenotype))
     paste0("N = ", result)
     }
   })

   # on male click, go to 'Data' tab
   observeEvent(input$male_bar_clicked, {
     updateTabsetPanel(session, "inTabset",
                       selected = "Data")
   })
  })
  })
  })
)
