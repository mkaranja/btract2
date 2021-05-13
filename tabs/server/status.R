library(glue)

## CURRENT STATUS +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

observe({
  sites <- dbGetQuery(pool, paste('SELECT * FROM location'))
  updateSelectInput(session, "current_site", "Site", choices = c("None", sites$location) )
})


observeEvent(input$show_current_activities1,{
             if(input$current_site == 'None'){
               shinyalert::shinyalert("", "Please select a specific site above", type = "warning")
             }else {
               shinyjs::hide("show_current_activities")
             }
})


# load data

status_clean <- reactive({
  req(input$current_site)
  
  if(input$current_site == 'None'){
    return(NULL)
  }else {
    loc = input$current_site
    activities = c("Flowering","First Pollination","Repeat Pollination","Bunch Harvesting","Harvested bunches","Seed Extraction","Embryo Rescue","Embryo Germination")
    
    sql = glue_sql("SELECT * FROM vw_1_CleanTable WHERE location = {loc} ORDER BY DateDone DESC",
                   loc = loc, .con = pool)
    result <- DBI::dbGetQuery(pool, sql)
    result <- result[result$Number > 0, ]
    result[!duplicated(result$ID),]
  }
})


# update chart

output$current_Activities <- renderHighchart({
  # Make the initial data.
  activities <- c("Flowering","First Pollination","Repeat Pollination","Bunch Harvesting","Harvested bunches","Seed Extraction","Embryo Rescue","Embryo Germination")
  p <- data.table(table(status_clean()$Activity)) %>% 
    filter(N >0)
  
  summarized <- arrange(tibble(name = p$V1, y = p$N),desc(y))
  
  tibbled <- left_join(data.frame(name=activities), summarized ,by="name")
  tibbled <- tibbled[complete.cases(tibbled),]
  
  # summarized <- status_clean() %>%
  #   dplyr::group_by(Activity) %>%
  #   dplyr::summarize(N = n())
  # 
  # summarized <- arrange(summarized, desc(N))
  # tibbled <- tibble(name = summarized$Activity, y = summarized$N)
  
  # This time, click handler is needed.
  drilldownHandler <- JS("function(event) {Shiny.onInputChange('ClickedInput', event.point.drilldown);}")
  
  # Also a message receiver for later async drilldown data has to be set.
  # Note in the JS: message.point is going to be the point ID. Highcharts addSeriesAsDrilldown need a point to attach
  #   the drilldown series to. This is retrieved via chart.get which takes the ID of any Highcharts Element.
  #   This means: IDs are kind of important here, so keep track of what you assign.
  installDrilldownReceiver <- JS("function() {
                                  var chart = this;
                                  Shiny.addCustomMessageHandler('drilldown', function(message) {
                                    var point = chart.get(message.point)
                                    chart.addSeriesAsDrilldown(point, message.series);
                                  });
                                }")
  
  highchart() %>%
    # Both events are on the chart layer, not by series. 
    hc_chart(events = list(load = installDrilldownReceiver, drilldown = drilldownHandler)) %>%
    hc_xAxis(type = "category") %>%
    # Note: We add a drilldown directive (= name) to tell Highcharts that this has a drilldown functionality.
    hc_add_series(tibbled, "column", hcaes(x = name, y = y, drilldown = name, id = name), color = "#E4551F") %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_drilldown(allowPointDrilldown = TRUE)
  
  
})

# show details of clicked bar
observeEvent(input$ClickedInput, {
  sub_data <- status_clean()[status_clean()$Activity == input$ClickedInput,]
  # colnames(sub_data)["DateDone"] <- "Date"
  rownames(sub_data) <- sub_data$ID
  sub_data[,c("Location", "Activity")] <- NULL
  output$current_Table <- renderDT({
    
    DT::datatable(sub_data, 
                  filter = 'top', 
                  rownames = FALSE, 
                  escape = FALSE, 
                  options = list(pageLength = 10,
                                 lengthMenu = c(10, 50, 100, 500,1000),
                                 searchHighlight=T, 
                                 stateSave = TRUE)
    )
  })
})

## SCHEDULE ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

observe({
  sites <- dbGetQuery(pool, paste('SELECT * FROM location'))
  updateSelectizeInput(session, "schedule_site",label = "Site:",choices = c('None', sites$location))
  
})
observeEvent(input$show_schedule1,{
  if(input$current_site == 'None'){
    shinyalert::shinyalert("", "Please select a specific site", type = "warning")
  }else {
    shinyjs::hide("show_schedule")
  }
})

output$overdue_site <- renderText({
  input$schedule_site
})


