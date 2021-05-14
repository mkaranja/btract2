source("data_prep/other_labels.R", local = T)

tc_server <- function(env_serv) with(env_serv, local({
  output$user <- reactive({session$user})

  tc_data <- reactive({
    switch(input$tc_tabs,
           'Crosses (Embryo Rescue)' = banana %>% 
             dplyr::select(Location, Crossnumber, `Embryo Rescue Date`, `Number of Embryo Rescued`) %>%
             dplyr::filter(`Number of Embryo Rescued` > 0) %>%
             dplyr::rename(Embryo_Rescue_Date = 'Embryo Rescue Date',
                           Number_of_Embryo_Rescued = 'Number of Embryo Rescued'),
           'Embryo Germinating' = germinating_embryos(),
           'Subcultures' = subcultures(),
           "Rooting" = rooting(),
           "Weaning 1/ Sending Out" = weaning1(),
           "Weaning 2" = weaning2(),
           "Screenhouse Transfer" = screenhouse(),
           "Hardening" = hardening(),
           "Openfield" = openfield()
    )
  })
  
  #  ---------- update inputs ----------------------
  observe({
    sites <- tc_data()
    updateSelectInput(session, 'tc_site', 'SITE',
                      choices = c(levels(sites$Location))
    )
  })

  observe({
    req(input$tc_site)
    
    dt <- tc_data() %>%
        dplyr::filter(Location %in% input$tc_site)%>%
        as.data.frame()
      
      col <- grep("Date", names(dt), value=T)
      col1 <- col %>%
        str_replace_all(., "([a-z])([A-Z])", "\\1 \\2") %>%
        capitalize()
      
      d <- unique(na.omit(as.Date(dt[,col])))
      startdate <- max(d)-30  
      updateDateRangeInput(session, "tcDateRange", 
                           label = paste(gsub("_"," ",col1)),
                           start = min(d), 
                           end = max(d),
                           min = min(d), 
                           max = max(d)
      )
    
    # dt <- tc_data() %>%
    #   dplyr::filter(Location %in% input$tc_site)
    # 
    # col <- grep("Date", names(banana), value=T)
    # d1 <- dt %>% 
    #   dplyr::select(col) %>% 
    #   unique()
    # 
    # d1[,col] <- lubridate::ymd(d1[,col])
    # startdate <- max(d)-30  
    # updateDateRangeInput(session, "tcDateRange", 
    #                      label = paste(gsub("_"," ",col1)),
    #                      start = min(d), 
    #                      end = max(d),
    #                      min = min(d), 
    #                      max = max(d)
    # )
  })
  
  
  
  # ----filtered data---------------------------------------
  
  tc_data_sel1 <- reactive({
    req(input$tc_site)
    req(input$tcDateRange)
    
    dt <- tc_data() %>%
      dplyr::filter(Location %in% input$tc_site)%>%
      as.data.frame()
    
    col <- grep("Date", names(dt), value = T)
    
    dt <- dt[(dt[,col] >= input$tcDateRange[1] & dt[,col] <= input$tcDateRange[2]),] 
    dt[order(dt[col], decreasing = TRUE),]  
  })
  
  observe({
    req(input$tc_tabs)
    
    if(input$tc_tabs == "Crosses (Embryo Rescue)"){
      id <- tc_data_sel1()$Crossnumber
    }else {
      id <- tc_data_sel1()$PlantletID
    }
    
    updateSelectizeInput(session, "scan_ids", 
                         "Scan to select specific IDs",
                         choices = c('', id))
    
  })
  
  # scanned ids
  tc_data_sel <- reactive({
    dt <- tc_data_sel1()
    if(length(input$scan_ids)>0){
      if(input$tc_tabs == 'Crosses (Embryo Rescue)'){
        dt <- dt %>% 
          dplyr::filter(Crossnumber %in% input$scan_ids)
      }else {
        dt <- dt %>% 
          dplyr::filter(PlantletID %in% input$scan_ids)
      }
    }
    dt
  })
  
  # ----------- Table Output ---------------------
  output$tc_dt <- renderDT(
	tc_data_sel(), options = list(lengtns = list(pageLength = 10,
                                 lengthMenu = c(10, 50, 100, 500,1000),
                                 searchHighlight=T, 
                                 stateSave = TRUE)
  )
)  
  
  # -------------- Download -----------------------
  
  gen_tc_labels <- reactive({
    dt <- tc_data_sel()
    
    # selected rows
    s <- input$tc_dt_rows_selected
    if(length(s)){
      dt <- dt[s,]
    }
    
    # get column with number of plants
    col <- grep("Number", names(dt))
    
    # single plants labels
    if(input$tc_tabs == "Embryo Germinating" || input$number_per_tube == 'single plant'){
      dt <- purrr::map_df(seq_len(input$number_of_copies), ~dt) %>% # repricate
        .[order(.[,1]),] 
    } else {
        # calculate labels for 3 plants per test tube
        if(input$number_per_tube == "3 plants/test tube"){
          for(i in 1:nrow(dt)){
            if(dt$Number[i]>3){
              dt$n[i] <- ceiling((dt$Number[i])/3)
            } else{
              dt$n[i] <- 1
            }
          }
        } else if(input$number_per_tube == "6 plants/test tube"){ # labels for 6 labels per test tube
          for(i in 1:nrow(dt)){
            if(dt$Number[i]>6){
              dt$n[i] <- ceiling((dt$Number[i])/6)
            } else{
              dt$n[i] <- 1
            }
          }
        } 
      # 
      dt <- dt %>% 
        tidyr::uncount(n)
    }
    # formats for different sections/ activities
    if(input$tc_tabs == 'Crosses (Embryo Rescue)'){
      df = data.frame(stringr::str_split_fixed(dt$Crossnumber,"_",2))
      colnames(df) = c('Prefix','Suffix')
      df$Suffix = gsub("[)]","",(gsub("[(]","", df$Suffix)))
      result = cbind(dt,df) %>% 
        dplyr::select(Crossnumber, Prefix, Suffix)
    } else {
      df = data.frame(stringr::str_split_fixed(dt$PlantletID,"_",3))
      colnames(df) = c('Prefix','Suffix',"EmbryoNo")
      df$Suffix = gsub("[)]","",(gsub("[(]","", df$Suffix)))
      result = cbind(dt,df) %>% 
        dplyr::select(PlantletID, Prefix, Suffix, EmbryoNo)
    }
    result
  })
  
  # on click download
  output$tc_download <- downloadHandler(
    filename = function(){
      paste0(input$tc_site,"-", input$tc_tabs,"-", Sys.Date(),".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(gen_tc_labels(), file)
    }
  )
 
}))
