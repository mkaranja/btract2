
tc_server <- function(env_serv) with(env_serv, local({
  output$user <- reactive({session$user})
  
  tc_data <- reactive({
    switch(input$tc_tabs,
           'Crosses (Embryo Rescue)' = pool %>% 
             tbl('vw_labels_embryoRescue') %>%
             dplyr::filter(numberRescued > 0)  %>%
             collect() %>% 
             mutate_if(is.character, as.factor),
           
           'Embryo Germinating' = tbl(pool, 'vw_labels_germinatingEmbryo') %>% 
             collect() %>% 
             mutate_if(is.character, as.factor),
           
           'Subcultures' = tbl(pool, 'vw_labels_subcultures') %>% 
             collect() %>% 
             mutate_if(is.character, as.factor),
           
           "Rooting" = tbl(pool, 'vw_labels_rooting') %>% 
             collect() %>% 
             mutate_if(is.character, as.factor),
           
           "Weaning 1/ Sending Out" = tbl(pool, 'vw_labels_weaning1') %>% 
             collect() %>% 
             mutate_if(is.character, as.factor),
           
           "Weaning 2" = tbl(pool, 'vw_labels_weaning2') %>% 
             collect() %>% 
             mutate_if(is.character, as.factor),
           "Screenhouse Transfer" = tbl(pool, 'vw_labels_screenhouse') %>%
             collect() %>% 
             mutate_if(is.character, as.factor),
           
           "Hardening" = tbl(pool, 'vw_labels_hardening') %>% 
             collect() %>% 
             mutate_if(is.character, as.factor),
           
           "Openfield" = tbl(pool, 'vw_labels_openfield') %>% 
             collect() %>% 
             mutate_if(is.character, as.factor)
    )
  })
  
  #  ---------- update inputs ----------------------
  observe({
    sites <- tc_data()
    updateSelectInput(session, 'tc_site', 'SITE',
                      choices = c("None", levels(sites$location))
    )
  })
  
  observe({
    req(input$tc_site)
    
    if(input$tc_site =='None'){
      return(NULL)
    }else {
      dt <- tc_data() %>%
        dplyr::filter(location %in% input$tc_site)%>%
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
    }
  })
  
  # ----filtered data---------------------------------------
  
  
  tc_data_sel1 <- reactive({
    req(input$tc_site)
    req(input$tcDateRange)
    
    dt <- tc_data()
    
    if(input$tc_site !='None'){
      
      dt <- dt %>%
        dplyr::filter(location %in% input$tc_site)
    }
    dt <- as.data.frame(dt)
    
    col <- grep("Date", names(dt))
    col_name <- names(dt[,col])
    
    dt <- dt[(dt[,col] >= input$tcDateRange[1] & dt[,col] <= input$tcDateRange[2]),] 
    dt[order(dt[col], decreasing = TRUE),]  
  })
  
  observe({
    req(input$tc_tabs)
    dt <- tc_data_sel1()
    
    if(input$tc_tabs == 'Crosses (Embryo Rescue)'){
      ids <- levels(dt$crossID)
    }else {
      ids <- levels(dt$plantletID)
    }
    
    updateSelectizeInput(session, "scan_ids", 
                         "Scan to select specific IDs",
                         choices = c('', ids),
                         server = TRUE)
  })
  
  # scanned ids
  tc_data_sel <- reactive({
    dt <- tc_data_sel1() 
    
    if(length(input$scan_ids)>0){
      if(input$tc_tabs == 'Crosses (Embryo Rescue)'){
        dt <- dt %>% 
          dplyr::filter(crossID %in% input$scan_ids)
      }else {
        dt <- dt %>% 
          dplyr::filter(plantletID %in% input$scan_ids)
      }
    }
    dt <- dt %>%
      rename_all(~ str_replace_all(., "([a-z])([A-Z])", "\\1 \\2"))
    colnames(dt) <- capitalize(names(dt))
    dt
  })
  
  # ----------- Table Output ---------------------
  
  observeEvent(input$label_show1, {
    shinyjs::hide("label_show_id1")
    
    output$tc_dt <- renderDT({
      
      DT::datatable(tc_data_sel(), 
                    filter = 'top', 
                    rownames = FALSE, 
                    escape = FALSE, 
                    options = list(pageLength = 10,
                                   lengthMenu = c(10, 50, 100, 500,1000),
                                   searchHighlight=T, 
                                   stateSave = TRUE)
      )
    })  
    
    # -------------- Download -----------------------
    
    gen_tc_labels <- reactive({
      dt <- tc_data_sel()
      colnames(dt) <- gsub(" ","", names(dt))
      # selected rows
      s <- input$tc_dt_rows_selected
      if(length(s)){
        dt <- dt[s,]
      }
      # rename column with number of plants
      col <- grep("Number", names(dt))
      colnames(dt)[col] <- 'Number'
      
      # single plants labels
      if(input$tc_tabs == "Embryo Germinating" || input$number_per_tube == 'single barcode'){
        dt <- purrr::map_df(seq_len(input$number_of_copies), ~dt) %>% # repricate
          .[order(.[,1]),]
      } else {
        # calculate labels for 3 plants per test tube
        if(input$number_per_tube == "3 plants/test tube"){
          for(i in 1:nrow(dt)){
            dt$n[i] <- 1
            if(dt$Number[i]>3){
              dt$n[i] <- ceiling((dt$Number[i])/3)
            }
          }
        } else if(input$number_per_tube == "6 plants/test tube"){ # labels for 6 labels per test tube
          for(i in 1:nrow(dt)){
            dt$n[i] <- 1
            if(dt$Number[i]>6){
              dt$n[i] <- ceiling((dt$Number[i])/6)
            }
          }
        }
        dt <- dt %>%
          tidyr::uncount(n)
      }
      dt
    })
    
    label_formats <- reactive({
      dt <- gen_tc_labels()
      
      # formats for different sections/ activities
      if(input$tc_tabs == 'Crosses (Embryo Rescue)'){
        df <- data.frame(stringr::str_split_fixed(dt$CrossID,"_",2))
        colnames(df) <- c('Prefix','Suffix')
        df$Suffix <- gsub("[)]","",(gsub("[(]","", df$Suffix)))
        result <- cbind(dt,df) %>%
          dplyr::rename('Crossnumber' = 'CrossID') %>%
          dplyr::select(Crossnumber, Prefix, Suffix)
      } else {
        df <- data.frame(stringr::str_split_fixed(dt$PlantletID,"_",3))
        colnames(df) <- c('Prefix','Suffix',"EmbryoNo")
        df$Suffix <- gsub("[)]","",(gsub("[(]","", df$Suffix)))
        result <- cbind(dt,df) %>%
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
        writexl::write_xlsx(label_formats(), file)
      }
    )
  })
}))
