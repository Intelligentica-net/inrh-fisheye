##############################
# server_tab_interaction.R
#
# 
# 
##############################

data_prediction <- eventReactive(input$btn_predict_attaque_negro,{
  req(input$periode_attaque)
  
  shinyalert(title = "Prédiction des Attaques",
             text = "Predicting...",
             type = "warning",
             closeOnEsc = F,
             closeOnClickOutside = F,
             showCancelButton = F,
             showConfirmButton = F,
             immediate = TRUE,
             animation = TRUE)
  
  date_start <- input$periode_attaque[1]
  date_end <- input$periode_attaque[2]
  date_start_date <- as.Date(date_start, origin = "1970-01-01")
  date_end_date <- as.Date(date_end, origin = "1970-01-01")
  
  seqDate <- seq.Date(from = date_start_date,to = date_end_date,by = 1)
  
  conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
  zone <- dbGetQuery(conn = conn,statement = "select * from interaction_med_zone")
  DBI::dbDisconnect(conn = conn)
  
  longSeqDate <- rep(seqDate,each = nrow(zone))
  data <- zone[0,]
  for(i in 1:length(seqDate)){
    data <- data %>%
      dplyr::bind_rows(zone)
  }
  data$Date <- longSeqDate
  
  data <- data %>%
    dplyr::rename(DATE = Date) %>%
    dplyr::mutate(Date = grade_date(x = (DATE-0*7),dx = 8)) %>%
    dplyr::group_by(Date,Latitude,Longitude) %>%
    dplyr::summarise(DEPTH = mean(DEPTH,na.rm = TRUE)) %>%
    dplyr::ungroup()
  
  zone_oceano <- add_oceano_negro(data)
  
  predictions_list <- predictAttaqueNegro(zone_oceano)
  
  shinyalert(title = "Fin de prédiction",
             text = "",
             type = "success",
             closeOnEsc = F,
             closeOnClickOutside = F,
             showCancelButton = F,
             showConfirmButton = TRUE,
             immediate = TRUE,
             animation = TRUE)
  predictions_list
})

### PLOTTING

observeEvent(input$btn_predict_attaque_negro,{
  req(data_prediction())
  predictions <- data_prediction()
  
  for (i in 1:predictions$Nweeks) {
    local({
      my_i <- i
      data_week <- predictions$data %>% dplyr::filter(DATE == predictions$weeks[my_i])
      week <- predictions$weeks[my_i]
      plotname <- paste("attaque_probability_plots_ui", my_i, sep="")
      
      output[[plotname]] <- renderPlot({
        data_week %>%
          ggplot2::ggplot() +
          ggplot2::theme_bw() +
          ggplot2::geom_tile(aes(LON, LAT, fill = Pred_Attaque)) +
          ggplot2::scale_fill_viridis_c(option = "B", direction = -1) +
          # only data within the data-bounds
          ggplot2::borders(database = "worldHires",xlim = range(data_week$LON), ylim = range(data_week$LAT),fill = "antiquewhite") + 
          # limit plot
          ggplot2::coord_quickmap(xlim = range(data_week$LON), ylim = range(data_week$LAT)) +
          ggtitle(label = paste("ATTAQUE PRÉDITE - SEMAINE N°",my_i," -- ",week)) +
          labs(fill = "Probabilité d'attaque" )
      },height = 400, width = 825) # height = 300, width = 550
    })
  }
})

output$attaque_probability_plots_ui <- renderUI({
  plot_output_list <- lapply(1:data_prediction()$Nweeks, function(i) {
    plotname <- paste("attaque_probability_plots_ui", i, sep="")
    list(
      div(
        plotOutput(plotname, height = 400, width = 825),
        style = "display:inline-block;"
      )
    )
  })
  # Convert the list to a tagList - this is necessary for the list of items
  # to display properly.
  do.call(tagList, plot_output_list)
})
