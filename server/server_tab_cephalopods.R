##############################
# server_tab_cephalopods.R
#
# 
# 
##############################

actionToDoPoulpe <- reactive({
  if("poulpe" %in% input$check_species_ID)
    return("restore")
  else
    return("remove")
})

actionToDoCalmar <- reactive({
  if("calmar" %in% input$check_species_ID)
    return("restore")
  else
    return("remove")
})

actionToDoSeiche <- reactive({
  if("seiche" %in% input$check_species_ID)
    return("restore")
  else
    return("remove")
})

observeEvent(input$check_species_ID, {
  updatebs4Card(id = "carte_distr_peche_card_poulpe", session = session, action = actionToDoPoulpe())
  updatebs4Card(id = "carte_distr_peche_card_calmar", session = session, action = actionToDoCalmar())
  updatebs4Card(id = "carte_distr_peche_card_seiche", session = session, action = actionToDoSeiche())
})

correction_cpue <- eventReactive(input$campagne_or_profession_ID,{
  if(input$campagne_or_profession_ID == "campagne"){
    facteur <- 2* 1.8 # Capturabilité du bateau hauturier et 1.8 celle de charif
                      # Durée de challutage au charif 30 minutes il faut ramener à l'heure
  }else{
    facteur <- 1/(2 * 9) # CPUE hauturier et par jour et chaque jour on fait 9 opérations en moyenne
                        # la durée de chalutage et de 120 minute je divise par deux pour ramener à l'heure
  }
  facteur
})

### Simulation
data_simulation <- eventReactive(input$btn_simulate_cpue_cephalopods,{
  shinyalert(title = "Préparation de Données",
             text = "Running...", 
             type = "warning", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             showCancelButton = F, 
             showConfirmButton = F, 
             immediate = TRUE,
             animation = TRUE)
  
  req(input$check_species_ID)
  req(input$nombre_semaine_peche_ID)
  req(input$debut_fin_saison_ID)
  req(correction_cpue())
  
  facteur <- correction_cpue()
  
  species <- input$check_species_ID
  nombre_semaine_peche <- as.numeric(input$nombre_semaine_peche_ID)
  
  date_start <- input$debut_fin_saison_ID[1]
  date_end <- input$debut_fin_saison_ID[2]
  date_start_date <- as.Date(date_start, origin = "1970-01-01")
  date_end_date <- as.Date(date_end, origin = "1970-01-01")
  
  date_oceano_limit_past <- date_start_date - 6*30
  dates <- seq.Date(from = date_start_date,to = date_end_date,by = 7)
  
  conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
  fishing_zone <- DBI::dbGetQuery(conn = conn,"select * from cephalopod_fishing_zone") %>%
    dplyr::filter(LAT <= 26.5, LAT >= 21)
  
  DBI::dbDisconnect(conn = conn)
  
  data_tbl <- data.frame(WEEK = numeric(),LON = numeric(),LAT = numeric(),DEPTH = numeric())
  for(i in 1:nombre_semaine_peche){
    d <- fishing_zone %>%
      mutate(WEEK = i, DATE = dates[i])
    data_tbl <- data_tbl %>%
      bind_rows(d)
  }
  
  data_tbl$SeasonProgress <- data_tbl$WEEK / nombre_semaine_peche
  
  if("poulpe" %in% species){
    poulpeSelected <- TRUE
    data_tbl$Week_CPUE_Lag_Poulpe <- (as.numeric(input$poulpe_last_week_cpue_ID) * facteur)^0.275
  }else{
    poulpeSelected <- FALSE
  }
  
  if("calmar" %in% species){
    calmarSelected <- TRUE
    data_tbl$Week_CPUE_Lag_Calmar <- (as.numeric(input$calmar_last_week_cpue_ID) * facteur)^0.075
  }else{
    calmarSelected <- FALSE
  }
  
  if("seiche" %in% species){
    seicheSelected <- TRUE
    data_tbl$Week_CPUE_Lag_Seiche <- (as.numeric(input$seiche_last_week_cpue_ID) * facteur)^0.3
  }else{
    seicheSelected <- FALSE
  }
  
  data_tbl_oceano <- add_oceano_to_cephalopod(data_tbl)
  
  data_list <- list("data" = data_tbl_oceano, 
                    "poulpeSelected"=poulpeSelected,
                    "calmarSelected"=calmarSelected,
                    "seicheSelected"=seicheSelected,
                    "nWeeks" = nombre_semaine_peche)
  data_list
})

### PREDICTION

predictions_cephalopods <- eventReactive(input$btn_simulate_cpue_cephalopods, {
  
  req(data_simulation())
  data_list <- data_simulation()
  shinyalert(title = "Prédiction des CPUE",
             text = "Running...", 
             type = "warning", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             showCancelButton = F, 
             showConfirmButton = F, 
             immediate = TRUE,
             animation = TRUE)
  
  
  res <- predict_fishing_cephalopods(data_list)
  
  data_list[["cpue_predictions_poulpe"]] <- res[["poulpe"]]
  data_list[["cpue_predictions_calmar"]] <- res[["calmar"]]
  data_list[["cpue_predictions_seiche"]] <- res[["seiche"]]
  return(data_list)
})


### PLOTTING CPUES

observeEvent(input$btn_simulate_cpue_cephalopods,{

  shinyalert(title = "Prédiction des CPUE",
             text = "Running...", 
             type = "warning", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             showCancelButton = F, 
             showConfirmButton = F, 
             immediate = TRUE,
             animation = TRUE)

  req(data_simulation())
  req(predictions_cephalopods())
  cpue_data_tbl <- predictions_cephalopods()
  
  if(cpue_data_tbl$poulpeSelected){
    for (i in 1:cpue_data_tbl$nWeeks) {
      local({
        my_i <- i
        cpue_week <- cpue_data_tbl$cpue_predictions_poulpe %>% dplyr::filter(week == my_i)
        week <- cpue_week$date[1]
        plotname <- paste("cpue_poulpe_plots_ui", my_i, sep="")
        
        output[[plotname]] <- renderPlot({
          cpue_week %>%
            ggplot2::ggplot() +
            ggplot2::theme_bw() +
            ggplot2::geom_tile(aes(lon, lat, fill = pred_weight_t)) +
            ggplot2::scale_fill_viridis_c(option = "B", direction = -1) +
            # only data within the data-bounds
            ggplot2::borders(database = "worldHires",xlim = range(cpue_week$lon), ylim = range(cpue_week$lat),fill = "antiquewhite") + 
            # limit plot
            ggplot2::coord_quickmap(xlim = range(cpue_week$lon), ylim = range(cpue_week$lat)) +
            ggtitle(label = paste("CPUE SIMULÉE - SEMAINE N°",my_i," -- ",week)) +
            labs(fill = "CPUE (Kg/Heure)")
        },height = 700, width = 550) 
      })
    }
  }
  
  if(cpue_data_tbl$calmarSelected){
    for (i in 1:cpue_data_tbl$nWeeks) {
      local({
        my_i <- i
        cpue_week <- cpue_data_tbl$cpue_predictions_calmar %>% dplyr::filter(week == my_i)
        week <- cpue_week$date[1]
        plotname <- paste("cpue_calmar_plots_ui", my_i, sep="")
        
        output[[plotname]] <- renderPlot({
          cpue_week %>%
            ggplot2::ggplot() +
            ggplot2::theme_bw() +
            ggplot2::geom_tile(aes(lon, lat, fill = pred_weight_t)) +
            ggplot2::scale_fill_viridis_c(option = "B", direction = -1) +
            # only data within the data-bounds
            ggplot2::borders(database = "worldHires",xlim = range(cpue_week$lon), ylim = range(cpue_week$lat),fill = "antiquewhite") + 
            # limit plot
            ggplot2::coord_quickmap(xlim = range(cpue_week$lon), ylim = range(cpue_week$lat)) +
            ggtitle(label = paste("CPUE SIMULÉE - SEMAINE N°",my_i," -- ",week)) +
            labs(fill = "CPUE (Kg/Heure)")
        },height = 700, width = 550) 
      })
    }
  }
  
  if(cpue_data_tbl$seicheSelected){
    for (i in 1:cpue_data_tbl$nWeeks) {
      local({
        my_i <- i
        cpue_week <- cpue_data_tbl$cpue_predictions_seiche %>% dplyr::filter(week == my_i)
        week <- cpue_week$date[1]
        plotname <- paste("cpue_seiche_plots_ui", my_i, sep="")
        
        output[[plotname]] <- renderPlot({
          cpue_week %>%
            ggplot2::ggplot() +
            ggplot2::theme_bw() +
            ggplot2::geom_tile(aes(lon, lat, fill = pred_weight_t)) +
            ggplot2::scale_fill_viridis_c(option = "B", direction = -1) +
            # only data within the data-bounds
            ggplot2::borders(database = "worldHires",xlim = range(cpue_week$lon), ylim = range(cpue_week$lat),fill = "antiquewhite") + 
            # limit plot
            ggplot2::coord_quickmap(xlim = range(cpue_week$lon), ylim = range(cpue_week$lat)) +
            ggtitle(label = paste("CPUE SIMULÉE - SEMAINE N°",my_i," -- ",week)) +
            labs(fill = "CPUE (Kg/Heure)")
        },height = 700, width = 550) 
      })
    }
  }
  
  shinyalert(title = "Fin de Prédiction",
             text = "",
             type = "success",
             closeOnEsc = F,
             closeOnClickOutside = F,
             showCancelButton = F,
             showConfirmButton = TRUE,
             immediate = TRUE,
             animation = TRUE)
  
})

output$cpue_poulpe_plots_ui <- renderUI({
  plot_output_list <- lapply(1:predictions_cephalopods()$nWeeks, function(i) {
    plotname <- paste("cpue_poulpe_plots_ui", i, sep="")
    list(
      div(
        plotOutput(plotname, height = 700, width = 550),
        style = "display:inline-block;"
      )
    )
  })
  # Convert the list to a tagList - this is necessary for the list of items
  # to display properly.
  do.call(tagList, plot_output_list)
})

output$cpue_calmar_plots_ui <- renderUI({
  plot_output_list <- lapply(1:predictions_cephalopods()$nWeeks, function(i) {
    plotname <- paste("cpue_calmar_plots_ui", i, sep="")
    list(
      div(
        plotOutput(plotname, height = 700, width = 550),
        style = "display:inline-block;"
      )
    )
  })
  # Convert the list to a tagList - this is necessary for the list of items
  # to display properly.
  do.call(tagList, plot_output_list)
})

output$cpue_seiche_plots_ui <- renderUI({
  plot_output_list <- lapply(1:predictions_cephalopods()$nWeeks, function(i) {
    plotname <- paste("cpue_seiche_plots_ui", i, sep="")
    list(
      div(
        plotOutput(plotname, height = 700, width = 550),
        style = "display:inline-block;"
      )
    )
  })
  # Convert the list to a tagList - this is necessary for the list of items
  # to display properly.
  do.call(tagList, plot_output_list)
})