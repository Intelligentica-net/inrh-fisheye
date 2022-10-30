##############################
# server_tab_pelagics_cpue.R
#
#
#
##############################

actionToDoCPUE <- reactive({
  if(input$prediction_type_ID == "catch")
    return("restore")
  else
    return("remove")
})


observeEvent(input$prediction_type_ID, {
  updatebs4Card(id = "graph_capture_par_espece_semaine_card", session = session, action = actionToDoCPUE())
  updatebs4Card(id = "graph_capture_mensuelle_card", session = session, action = actionToDoCPUE())
})

### Prediction of CPUE
data_cpue <- shiny::eventReactive(input$btn_run_fishing_prediction_ID, {
  data_list <- list()
  if(input$prediction_type_ID == "cpue"){
    req(input$cpue_simulation_year_ID)
    req(input$cpue_simulation_months_ID)
    
    sim_year <- as.numeric(input$cpue_simulation_year_ID)
    sim_months <- as.numeric(input$cpue_simulation_months_ID)
    
    if("Sardina_pilchardus" %in% input$species_ID){
      sardine_last_month_catch <- as.numeric(input$sardine_last_month_catch_ID)
      sardine_selected <- TRUE
    }else{
      sardine_last_month_catch <- -1
      sardine_selected <- FALSE
    }
    
    if("Scomber_colias" %in% input$species_ID){
      maquereau_last_month_catch <- as.numeric(input$maquereau_last_month_catch_ID)
      maquereau_selected <- TRUE
    }else{
      maquereau_last_month_catch <- -1
      maquereau_selected <- FALSE
    }
    
    if("Trachurus_trachurus" %in% input$species_ID){
      chinchard_last_month_catch <- as.numeric(input$chinchard_last_month_catch_ID)
      chinchard_selected <- TRUE
    }else{
      chinchard_last_month_catch <- -1
      chinchard_selected <- FALSE
    }
    
    coordinates <- create_effort_data_pelagics_cpue(sim_year = sim_year, sim_months = sim_months)
    data_with_oceano <- add_oceano_to_coordinates(coordinates = coordinates)
    
    weeks <- unique(data_with_oceano$DATE) %>% sort()
    Nweeks <- length(weeks)
    
    data_list <- list("data" = data_with_oceano,
                      "sardine_last_month_catch" = sardine_last_month_catch,
                      "sardine_selected" = sardine_selected,
                      "maquereau_last_month_catch" = maquereau_last_month_catch,
                      "maquereau_selected" = maquereau_selected,
                      "chinchard_last_month_catch" = chinchard_last_month_catch,
                      "chinchard_selected" = chinchard_selected,
                      "sim_months" = sim_months,
                      "weeks" = weeks,
                      "Nweeks" = Nweeks,
                      "type" = "cpue")
  }
  return(data_list)
})


cpue_predictions <- shiny::eventReactive(input$btn_run_fishing_prediction_ID, {
  if(input$prediction_type_ID == "cpue"){
    
    shinyalert(title = "Préparation de données...",
               text = "PRÉDICTION DES CPUEs",
               type = "warning",
               closeOnEsc = F,
               closeOnClickOutside = F,
               showCancelButton = F,
               showConfirmButton = F,
               immediate = TRUE,
               animation = TRUE)
    
    req(data_cpue())
    data_list <- data_cpue()
    
    shinyalert(title = "Prédiction des CPUEs...",
               text = "PRÉDICTION DES CPUEs",
               type = "warning",
               closeOnEsc = F,
               closeOnClickOutside = F,
               showCancelButton = F,
               showConfirmButton = F,
               immediate = TRUE,
               animation = TRUE)
    
    predictions <- predict_pelagics(data_list)
    
    shinyalert(title = "Terminé!",
               text = "PRÉDICTION DES CPUEs",
               type = "success",
               closeOnEsc = TRUE,
               closeOnClickOutside = TRUE,
               showCancelButton = F,
               showConfirmButton = TRUE,
               immediate = TRUE,
               animation = TRUE)
    
    return(predictions)
    
  }
})