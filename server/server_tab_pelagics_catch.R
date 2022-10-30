##############################
# server_tab_pelagics_catch.R
#
#
#
##############################
output$tot_catch_sardine <- renderValueBox({
  shiny::req(catch_predictions())
  val <- round(sum(catch_predictions()$catch_predictions$sardine$pred_weight_t))
  valueBoxSpark(
    value = format(val, big.mark = ","),
    title = toupper("CAPTURE TOTALE DE SARDINE EN TONNES"),
    sparkobj = NULL,
    subtitle = NULL,
    info = "",
    icon = icon("weight",lib = "font-awesome"),
    width = 3,
    color = "aqua",
    href = NULL
  )
})
  
output$tot_catch_maquerel <- renderValueBox({
  shiny::req(catch_predictions())
  val <- round(sum(catch_predictions()$catch_predictions$maquereau$pred_weight_t))
  valueBoxSpark(
    value = format(val, big.mark = ","),
    title = toupper("CAPTURE TOTALE DE MAQUEREAU EN TONNES"),
    sparkobj = NULL,
    subtitle = NULL,
    info = "",
    icon = icon("weight",lib = "font-awesome"),
    width = 3,
    color = "aqua",
    href = NULL
  )
})
  

output$tot_catch_chinchard <- renderValueBox({
  shiny::req(catch_predictions())
  val <- round(sum(catch_predictions()$catch_predictions$chinchard$pred_weight_t))
  valueBoxSpark(
    value = format(val, big.mark = ","),
    title = toupper("CAPTURE TOTALE DE CHINCHARD EN TONNES"),
    sparkobj = NULL,
    subtitle = NULL,
    info = "",
    icon = icon("weight",lib = "font-awesome"),
    width = 3,
    color = "aqua",
    href = NULL
  )
})
  

output$tot_chiffre_affaire <- renderValueBox({
  shiny::req(catch_predictions())
  val1 <- sum(catch_predictions()$catch_predictions$chinchard$pred_weight_t)
  val2 <- sum(catch_predictions()$catch_predictions$maquereau$pred_weight_t)
  val3 <- sum(catch_predictions()$catch_predictions$sardine$pred_weight_t)
  val <- round(((val1 + val2 + val3)*1843)/1000000,digits = 1)
  valueBoxSpark(
    value = format(val, big.mark = ","),
    title = toupper("CHIFFRE D'AFFAIRE EN MILLIONS DE DIRHAMS"),
    sparkobj = NULL,
    subtitle = NULL,
    info = "",
    icon = icon("euro-sign",lib = "font-awesome"),
    width = 3,
    color = "aqua",
    href = NULL
  )
})

### Prediction of CPUE to be used as environmental preference for fishing effort distribution
### Observe "Simulate Button" clicked
data_cpue_for_catch <- shiny::eventReactive(input$btn_simulate_effort_ID, {
  data_list <- list()
  if(input$prediction_type_ID == "catch"){
    req(input$catch_simulation_year_ID)
    req(input$catch_simulation_months_ID)
    
    sim_year <- as.numeric(input$catch_simulation_year_ID)
    sim_months <- as.numeric(input$catch_simulation_months_ID)
    
    if("Sardina_pilchardus" == input$rb_main_species_ID){
      sardine_last_month_catch <- 0
      sardine_selected <- TRUE
    }else{
      sardine_last_month_catch <- -1
      sardine_selected <- FALSE
    }
    
    if("Scomber_colias" == input$rb_main_species_ID){
      maquereau_last_month_catch <- 0
      maquereau_selected <- TRUE
    }else{
      maquereau_last_month_catch <- -1
      maquereau_selected <- FALSE
    }
    
    if("Trachurus_trachurus" == input$rb_main_species_ID){
      chinchard_last_month_catch <- 0
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

### Observe event of simulating fishing effort for catch prediction
### Observe "Simulate Button" clicked
catch_effort_data <- shiny::eventReactive(input$btn_simulate_effort_ID, {
  
  shinyalert(title = "Préparation de données...",
             text = "SIMULATION DE L'EFFORT DE PÊCHE",
             type = "warning",
             closeOnEsc = F,
             closeOnClickOutside = F,
             showCancelButton = F,
             showConfirmButton = F,
             immediate = TRUE,
             animation = TRUE)
  
  shiny::req(data_cpue_for_catch)
  req(input$direction_ID, input$slider_wgt_env_ID, input$slider_wgt_dist_ID, input$slider_wgt_hist_ID)
  
  env_weight <- as.numeric(input$slider_wgt_env_ID)
  dist_weight <- as.numeric(input$slider_wgt_dist_ID)
  hist_weight <- as.numeric(input$slider_wgt_hist_ID)
  
  fishing_direction <- input$direction_ID
  sim_year <- as.numeric(input$catch_simulation_year_ID)
  sim_months <- as.numeric(input$catch_simulation_months_ID)
  sim_ref_year <- input$catch_year_to_simulate_from_ID
  espece_cible <- input$rb_main_species_ID
  data_list1 <- data_cpue_for_catch()
  ### Predicting CPUEs to be user for env_prob calculation
  cpues <- predict_pelagics(data_list1)
  
  if("Sardina_pilchardus" == input$rb_main_species_ID){
    cpue <- cpues$sardine
  }else if("Scomber_colias" == input$rb_main_species_ID){
    cpue <- cpues$maquereau
  }else{
    cpue <- cpues$chinchard
  }

  data_list2 <- list("env_weight" = env_weight,
                    "dist_weight" = dist_weight,
                    "hist_weight" = hist_weight,
                    "fishing_direction" = fishing_direction,
                    "sim_year" = sim_year,
                    "sim_months" = sim_months,
                    "sim_ref_year" = sim_ref_year,
                    "espece_cible" = espece_cible,
                    "cpue" = cpue)
  
  effort_data <- func_simulate_effort_pelagics(data_list2)
  
  shinyalert(title = "Jointure de données océanique...",
             text = "SIMULATION DE L'EFFORT DE PÊCHE",
             type = "warning",
             closeOnEsc = F,
             closeOnClickOutside = F,
             showCancelButton = F,
             showConfirmButton = F,
             immediate = TRUE,
             animation = TRUE)
  
  effort_data_oceano <- add_oceano_to_coordinates(coordinates = effort_data)
  
  shinyalert(title = "Terminé!",
             text = "SIMULATION DE L'EFFORT DE PÊCHE",
             type = "success",
             closeOnEsc = TRUE,
             closeOnClickOutside = TRUE,
             showCancelButton = F,
             showConfirmButton = TRUE,
             immediate = TRUE,
             animation = TRUE)
  weeks <- unique(effort_data_oceano$DATE) %>% sort()
  Nweeks <- length(weeks)
  
  data_list3 <- list("data" = effort_data_oceano, 
                     "weeks" = weeks, 
                     "Nweeks" = Nweeks,
                     "sim_months" = sim_months,
                     "sim_year" = sim_year,
                     "sim_ref_year" = sim_ref_year,
                     "type" = "catch")
  return(data_list3)
})

### Predicting catch data
### Observe "Prediction button" clicked
catch_predictions <- shiny::eventReactive(input$btn_run_fishing_prediction_ID,{
  
  shiny::req(catch_effort_data())
  data_list <- catch_effort_data()
  
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
  
  data_list[["sardine_last_month_catch"]] = sardine_last_month_catch
  data_list[["sardine_selected"]] = sardine_selected
  data_list[["maquereau_last_month_catch"]] = maquereau_last_month_catch
  data_list[["maquereau_selected"]] = maquereau_selected
  data_list[["chinchard_last_month_catch"]] = chinchard_last_month_catch
  data_list[["chinchard_selected"]] = chinchard_selected
  data_list[["type"]] = "catch"
  
  shinyalert(title = "Prédiction des captures",
             text = "En cours...",
             type = "warning",
             closeOnEsc = F,
             closeOnClickOutside = F,
             showCancelButton = F,
             showConfirmButton = F,
             immediate = TRUE,
             animation = TRUE)
  
  catch_predictions <- predict_pelagics(data_list)
  
  shinyalert(title = "Fin de prédiction",
             text = "Terminé!",
             type = "success",
             closeOnEsc = TRUE,
             closeOnClickOutside = TRUE,
             showCancelButton = F,
             showConfirmButton = TRUE,
             immediate = TRUE,
             animation = TRUE)
  
  data_list1 <- list("catch_predictions" = catch_predictions,
                     "sardine_selected" = sardine_selected,
                     "maquereau_selected" = maquereau_selected,
                     "chinchard_selected" = chinchard_selected)
  
  return(data_list1)
  
})
