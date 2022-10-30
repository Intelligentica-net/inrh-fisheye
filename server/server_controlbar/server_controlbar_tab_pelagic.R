##############################
# server_controlbar_pelagic.R
#
#
#
##############################

shiny::observeEvent(input$sidebarMenu,{
  
  if(input$sidebarMenu == "pelagic_sud_tab"){

    # pelagic tab cards
    shinyjs::show(id = "pelagic_type_prediction_card")
    shinyjs::hide(id = "pelagic_new_or_old_simulation_card")
    shinyjs::hide(id = "pelagic_fishing_effort_scenario_card")
    shinyjs::hide(id = "pelagic_fishing_effort_simulation_catch_card")
    shinyjs::show(id = "pelagic_fishing_effort_simulation_cpues_card")
    shinyjs::show(id = "pelagic_fishing_catch_simulation_card")

    # cephalopod tab cards
    shinyjs::hide(id = "cephalopod_species_card")
    shinyjs::hide(id = "cephalopod_fishing_season_card")

    # interaction tab_cards
    shinyjs::hide(id = "interaction_negro_attaque_card")

    # oceano tab cards
    # shinyjs::hide(id = "oceano_type_indicators_card")
    # shinyjs::hide(id = "oceano_list_indicators_card")
    # shinyjs::hide(id = "oceano_depth_indicator_card")
    # shinyjs::hide(id = "oceano_selection_dates_card")
    # shinyjs::hide(id = "oceano_selection_latitude_card")
  }

})

shiny::observeEvent(input$prediction_type_ID,{

  if(input$sidebarMenu == "pelagic_sud_tab"){
    if(input$prediction_type_ID == "cpue"){

      # pelagic tab cards
      shinyjs::show(id = "pelagic_type_prediction_card")
      shinyjs::hide(id = "pelagic_new_or_old_simulation_card")
      shinyjs::hide(id = "pelagic_fishing_effort_scenario_card")
      shinyjs::hide(id = "pelagic_fishing_effort_simulation_catch_card")
      shinyjs::show(id = "pelagic_fishing_effort_simulation_cpues_card")
      shinyjs::show(id = "pelagic_fishing_catch_simulation_card")

      # cephalopod tab cards
      shinyjs::hide(id = "cephalopod_species_card")
      shinyjs::hide(id = "cephalopod_fishing_season_card")

      # interaction tab_cards
      shinyjs::hide(id = "interaction_negro_attaque_card")

      # oceano tab cards
      # shinyjs::hide(id = "oceano_type_indicators_card")
      # shinyjs::hide(id = "oceano_list_indicators_card")
      # shinyjs::hide(id = "oceano_depth_indicator_card")
      # shinyjs::hide(id = "oceano_selection_dates_card")
      # shinyjs::hide(id = "oceano_selection_latitude_card")

    }else{

      # pelagic tab cards
      shinyjs::show(id = "pelagic_type_prediction_card")
      shinyjs::show(id = "pelagic_new_or_old_simulation_card")
      shinyjs::show(id = "pelagic_fishing_effort_scenario_card")
      shinyjs::show(id = "pelagic_fishing_effort_simulation_catch_card")
      shinyjs::hide(id = "pelagic_fishing_effort_simulation_cpues_card")
      shinyjs::show(id = "pelagic_fishing_catch_simulation_card")

      # cephalopod tab cards
      shinyjs::hide(id = "cephalopod_species_card")
      shinyjs::hide(id = "cephalopod_fishing_season_card")

      # interaction tab_cards
      shinyjs::hide(id = "interaction_negro_attaque_card")

      # oceano tab cards
      # shinyjs::hide(id = "oceano_type_indicators_card")
      # shinyjs::hide(id = "oceano_list_indicators_card")
      # shinyjs::hide(id = "oceano_depth_indicator_card")
      # shinyjs::hide(id = "oceano_selection_dates_card")
      # shinyjs::hide(id = "oceano_selection_latitude_card")
      
    }
  }

})

shiny::observeEvent(input$new_old_sim_ID, {

  if(input$new_old_sim_ID == "new_sim"){

    shinyjs::hide(id = "select_saved_simulation_ID")

  }else{

    shinyjs::show(id = "select_saved_simulation_ID")
    conn = DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
    list_saved_simulations <- DBI::dbGetQuery(conn, "select effort_tbl_names from effort_simulations_table")
    DBI::dbDisconnect(conn = conn)
    updateSelectInput(session = session,inputId = "select_saved_simulation_ID",
                      choices = list_saved_simulations,selected = NULL)
  }
})

### Update numericInput according to species selected

shiny::observeEvent(input$species_ID, {
  if("Sardina_pilchardus" %in% input$species_ID){
    shinyjs::show(id = "sardine_last_month_catch_ID")
  }else{
    shinyjs::hide(id = "sardine_last_month_catch_ID")
  }

  if("Scomber_colias" %in% input$species_ID){
    shinyjs::show(id = "maquereau_last_month_catch_ID")
  }else{
    shinyjs::hide(id = "maquereau_last_month_catch_ID")
  }

  if("Trachurus_trachurus" %in% input$species_ID){
    shinyjs::show(id = "chinchard_last_month_catch_ID")
  }else{
    shinyjs::hide(id = "chinchard_last_month_catch_ID")
  }

})

shiny::observe({
  if(is.null(input$species_ID)){
    
    shinyjs::hide(id = "sardine_last_month_catch_ID")
    shinyjs::hide(id = "maquereau_last_month_catch_ID")
    shinyjs::hide(id = "chinchard_last_month_catch_ID")

    shinyjs::disable(id = "btn_run_fishing_prediction_ID")
  }else{
    shinyjs::enable(id = "btn_run_fishing_prediction_ID")
  }
})

shiny::observeEvent(input$prediction_type_ID, {
  
  if(input$prediction_type_ID == "cpue"){
    
    shinyjs::hide(id = "rb_main_species_ID")
    shinyjs::hide(id = "carte_distr_effort_card")
    
  }else{
    
    shinyjs::show(id = "rb_main_species_ID")
    shinyjs::show(id = "carte_distr_effort_card")
  }
})

shiny::observeEvent(input$slider_wgt_env_ID, {
  
  updateSliderInput(session = session,inputId = "slider_wgt_dist_ID", 
                    value = 100 - input$slider_wgt_env_ID, 
                    max = 100 - input$slider_wgt_env_ID)
})

shiny::observeEvent(input$slider_wgt_dist_ID, {
  
  updateSliderInput(session = session,inputId = "slider_wgt_hist_ID", 
                    value = 100 - input$slider_wgt_env_ID - input$slider_wgt_dist_ID, 
                    max = 100 - input$slider_wgt_env_ID - input$slider_wgt_dist_ID)
  
})
