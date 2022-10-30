##############################
# server_controlbar_cephalopod.R
#
# 
# 
##############################

shiny::observeEvent(input$sidebarMenu,{
  
  if(input$sidebarMenu == "cephalopod_tab"){
    
    # pelagic tab cards
    shinyjs::hide(id = "pelagic_type_prediction_card")
    shinyjs::hide(id = "pelagic_new_or_old_simulation_card")
    shinyjs::hide(id = "pelagic_fishing_effort_scenario_card")
    shinyjs::hide(id = "pelagic_fishing_effort_simulation_catch_card")
    shinyjs::hide(id = "pelagic_fishing_effort_simulation_cpues_card")
    shinyjs::hide(id = "pelagic_fishing_catch_simulation_card")
    
    # cephalopod tab cards
    shinyjs::show(id = "cephalopod_species_card")
    shinyjs::show(id = "cephalopod_fishing_season_card")
    
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

### Update numericInput according to species selected

shiny::observeEvent(input$check_species_ID, {
  if("poulpe" %in% input$check_species_ID){
    shinyjs::show(id = "poulpe_last_week_cpue_ID")
  }else{
    shinyjs::hide(id = "poulpe_last_week_cpue_ID")
  }
  
  if("calmar" %in% input$check_species_ID){
    shinyjs::show(id = "calmar_last_week_cpue_ID")
  }else{
    shinyjs::hide(id = "calmar_last_week_cpue_ID")
  }
  
  if("seiche" %in% input$check_species_ID){
    shinyjs::show(id = "seiche_last_week_cpue_ID")
  }else{
    shinyjs::hide(id = "seiche_last_week_cpue_ID")
  }
  
})

shiny::observe({
  if(is.null(input$check_species_ID)){
    
    shinyjs::hide(id = "poulpe_last_week_cpue_ID")
    shinyjs::hide(id = "calmar_last_week_cpue_ID")
    shinyjs::hide(id = "seiche_last_week_cpue_ID")
    
    shinyjs::disable(id = "btn_simulate_cpue_cephalopods")
  }else{
    shinyjs::enable(id = "btn_simulate_cpue_cephalopods")
  }
})

