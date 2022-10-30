##############################
# server_controlbar_tab_oceano.R
#
# 
# 
##############################

shiny::observeEvent(input$sidebarMenu,{
  
  if(input$sidebarMenu == "oceano_tab"){
    
    # pelagic tab cards
    shinyjs::hide(id = "pelagic_type_prediction_card")
    shinyjs::hide(id = "pelagic_new_or_old_simulation_card")
    shinyjs::hide(id = "pelagic_fishing_effort_scenario_card")
    shinyjs::hide(id = "pelagic_fishing_effort_simulation_catch_card")
    shinyjs::hide(id = "pelagic_fishing_effort_simulation_cpues_card")
    shinyjs::hide(id = "pelagic_fishing_catch_simulation_card")
    
    # cephalopod tab cards
    shinyjs::hide(id = "cephalopod_species_card")
    shinyjs::hide(id = "cephalopod_fishing_season_card")
    
    # interaction tab_cards
    shinyjs::hide(id = "interaction_negro_attaque_card")
    
    # oceano tab cards
    # shinyjs::show(id = "oceano_type_indicators_card")
    # shinyjs::show(id = "oceano_list_indicators_card")
    # shinyjs::show(id = "oceano_depth_indicator_card")
    # shinyjs::show(id = "oceano_selection_dates_card")
    # shinyjs::show(id = "oceano_selection_latitude_card")
  }
  
})