##############################
# ui_tab_interaction.R
#
# 
# 
##############################

bs4TabItem(
  tabName = "interaction_tab",
  fluidRow(
    bs4Card(title = "CARTE DE PROBABILITÉ DES ATTAQUES DE NEGRO",
            id = "carte_distr_attaque_negro_card",
            status = "navy", 
            solidHeader = TRUE, 
            collapsible = TRUE, 
            collapsed = FALSE, 
            width = 12,
            icon = icon("chart-area",lib = "font-awesome"), 
            maximizable = FALSE, 
            # dropdownMenu = cardDropdown(
            #   downloadBttn(outputId = "saveProbAttaqueNegro", 
            #                label = "Enregistrer", 
            #                style = "minimal", 
            #                color = "primary", 
            #                size = "sm", 
            #                block = TRUE),
            #   icon = icon("wrench",lib = "font-awesome")),
            uiOutput("attaque_probability_plots_ui")
    )
  )
)