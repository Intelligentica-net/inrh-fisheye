##############################
# ui_tab_pelagic.R
#
# 
# 
##############################

bs4TabItem(
  tabName = "pelagic_sud_tab",
  fluidRow(
    valueBoxOutput(outputId = "tot_catch_sardine",width = 3),
    valueBoxOutput(outputId = "tot_catch_maquerel",width = 3),
    valueBoxOutput(outputId = "tot_catch_chinchard",width = 3),
    valueBoxOutput(outputId = "tot_chiffre_affaire",width = 3)
  ),
  fluidRow(
    column(width = 6,
           bs4Card(title = "CAPTURE PAR ESPECE ET PAR SEMAINE", 
                   id = "graph_capture_par_espece_semaine_card",
                   status = "navy", 
                   solidHeader = TRUE, 
                   collapsible = TRUE, 
                   collapsed = FALSE, 
                   width = 12,
                   icon = icon("chart-area",lib = "font-awesome"), 
                   maximizable = FALSE, 
                   # dropdownMenu = cardDropdown(
                   #   actionBttn(inputId = "saveDistribPecheSardine",
                   #              color = "primary",
                   #              label = "Enregistrer", 
                   #              icon = icon("save"), 
                   #              style = "minimal", 
                   #              size = "sm", 
                   #              block = TRUE),
                   #   icon = icon("wrench",lib = "font-awesome")),
                   plotOutput("cpue_catch_moyenne_plot")
                   
           )   
    ),
    column(width = 6,
           bs4Card(title = "CAPTURE MENSUELLE PAR ESPÈCE", 
                   id = "graph_capture_mensuelle_card",
                   status = "navy", 
                   solidHeader = TRUE, 
                   collapsible = TRUE, 
                   collapsed = FALSE, 
                   width = 12,
                   icon = icon("chart-area",lib = "font-awesome"), 
                   maximizable = FALSE, 
                   # dropdownMenu = cardDropdown(
                   #   actionBttn(inputId = "saveDistribPecheSardine",
                   #              color = "primary",
                   #              label = "Enregistrer", 
                   #              icon = icon("save"), 
                   #              style = "minimal", 
                   #              size = "sm", 
                   #              block = TRUE),
                   #   icon = icon("wrench",lib = "font-awesome")),
                   plotOutput("catch_totale_mensuelle_plot")
                   
           )
    )
  ),
  fluidRow(
    bs4Card(title = "CARTE DE DISTRIBTION DE L'EFFORT DE PÊCHE SIMULÉ", 
            id = "carte_distr_effort_card",
            status = "navy", 
            solidHeader = TRUE, 
            collapsible = TRUE, 
            collapsed = FALSE, 
            width = 12,
            icon = icon("chart-area",lib = "font-awesome"), 
            maximizable = FALSE, 
            # dropdownMenu = cardDropdown(
            #   actionBttn(inputId = "saveDistribEffort",
            #              color = "primary",
            #              label = "Enregistrer", 
            #              icon = icon("save"), 
            #              style = "minimal", 
            #              size = "sm", 
            #              block = TRUE),
            #   icon = icon("wrench",lib = "font-awesome")),
            uiOutput("effort_peche_plots_ui")
    ),
    bs4Card(title = "CARTE DE DISTRIBTION DE LA PÊCHE EN SARDINE", 
            id = "carte_distr_peche_card_sardine",
            status = "navy", 
            solidHeader = TRUE, 
            collapsible = TRUE, 
            collapsed = FALSE, 
            width = 12,
            icon = icon("chart-area",lib = "font-awesome"), 
            maximizable = FALSE, 
            # dropdownMenu = cardDropdown(
            #   actionBttn(inputId = "saveDistribPecheSardine",
            #              color = "primary",
            #              label = "Enregistrer", 
            #              icon = icon("save"), 
            #              style = "minimal", 
            #              size = "sm", 
            #              block = TRUE),
            #   icon = icon("wrench",lib = "font-awesome")),
            uiOutput("catch_sardine_plots_ui"),
            uiOutput("cpue_sardine_plots_ui")
            
    ),
    bs4Card(title = "CARTE DE DISTRIBTION DE LA PÊCHE EN MAQUEREAU", 
            id = "carte_distr_peche_card_maquereau",
            status = "navy", 
            solidHeader = TRUE, 
            collapsible = TRUE, 
            collapsed = FALSE, 
            width = 12,
            icon = icon("chart-area",lib = "font-awesome"), 
            maximizable = FALSE, 
            # dropdownMenu = cardDropdown(
            #   actionBttn(inputId = "saveDistribPecheMaquereau",
            #              color = "primary",
            #              label = "Enregistrer", 
            #              icon = icon("save"), 
            #              style = "minimal", 
            #              size = "sm", 
            #              block = TRUE),
            #   icon = icon("wrench",lib = "font-awesome")),
            uiOutput("catch_maquereau_plots_ui"),
            uiOutput("cpue_maquereau_plots_ui")
            
    ),
    bs4Card(title = "CARTE DE DISTRIBTION DE LA PÊCHE EN CHINCHARD", 
            id = "carte_distr_peche_card_chinchard",
            status = "navy", 
            solidHeader = TRUE, 
            collapsible = TRUE, 
            collapsed = FALSE, 
            width = 12,
            icon = icon("chart-area",lib = "font-awesome"), 
            maximizable = FALSE, 
            # dropdownMenu = cardDropdown(
            #   actionBttn(inputId = "saveDistribPecheChinchard",
            #              color = "primary",
            #              label = "Enregistrer", 
            #              icon = icon("save"), 
            #              style = "minimal", 
            #              size = "sm", 
            #              block = TRUE),
            #   icon = icon("wrench",lib = "font-awesome")),
            uiOutput("catch_chinchard_plots_ui"),
            uiOutput("cpue_chinchard_plots_ui")
            
    )
  )
)
