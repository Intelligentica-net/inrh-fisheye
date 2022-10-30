##############################
# ui_tab_cephalopods.R
#
# 
# 
##############################

bs4TabItem(
  tabName = "cephalopod_tab",
  fluidRow(
    column(width = 2,bs4Dash::bs4ValueBoxOutput("cephalopod_tot_catch_poulpe")),
    column(width = 2,bs4Dash::bs4ValueBoxOutput("cephalopod_tot_catch_calmar")),
    column(width = 2,bs4Dash::bs4ValueBoxOutput("cephalopod_tot_catch_seiche")),
    column(width = 2,bs4Dash::bs4ValueBoxOutput("cephalopod_tot_chiffre_affaire")),
    column(width = 2,bs4Dash::bs4ValueBoxOutput("cephalopod_tot_resultat_net")),
    column(width = 2,bs4Dash::bs4ValueBoxOutput("cephalopod_tot_cout_peche")),
  ),
  fluidRow(
    bs4Card(title = "CARTE DE DISTRIBTION DES CPUEs DU POULPE", 
            id = "carte_distr_peche_card_poulpe",
            status = "navy", 
            solidHeader = TRUE, 
            collapsible = TRUE, 
            collapsed = FALSE, 
            width = 12,
            icon = icon("chart-area",lib = "font-awesome"), 
            maximizable = FALSE, 
            # dropdownMenu = cardDropdown(
            #   actionBttn(inputId = "saveDistribCPUEPoulpe",
            #              color = "primary",
            #              label = "Enregistrer", 
            #              icon = icon("save"), 
            #              style = "minimal", 
            #              size = "sm", 
            #              block = TRUE),
            #   icon = icon("wrench",lib = "font-awesome")),
            uiOutput("cpue_poulpe_plots_ui")
    ),
    bs4Card(title = "CARTE DE DISTRIBTION DES CPUEs DU CALMAR", 
            id = "carte_distr_peche_card_calmar",
            status = "navy", 
            solidHeader = TRUE, 
            collapsible = TRUE, 
            collapsed = FALSE, 
            width = 12,
            icon = icon("chart-area",lib = "font-awesome"), 
            maximizable = FALSE, 
            # dropdownMenu = cardDropdown(
            #   actionBttn(inputId = "saveDistribCPUECalmar",
            #              color = "primary",
            #              label = "Enregistrer", 
            #              icon = icon("save"), 
            #              style = "minimal", 
            #              size = "sm", 
            #              block = TRUE),
            #   icon = icon("wrench",lib = "font-awesome")),
            uiOutput("cpue_calmar_plots_ui")
    ),
    bs4Card(title = "CARTE DE DISTRIBTION DES CPUEs DE LA SEICHE", 
            id = "carte_distr_peche_card_seiche",
            status = "navy", 
            solidHeader = TRUE, 
            collapsible = TRUE, 
            collapsed = FALSE, 
            width = 12,
            icon = icon("chart-area",lib = "font-awesome"), 
            maximizable = FALSE, 
            # dropdownMenu = cardDropdown(
            #   actionBttn(inputId = "saveDistribCPUESeiche",
            #              color = "primary",
            #              label = "Enregistrer", 
            #              icon = icon("save"), 
            #              style = "minimal", 
            #              size = "sm", 
            #              block = TRUE),
            #   icon = icon("wrench",lib = "font-awesome")),
            uiOutput("cpue_seiche_plots_ui")
    )
  )
)