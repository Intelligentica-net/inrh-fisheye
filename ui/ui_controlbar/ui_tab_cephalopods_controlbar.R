##############################
# ui_tab_cephalopods_controlbar.R
#
# 
# 
##############################

fluidRow(
  bs4Card(title = "ESPÈCES", 
          id = "cephalopod_species_card",
          status = "navy", 
          solidHeader = TRUE, 
          collapsible = TRUE, 
          collapsed = FALSE, 
          width = 12,
          icon = icon("cogs"), 
          maximizable = FALSE, 
          div(class = "left-text",
              checkboxGroupInput(inputId = "check_species_ID",selected = c("poulpe","calmar","seiche"), label = NULL,inline = TRUE,
                                 choiceValues = c("poulpe","calmar","seiche"),
                                 choiceNames = c("Poulpe","Calmar","Seiche"),width = "100%"
              )
          )
  ),
  
  bs4Card(title = "SAISON DE PÊCHE",
          id = "cephalopod_fishing_season_card",
          status = "navy", 
          solidHeader = TRUE, 
          collapsible = TRUE, 
          collapsed = FALSE, 
          width = 12,
          icon = icon("cogs"), 
          maximizable = FALSE, 
          div(class = "left-text",
              numericInput(inputId = "nombre_semaine_peche_ID",
                           label = "Nombre de semaines", 
                           value = 1,min = 1,max = 30,step = 1),
              hr(),
              dateRangeInput(inputId = "debut_fin_saison_ID",
                             format = "yyyy-mm-dd",
                             separator = "-",
                             label = "Saison de pêche", 
                             start = Sys.Date(), 
                             end = Sys.Date() + 30),
              hr(),
              radioButtons(inputId = "campagne_or_profession_ID", 
                           label = "Source du CPUE", 
                           choices = c("Campagne Scientifique"="campagne","Flotte Hauturière"="flotte"), 
                           selected = "campagne",inline = TRUE,width = "100%"),
              hr(),
              numericInput(inputId = "poulpe_last_week_cpue_ID", 
                           label = "CPUE (t-1) en Poulpe",value = 0, 
                           min = 0,max = 1000000,step = 1),
              numericInput(inputId = "calmar_last_week_cpue_ID", 
                           label = "CPUE (t-1) en Calmar",value = 0, 
                           min = 0,max = 1000000,step = 1),
              numericInput(inputId = "seiche_last_week_cpue_ID", 
                           label = "CPUE (t-1) en Seiche",value = 0, 
                           min = 0,max = 1000000,step = 1),
          ),
          hr(),
          useShinyalert(),
          actionButton(inputId = "btn_simulate_cpue_cephalopods", 
                       label = "Simulation", color = "primary",
                       status = "primary",
                       flat =TRUE ,
                       width = "100%",
                       icon = icon("random",lib = "font-awesome"))
  )
)