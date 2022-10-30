##############################
# ui_tab_interaction_controlbar.R
#
# 
# 
##############################

fluidRow(
  bs4Card(title = "PROBABILITÉ D'ATTAQUE", 
          id = "interaction_negro_attaque_card",
          status = "navy", 
          solidHeader = TRUE, 
          collapsible = TRUE, 
          collapsed = FALSE, 
          width = 12,
          icon = icon("cogs"), 
          maximizable = FALSE, 
          div(class = "left-text",
              dateRangeInput(inputId = "periode_attaque",
                             format = "yyyy-mm-dd",
                             separator = "-",
                             label = "Période de prédiction", 
                             start = Sys.Date(), 
                             end = Sys.Date() + 30),
              useShinyalert(),  
              actionButton(inputId = "btn_predict_attaque_negro", 
                           label = "Prédiction", color = "primary",
                           status = "primary",
                           flat =TRUE ,
                           width = "100%",
                           icon = icon("random",lib = "font-awesome"))
          )
  )
)