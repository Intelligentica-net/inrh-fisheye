##############################
# ui_tab_oceano_controlbar.R
#
# 
# 
##############################

# fluidPage(
#   
#   bs4Card(title = "Types d'indicateurs",
#           id = "oceano_type_indicators_card",
#           status = "navy", 
#           solidHeader = TRUE, 
#           collapsible = TRUE, 
#           collapsed = FALSE, 
#           width = 12,
#           icon = icon("cogs"), 
#           maximizable = FALSE, 
#           div(class = "left-text",
#               radioButtons(inputId = "dash_type_idx",
#                            label = NULL,
#                            choices = c("Biologiques" = "bio",
#                                        "Physiques" = "phys",
#                                        "Meteo" = "meteo",
#                                        "Autres" = "indices"),
#                            inline = TRUE
#               )
#           )
#   ),
#   
#   bs4Card(title = NULL,
#           id = "oceano_list_indicators_card",
#           status = "navy",
#           solidHeader = TRUE,
#           collapsible = TRUE,
#           collapsed = FALSE,
#           width = 12,
#           icon = icon("cogs"),
#           maximizable = FALSE,
#           div(class = "left-text",
#               radioButtons(inputId = "dash_vars_idx",
#                          label = NULL,
#                          choiceNames = "",
#                          choiceValues = "",
#                          inline = TRUE
#             )
#           )
#   ),
# 
#   bs4Card(title = "Profondeur de collecte de données",
#           id = "oceano_depth_indicator_card",
#           status = "navy",
#           solidHeader = TRUE,
#           collapsible = TRUE,
#           collapsed = FALSE,
#           width = 12,
#           icon = icon("cogs"),
#           maximizable = FALSE,
#           div(class = "left-text",
#               shinyWidgets::pickerInput(
#                 inputId = "profond_value",
#                 label = NULL,
#                 choices = NULL,
#                 options = list(`actions-box` = TRUE),
#                 multiple = FALSE,
#                 selected = NULL
#                 )
#               )
#           ),
# 
#   bs4Card(title = "Selection de dates",
#           id = "oceano_selection_dates_card",
#           status = "navy",
#           solidHeader = TRUE,
#           collapsible = TRUE,
#           collapsed = FALSE,
#           width = 12,
#           icon = icon("cogs"),
#           maximizable = FALSE,
#           div(class = "left-text",
#               selectInput(inputId = "dateRange",label = NULL,choices = NULL,selected = NULL)
#               )
#           ),
# 
#   bs4Card(title = "Selection de latitudes",
#           id = "oceano_selection_latitude_card",
#           status = "navy",
#           solidHeader = TRUE,
#           collapsible = TRUE,
#           collapsed = FALSE,
#           width = 12,
#           icon = icon("cogs"),
#           maximizable = FALSE,
#           div(class = "left-text",
#               numericInput(inputId = "latitudeRangeStart",value = 20,min = 19.5,max = 36.5,label = "Du"),
#               br(),
#               numericInput(inputId = "latitudeRangeEnd",value = 36.5,min = 19.5,max = 36.5,label = "Au")
#           )
#   )
# )