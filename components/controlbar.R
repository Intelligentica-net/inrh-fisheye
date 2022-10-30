##############################
# controlbar.R
#
# 
# 
##############################

controlbar <- bs4DashControlbar(
  id = "controlbar",
  skin = "light",
  title = NULL,
  width = "410px",
  source("./ui/ui_controlbar/ui_tab_main_controlbar.R",local = TRUE)$value,
  source("./ui/ui_controlbar/ui_tab_pelagics_controlbar.R",local = TRUE)$value,
  source("./ui/ui_controlbar/ui_tab_cephalopods_controlbar.R",local = TRUE)$value,
  source("./ui/ui_controlbar/ui_tab_interaction_controlbar.R",local = TRUE)$value,
  source("./ui/ui_controlbar/ui_tab_oceano_controlbar.R",local = TRUE)$value
)