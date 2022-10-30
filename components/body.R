##############################
# body.R
#
# 
# 
##############################

body = bs4DashBody(
  shinyjs::useShinyjs(),
  bs4TabItems(
    source("./ui/ui_tab_main.R",local = TRUE)$value,
    source("./ui/ui_tab_pelagics.R",local = TRUE)$value,
    source("./ui/ui_tab_cephalopods.R",local = TRUE)$value,
    source("./ui/ui_tab_interaction.R",local = TRUE)$value,
    source("./ui/ui_tab_oceano.R",local = TRUE)$value,
    source("./ui/ui_tab_documentation.R",local = TRUE)$value,
    source("./ui/ui_tab_support.R",local = TRUE)$value 
  )
)
