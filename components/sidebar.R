##############################
# sidebar.R
#
# 
# 
##############################

sidebar = bs4DashSidebar(
  width = "320px",
  skin = "light",
  status = "info",
  elevation = 2,
  opacity = 0.8,
  collapsed = TRUE,
  minified = TRUE,
  expandOnHover = TRUE,
  
  bs4SidebarMenu(id = "sidebarMenu",
    bs4SidebarMenuItem(
      text = "ACCUEIL",
      tabName = "home_tab",
      icon = icon("home")
    ),
    bs4SidebarMenuItem(
      text = "PÊCHERIE PÉLAGIQUE SUD",
      tabName = "pelagic_sud_tab",
      icon = icon("fish",lib = "font-awesome")
    ),
    bs4SidebarMenuItem(
      text = "PÊCHERIE CÉPHALOPODIÈRE",
      tabName = "cephalopod_tab",
      icon = icon("ship",lib = "font-awesome")
    ),
    bs4SidebarMenuItem(
      text = "INTERACTIONS",
      tabName = "interaction_tab",
      icon = icon("random", lib = "glyphicon")
    ),
    bs4SidebarMenuItem(
      text = "OCÉANOGRAPHIE",
      tabName = "oceano_tab",
      icon = icon("water")
    ),
    bs4SidebarMenuItem(
      text = "DOCUMENTATION",
      tabName = "documentation_tab",
      icon = icon("book",lib = "font-awesome")
    ),
    bs4SidebarMenuItem(
      text = "AIDE",
      tabName = "support_tab",
      icon = icon("question-circle",lib = "font-awesome")
    )
  )
)