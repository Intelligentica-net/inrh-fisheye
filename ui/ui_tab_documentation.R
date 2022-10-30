##############################
# ui_tab_documentation.R
#
# 
# 
##############################

bs4TabItem(
  tabName = "documentation_tab",
  htmlOutput("documentation"),
  tags$head(
    tags$link(rel = "icon",
              type = "image/png",
              href = "logo/FishEyeLogo.png")
  )
)
