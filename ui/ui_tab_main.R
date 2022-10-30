##############################
# ui_tab_main.R
#
# 
# 
##############################

bs4TabItem(
  tabName = "home_tab",
  htmlOutput("index"),
  tags$head(
    tags$link(rel = "icon",
              type = "image/png",
              href = "logo/FishEyeLogo.png")
  )
)