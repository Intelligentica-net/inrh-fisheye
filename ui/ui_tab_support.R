##############################
# ui_tab_support.R
#
# 
# 
##############################

bs4TabItem(
  tabName = "support_tab",
  htmlOutput("support"),
  tags$head(
    tags$link(rel = "icon",
              type = "image/png",
              href = "logo/FishEyeLogo.png")
  )
)
