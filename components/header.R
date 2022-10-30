##############################
# header.R
#
# 
# 
##############################

header = bs4DashNavbar(
  title = dashboardBrand(
    title = "FishEye",
    href = "https://www.inrh.ma/",
    image = "logo/FishEyeLogo.png"
  ),
  rightUi = tags$li(
    class = "dropdown",
    actionButton(inputId = "sign_out",
                 label = "",
                 icon = icon("log-out",lib = "glyphicon")
    )
  ),
  skin = "light",
  status = "white",
  sidebarIcon = icon("bars"),
  controlbarIcon = icon("th",lib = "glyphicon"),
  fixed = TRUE
)
