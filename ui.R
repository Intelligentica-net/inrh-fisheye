##############################
# ui.R
#
# Initializes the ui.
# Used to load in your header, sidebar, and body components.
##############################

source("./components/header.R")
source("./components/sidebar.R")
source("./components/controlbar.R")
source("./components/footer.R")
source("./components/body.R")
source("./components/theme.R")

# Let thematic know to update the fonts, too
thematic_shiny(font = "auto")

ui <- bs4DashPage(
  header = header,
  sidebar = sidebar,
  body = body,
  controlbar = controlbar,
  footer = footer,
  title = "FishEye",
  skin = "light",
  options = NULL,
  dark = FALSE,
  scrollToTop = TRUE
)

sign_in_page_ui = sign_in_ui_default(
  color = "#006CB5",
  company_name = "FISHEYE",
  logo_top = tags$img(
    src = "logo/fisheye_white_logo.png",
    alt = "FISHEYE Logo",
    style = "width: 125px; margin-top: 30px; margin-bottom: 30px;"
  ),
  logo_bottom = tags$img(
    src = "logo/logo.png",
    alt = "INRH Logo",
    style = "width: 300px; margin-bottom: 15px;"
  ),
  icon_href = "logo/fisheye_icon.png",
  background_image = "images/ocean_waves.jpg"
)

# secure your UI behind your custom sign in page
polished::secure_ui(
  ui,
  sign_in_page_ui = sign_in_page_ui
)