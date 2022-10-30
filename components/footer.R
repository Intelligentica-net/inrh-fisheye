##############################
# footer.R
#
# 
# 
##############################

left_footer <- fluidRow(
  column(
    width = 3,
    align = "center",
    "",
    a(
      href = "#",
      target = "_blank",
      img(src = "logo/fisheye_logo.png", height = "30px")
    )
  ),
  column(
    width = 3,
    align = "center",
    "",
    a(
      href = "#",
      target = "_blank",
      img(src = "logo/intelligentica.svg", height = "30px")
    )
  ),
  column(
    width = 3,
    align = "center",
    a(
      href = "https://www.inrh.ma",
      target = "_blank",
      img(src = "logo/inrh_logo.png", height = "30px")
    )
  ),
  column(
    width = 3,
    align = "center",
    "Version 1.0.5 - last update: 22/10/2021"
  )
)

footer <- bs4DashFooter(
  left_footer
)