##############################
# ui_tab_oceano.R
#
# 
# 
##############################

bs4TabItem(
  tabName = "oceano_tab",
  fluidRow(
    bs4Card(
      title = "FEATURES",
      elevation = 4,
      closable = FALSE,
      width = 2,
      solidHeader = TRUE,
      status = "white",
      collapsible = TRUE,
      h6("INDICES"),
      bsButton(inputId = "sst_btn_ID",label = "SST",block = TRUE,type = "action",style = "primary"),
      bsButton(inputId = "ekman_btn_ID",label = "EKMAN",block = TRUE,type = "action",style = "primary"),
      bsButton(inputId = "nao_btn_ID",label = "NAO",block = TRUE,type = "action",style = "primary"),
      h6("VARIABLES BIOLOGIQUES"),
      bsButton(inputId = "o2_btn_ID",label = "O2",block = TRUE,type = "action",style = "primary"),
      bsButton(inputId = "chl_btn_ID",label = "CHL",block = TRUE,type = "action",style = "primary"),
      bsButton(inputId = "pp_btn_ID",label = "PP",block = TRUE,type = "action",style = "primary"),
      h6("VARIABLES PHYSIQUES"),
      bsButton(inputId = "temp_btn_ID",label = "TEMPERATURE",block = TRUE,type = "action",style = "primary"),
      bsButton(inputId = "sal_btn_ID",label = "SALINITE",block = TRUE,type = "action",style = "primary"),
      bsButton(inputId = "u_phys_btn_ID",label = "U",block = TRUE,type = "action",style = "primary"),
      bsButton(inputId = "v_phys_btn_ID",label = "V",block = TRUE,type = "action",style = "primary"),
      h6("VARIABLES METEOROLOGIQUES"),
      bsButton(inputId = "pres_btn_ID",label = "PRES",block = TRUE,type = "action",style = "primary"),
      bsButton(inputId = "u_meteo_btn_ID",label = "U",block = TRUE,type = "action",style = "primary"),
      bsButton(inputId = "v_meteo_btn_ID",label = "V",block = TRUE,type = "action",style = "primary"),
      bsButton(inputId = "per_btn_ID",label = "PER",block = TRUE,type = "action",style = "primary"),
      bsButton(inputId = "dir_btn_ID",label = "DIR",block = TRUE,type = "action",style = "primary"),
      bsButton(inputId = "hs_btn_ID",label = "HS",block = TRUE,type = "action",style = "primary")
    ),
    bs4Card(
      title = textOutput(outputId = "title_variable_box_ID"),
      elevation = 4,
      closable = FALSE,
      width = 10,
      solidHeader = TRUE,
      status = "white",
      collapsible = TRUE,
      fluidRow(
        box(
          solidHeader = F,
          width = 3,
          collapsible = F,
          collapsed = F,
          tableOutput(outputId = "oceano_var_smry")
        ),
        box(
          solidHeader = F,
          collapsible = F,
          collapsed = F,
          width = 9,
          plotOutput(outputId = "oceano_var_plot")
        )
      ),
      fluidRow(
        box(
          solidHeader = F,
          width = 6,
          collapsible = F,
          collapsed = F,
          tableOutput(outputId = "oceano_var_quantiles")
        ),
        box(
          solidHeader = F,
          collapsible = F,
          collapsed = F,
          width = 6,
          tableOutput(outputId = "oceano_var_stats")
        )
      )
    )
  )
)