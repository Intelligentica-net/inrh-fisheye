##############################
# server.R
#
# Backend of the whole app
# Call all the server files for the other components
##############################

server <- function(input, output, session){
  useAutoColor() # initialise bs4Dash autocolor module
  output$user_out <- renderPrint({session$userData$user() })
  observeEvent(input$sign_out, {
    sign_out_from_shiny()
    session$reload()
  })
  
  source("./server/server_sidebar.R",local = TRUE)
  source("./server/server_controlbar.R",local = TRUE)
  source("./server/server_tab_main.R",local = TRUE)
  source("./server/server_tab_pelagics.R",local = TRUE)
  source("./server/server_tab_cephalopods.R",local = TRUE)
  source("./server/server_tab_interaction.R",local = TRUE)
  source("./server/server_tab_oceano.R",local = TRUE)
  source("./server/server_tab_documentation.R",local = TRUE)
  source("./server/server_tab_support.R",local = TRUE)
}

polished::secure_server(server)