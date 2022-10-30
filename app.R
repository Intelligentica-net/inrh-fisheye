##############################
# app.R
#
# Main controller.
# Used to import the ui and server components; initializes the app.
##############################

source("./ui.R")
source("./server.R")

# Run the application 
shinyApp(ui = ui, server = server)
