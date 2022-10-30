##############################
# server_tab_documentation.R
#
#
#
##############################
getDocumentation<-function() {
  return(includeHTML("www/documentation.html"))
}
output$documentation<-renderUI({getDocumentation()})