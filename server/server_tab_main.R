##############################
# server_tab_main.R
#
# 
# 
##############################
getIndex<-function() {
  return(includeHTML("www/index.html"))
}
output$index<-renderUI({getIndex()})