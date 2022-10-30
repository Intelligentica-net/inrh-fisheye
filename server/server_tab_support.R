##############################
# server_tab_support.R
#
#
#
##############################
getSupport<-function() {
  return(includeHTML("www/support.html"))
}
output$support<-renderUI({getSupport()})