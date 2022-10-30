##############################
# server_tab_pelagics.R
#
#
#
##############################

shiny::observeEvent(input$btn_run_fishing_prediction_ID, {
  
  if(input$prediction_type_ID == "cpue"){
    req(cpue_predictions())
    req(data_cpue())
    
    pred_data <- cpue_predictions()
    other_data <- data_cpue()
    
    title_chunk <- "CPUE SIMULÉE - SEMAINE N°"
    fill_chunk <- "CPUE (Tonnes/Heure)"
    
    plot_sardine <- "cpue_sardine_plots_ui"
    plot_maquereau <- "cpue_maquereau_plots_ui"
    plot_chinchard <- "cpue_chinchard_plots_ui"
  }else{
    req(catch_predictions())
    req(catch_effort_data())
    
    pred_data <- catch_predictions()$catch_predictions
    other_data <- catch_effort_data()
    other_data[["sardine_selected"]] <- catch_predictions()$sardine_selected
    other_data[["maquereau_selected"]] <- catch_predictions()$maquereau_selected
    other_data[["chinchard_selected"]] <- catch_predictions()$chinchard_selected
    
    title_chunk <- "CAPTURE SIMULÉE - SEMAINE N°"
    fill_chunk <- "CAPTURES (Tonnes)"
    
    plot_sardine <- "catch_sardine_plots_ui"
    plot_maquereau <- "catch_maquereau_plots_ui"
    plot_chinchard <- "catch_chinchard_plots_ui"
  }
  
  if(other_data$sardine_selected){
    for (i in 1:other_data$Nweeks) {
      local({
        my_i <- i
        catch_week <- pred_data$sardine %>% dplyr::filter(date == other_data$weeks[my_i])
        if(other_data$type == "catch"){
          tot_catch <- round(sum(catch_week$pred_weight_t),digits = 2)
        }else{
          tot_catch <- NA
        }
        week <- other_data$weeks[my_i]
        plotname <- paste(plot_sardine, my_i, sep="")
        output[[plotname]] <- renderPlot({
          catch_week %>%
            ggplot2::ggplot() +
            ggplot2::theme_bw() +
            ggplot2::geom_tile(aes(lon, lat, fill = pred_weight_t)) +
            ggplot2::scale_fill_viridis_c(option = "B", direction = -1) +
            # only data within the data-bounds
            ggplot2::borders(database = "worldHires",xlim = range(catch_week$lon), ylim = range(catch_week$lat),fill = "antiquewhite") + 
            # limit plot
            ggplot2::coord_quickmap(xlim = range(catch_week$lon), ylim = range(catch_week$lat)) +
            ggplot2::ggtitle(label = paste(paste(title_chunk ,my_i," -- ",week),
                                    paste("CAPTURE TOTALE = ",tot_catch," (t)",sep = " "),sep = "\n"),
                      subtitle = "Espèce: Sardine") +
            ggplot2::xlab("Longitude") + ylab("Latitude") +
            ggplot2::labs(fill = fill_chunk) +
            ggplot2::theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
                    panel.background = element_rect(fill = "aliceblue"))
        },height = 700, width = 550) 
      })
    }
  }
  
  if(other_data$maquereau_selected){
    for (i in 1:other_data$Nweeks) {
      local({
        my_i <- i
        catch_week <- pred_data$maquereau %>% dplyr::filter(date == other_data$weeks[my_i])
        if(other_data$type == "catch"){
          tot_catch <- round(sum(catch_week$pred_weight_t),digits = 2)
        }else{
          tot_catch <- NA
        }
        week <- other_data$weeks[my_i]
        plotname <- paste(plot_maquereau, my_i, sep="")
        output[[plotname]] <- renderPlot({
          catch_week %>%
            ggplot2::ggplot() +
            ggplot2::theme_bw() +
            ggplot2::geom_tile(aes(lon, lat, fill = pred_weight_t)) +
            ggplot2::scale_fill_viridis_c(option = "B", direction = -1) +
            # only data within the data-bounds
            ggplot2::borders(database = "worldHires",xlim = range(catch_week$lon), ylim = range(catch_week$lat),fill = "antiquewhite") +
            # limit plot
            ggplot2::coord_quickmap(xlim = range(catch_week$lon), ylim = range(catch_week$lat)) + 
            ggplot2::ggtitle(label = paste(paste(title_chunk ,my_i," -- ",week),
                                    paste("CAPTURE TOTALE = ",tot_catch," (t)",sep = " "),sep = "\n"),
                      subtitle = "Espèce: Maquereau") +
            ggplot2::xlab("Longitude") + ylab("Latitude") +
            ggplot2::labs(fill = fill_chunk) +
            ggplot2::theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
                    panel.background = element_rect(fill = "aliceblue"))
        },height = 700, width = 550) 
      })
    }
  }
  
  if(other_data$chinchard_selected){
    for (i in 1:other_data$Nweeks) {
      local({
        my_i <- i
        catch_week <- pred_data$chinchard %>% dplyr::filter(date == other_data$weeks[my_i])
        if(other_data$type == "catch"){
          tot_catch <- round(sum(catch_week$pred_weight_t),digits = 2)
        }else{
          tot_catch <- NA
        }
        week <- other_data$weeks[my_i]
        plotname <- paste(plot_chinchard, my_i, sep="")
        output[[plotname]] <- renderPlot({
          catch_week %>%
            ggplot2::ggplot() +
            ggplot2::theme_bw() +
            ggplot2::geom_tile(aes(lon, lat, fill = pred_weight_t)) +
            ggplot2::scale_fill_viridis_c(option = "B", direction = -1) +
            # only data within the data-bounds
            ggplot2::borders(database = "worldHires",xlim = range(catch_week$lon), ylim = range(catch_week$lat),fill = "antiquewhite")+
            # limit plot
            ggplot2::coord_quickmap(xlim = range(catch_week$lon), ylim = range(catch_week$lat)) +
            ggplot2::ggtitle(label = paste(paste(title_chunk ,my_i," -- ",week),
                                    paste("CAPTURE TOTALE = ",tot_catch," (t)",sep = " "),sep = "\n"),
                      subtitle = "Espèce: Chinchard") +
            ggplot2::xlab("Longitude") + ylab("Latitude") +
            ggplot2::labs(fill = fill_chunk) +
            ggplot2::theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
                    panel.background = element_rect(fill = "aliceblue"))
        },height = 700, width = 550) 
      })
    }
  }
})

output$cpue_sardine_plots_ui <- renderUI({
  shiny::req(data_cpue())
  if(purrr::is_empty(data_cpue())){
    div()
  }else{
    plot_output_list <- lapply(1:data_cpue()$Nweeks, function(i) {
      plotname <- paste("cpue_sardine_plots_ui", i, sep="")
      list(
        div(
          plotOutput(plotname, height = 700, width = 550),
          style = "display:inline-block;"
        )
      )
    })
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  }
})

output$catch_sardine_plots_ui <- renderUI({
  shiny::req(catch_effort_data())
  if(purrr::is_empty(catch_effort_data())){
    div()
  }else{
    plot_output_list <- lapply(1:catch_effort_data()$Nweeks, function(i) {
      plotname <- paste("catch_sardine_plots_ui", i, sep="")
      list(
        div(
          plotOutput(plotname, height = 700, width = 550),
          style = "display:inline-block;"
        )
      )
    })
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  }
})

output$cpue_maquereau_plots_ui <- renderUI({
  shiny::req(data_cpue())
  if(purrr::is_empty(data_cpue())){
    div()
  }else{
    plot_output_list <- lapply(1:data_cpue()$Nweeks, function(i) {
      plotname <- paste("cpue_maquereau_plots_ui", i, sep="")
      list(
        div(
          plotOutput(plotname, height = 700, width = 550),
          style = "display:inline-block;"
        )
      )
    })
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  }

})

output$catch_maquereau_plots_ui <- renderUI({
  shiny::req(catch_effort_data())
  if(purrr::is_empty(catch_effort_data())){
    div()
  }else{
    plot_output_list <- lapply(1:catch_effort_data()$Nweeks, function(i) {
      plotname <- paste("catch_maquereau_plots_ui", i, sep="")
      list(
        div(
          plotOutput(plotname, height = 700, width = 550),
          style = "display:inline-block;"
        )
      )
    })
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list) 
  }
})

output$cpue_chinchard_plots_ui <- renderUI({
  shiny::req(data_cpue())
  if(purrr::is_empty(data_cpue())){
    div()
  }else{
    plot_output_list <- lapply(1:data_cpue()$Nweeks, function(i) {
      plotname <- paste("cpue_chinchard_plots_ui", i, sep="")
      list(
        div(
          plotOutput(plotname, height = 700, width = 550),
          style = "display:inline-block;"
        )
      )
    })
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  }
})

output$catch_chinchard_plots_ui <- renderUI({
  shiny::req(catch_effort_data())
  if(purrr::is_empty(catch_effort_data())){
    div()
  }else{
    plot_output_list <- lapply(1:catch_effort_data()$Nweeks, function(i) {
      plotname <- paste("catch_chinchard_plots_ui", i, sep="")
      list(
        div(
          plotOutput(plotname, height = 700, width = 550),
          style = "display:inline-block;"
        )
      )
    })
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  }
})

### SIMULATED FISHING EFFORT

shiny::observeEvent(input$btn_simulate_effort_ID, {
  shiny::req(catch_effort_data())
  for (i in 1:catch_effort_data()$Nweeks) {
    local({
      my_i <- i
      effort_week <- catch_effort_data()$data %>% janitor::clean_names() %>% dplyr::filter(date == catch_effort_data()$weeks[my_i])
      week <- catch_effort_data()$weeks[my_i]
      plotname <- paste("effort_peche_plots_ui", my_i, sep="")
      
      output[[plotname]] <- renderPlot({
        effort_week %>%
          ggplot2::ggplot() +
          ggplot2::theme_bw() +
          ggplot2::geom_tile(aes(lon, lat, fill = effort)) +
          ggplot2::scale_fill_viridis_c(option = "B", direction = -1) +
          # only data within the data-bounds
          ggplot2::borders(database = "worldHires",xlim = range(effort_week$lon), ylim = range(effort_week$lat),fill = "antiquewhite") +
          # limit plot
          ggplot2::coord_quickmap(xlim = range(effort_week$lon), ylim = range(effort_week$lat)) +
          ggplot2::ggtitle(label = paste("EFFORT DE PÊCHE SIMULÉ - SEMAINE N°",my_i," -- ",week)) +
          ggplot2::xlab("Longitude") + ylab("Latitude") +
          ggplot2::labs(fill = "Effort de pêche" ) +
          ggplot2::theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
                  panel.background = element_rect(fill = "aliceblue"))
      },height = 700, width = 550) 
    })
  }
})

output$effort_peche_plots_ui <- renderUI({
  shiny::req(catch_effort_data())
  plot_output_list <- lapply(1:catch_effort_data()$Nweeks, function(i) {
    plotname <- paste("effort_peche_plots_ui", i, sep="")
    list(
      div(
        plotOutput(plotname, height = 700, width = 550),
        style = "display:inline-block;"
      )
    )
  })
  # Convert the list to a tagList - this is necessary for the list of items
  # to display properly.
  do.call(tagList, plot_output_list)
})


output$cpue_catch_moyenne_plot <- renderPlot({
  shiny::req(catch_predictions())
  data_preds <- catch_predictions()$catch_predictions
  
  weekly_sardine_catch <- data_preds$sardine %>% dplyr::group_by(date) %>%
    dplyr::summarise(Sardine = sum(pred_weight_t))
  
  weekly_maquereau_catch <- data_preds$maquereau %>% dplyr::group_by(date) %>%
    dplyr::summarise(Maquereau = sum(pred_weight_t))
  
  weekly_chinchard_catch <- data_preds$chinchard %>% dplyr::group_by(date) %>%
    dplyr::summarise(Chinchard = sum(pred_weight_t))
  
  df <- weekly_sardine_catch %>%
    dplyr::left_join(weekly_maquereau_catch,by = "date") %>%
    dplyr::left_join(weekly_chinchard_catch,by = "date")
  
  fig <- df %>%
    pivot_longer(!date, names_to = "Espece",values_to = "Captures") %>%
    ggplot2::ggplot(aes(x = date,y = Captures, color = Espece)) + geom_line(size = 2) + 
    ggplot2::scale_x_date(date_labels = "%d/%b", breaks = unique(df$date)) +
    ggplot2::scale_fill_brewer(palette="Paired") + 
    ggplot2::theme(legend.position="top", text = element_text(size = 12),legend.title = element_blank()) + 
    ggplot2::xlab(label = "Date")
    
  fig
})

output$catch_totale_mensuelle_plot <- renderPlot({
  
  shiny::req(catch_predictions())
  data_preds <- catch_predictions()$catch_predictions
  
  weekly_sardine_catch <- data_preds$sardine %>% 
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::mutate(month = lubridate::month(date)) %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(Sardine = sum(pred_weight_t))
  
  weekly_maquereau_catch <- data_preds$maquereau %>% 
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::mutate(month = lubridate::month(date)) %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(Maquereau = sum(pred_weight_t))
  
  weekly_chinchard_catch <- data_preds$chinchard %>% 
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::mutate(month = lubridate::month(date)) %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(Chinchard = sum(pred_weight_t))
  
  df <- weekly_sardine_catch %>%
    dplyr::left_join(weekly_maquereau_catch,by = "month") %>%
    dplyr::left_join(weekly_chinchard_catch,by = "month") %>%
    pivot_longer(!month, names_to = "Espece",values_to = "Captures")
  
  
  fig <- ggplot(data=df, aes(x=month.abb[month], y=Captures, fill=Espece)) +
    ggplot2::geom_bar(stat="identity", position=position_dodge())+
    geom_text(aes(label=round(Captures)), position=position_dodge(width=0.9), vjust=-0.25) + 
    ggplot2::scale_fill_brewer(palette="Paired")+  
    ggplot2::theme(legend.position="top", text = element_text(size = 12), legend.title = element_blank())+
    ggplot2::xlab(label = "Mois")
  
  fig 
  
})