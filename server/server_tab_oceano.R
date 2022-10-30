##############################
# server_tab_oceano.R
#
# 
# 
##############################
### SST
observeEvent(input$sst_btn_ID,{
  shinyalert(title = "Loading", 
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             showCancelButton = F, 
             showConfirmButton = F, 
             size = "xs")
  output$title_variable_box_ID <- renderText({
    "INDICE: SEA SURFACE TEMPERATURE"
  })
  updateButton(session, "sst_btn_ID",style = "success") 
  updateButton(session, "ekman_btn_ID",style = "primary") 
  updateButton(session, "nao_btn_ID",style = "primary") 
  updateButton(session, "o2_btn_ID",style = "primary") 
  updateButton(session, "chl_btn_ID",style = "primary") 
  updateButton(session, "pp_btn_ID",style = "primary") 
  updateButton(session, "temp_btn_ID",style = "primary") 
  updateButton(session, "sal_btn_ID",style = "primary") 
  updateButton(session, "u_phys_btn_ID",style = "primary") 
  updateButton(session, "v_phys_btn_ID",style = "primary") 
  updateButton(session, "pres_btn_ID",style = "primary") 
  updateButton(session, "u_meteo_btn_ID",style = "primary") 
  updateButton(session, "v_meteo_btn_ID",style = "primary") 
  updateButton(session, "per_btn_ID",style = "primary") 
  updateButton(session, "dir_btn_ID",style = "primary") 
  updateButton(session, "hs_btn_ID",style = "primary") 
  
  conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
  sst <- dbGetQuery(conn = conn,statement = "select * from oceano_sst_table where Latitude >= 35 or Latitude <= 28") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = ifelse(test = Latitude <= 28,yes = "Atlantique Sud",no = "Méditerranée"))
  
  count <- nrow(sst)
  Missing.values <- sum(is.na(sst$indice_upw_sst))
  Mean <- round(mean(sst$indice_upw_sst,na.rm = TRUE),digits = 8)
  Minimum <- round(min(sst$indice_upw_sst,na.rm = TRUE),digits = 8)
  Maximum <- round(max(sst$indice_upw_sst,na.rm = TRUE),digits = 8)
  
  quantile5th <- round(quantile(sst$indice_upw_sst,probs = 0.05,na.rm = TRUE),digits = 8)
  quantile25th <- round(quantile(sst$indice_upw_sst,probs = 0.25,na.rm = TRUE),digits = 8)
  quantile50th <- round(quantile(sst$indice_upw_sst,probs = 0.50,na.rm = TRUE),digits = 8)
  quantile75th <- round(quantile(sst$indice_upw_sst,probs = 0.75,na.rm = TRUE),digits = 8)
  quantile95th <- round(quantile(sst$indice_upw_sst,probs = 0.95,na.rm = TRUE),digits = 8)
  
  StdDev <- round(sd(sst$indice_upw_sst,na.rm = TRUE),digits = 8)
  CV <- round(StdDev/Mean,digits = TRUE)
  
  Kurtosis <- round(moments::kurtosis(x = sst$indice_upw_sst,na.rm = TRUE),digits = 8)
  Skewnes <- round(moments::skewness(x = sst$indice_upw_sst,na.rm = TRUE),digits = 8)
  
  Somme <- round(sum(sst$indice_upw_sst,na.rm = TRUE),digits = 8)
  Variance <- round(var(sst$indice_upw_sst,na.rm = TRUE),digits = 8)
  
  Min.Date <- min(sst$Date,na.rm = TRUE)
  Max.Date <- max(sst$Date,na.rm = TRUE)
  
  sst_tabl_smry <- data.frame("Stats" = c("Count","Missing Values","Mean","Minimum","Maximum","Min.Date","Max.Date"), 
                              "Value" = c(count,Missing.values,Mean,Minimum,Maximum,Min.Date,Max.Date))
  
  sst_tabl_quant <- data.frame("Quantile Statistics" = c("Minimum","5th Percentile","Q1","Median","Q3","95th Percentile","Maximum"),
                               "Value" = c(Minimum,quantile5th,quantile25th,quantile50th,quantile75th,quantile95th,Maximum))
  
  sst_tabl_descr <- data.frame("Descriptive Statistics" = c("Standard Deviation","Coefficient of Variation (CV)","Kurtosis","Mean","Skewness","Sum","Variance"),
                               "Value" = c(StdDev,CV,Kurtosis,Mean,Skewnes,Somme,Variance))
  
  names(sst_tabl_descr) <- c("Descriptive Statistics","Value")
  names(sst_tabl_quant) <- c("Quantile Statistics","Value")
  
  sst_tabl_smry$Value[6] <- as.character(Min.Date)
  sst_tabl_smry$Value[7] <- as.character(Max.Date)
  
  output$oceano_var_smry <- renderTable(digits = 6,{
    sst_tabl_smry
  })
  
  output$oceano_var_plot <- renderPlot({
    ggplot(sst, aes(indice_upw_sst, group = Zone, fill = Zone)) +
      geom_histogram(aes(y=..density..), alpha=0.5, position="identity",
                     na.rm = TRUE,bins = 100, color = "gray") + geom_density(alpha=.2) +
      theme_minimal(base_size = 20)+
      theme(legend.position="bottom",legend.title = element_blank()) + 
      xlab(label = "") + ylab("") 
  },height = 300)
  
  output$oceano_var_quantiles <- renderTable(digits = 6,{
    sst_tabl_quant
  },width = "100%")
  
  output$oceano_var_stats <- renderTable(digits = 6,{
    sst_tabl_descr
  },width = "100%")
  
  DBI::dbDisconnect(conn)
  shinyalert(title = "", immediate = TRUE,
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             timer = 100)
})

### EKMAN
observeEvent(input$ekman_btn_ID,{
  shinyalert(title = "Loading", 
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             showCancelButton = F, 
             showConfirmButton = F, 
             size = "xs")
  output$title_variable_box_ID <- renderText({
    "INDICE: EKMAN TRANSPORT"
  })
  updateButton(session, "sst_btn_ID",style = "primary") 
  updateButton(session, "ekman_btn_ID",style = "success") 
  updateButton(session, "nao_btn_ID",style = "primary") 
  updateButton(session, "o2_btn_ID",style = "primary") 
  updateButton(session, "chl_btn_ID",style = "primary") 
  updateButton(session, "pp_btn_ID",style = "primary") 
  updateButton(session, "temp_btn_ID",style = "primary") 
  updateButton(session, "sal_btn_ID",style = "primary") 
  updateButton(session, "u_phys_btn_ID",style = "primary") 
  updateButton(session, "v_phys_btn_ID",style = "primary") 
  updateButton(session, "pres_btn_ID",style = "primary") 
  updateButton(session, "u_meteo_btn_ID",style = "primary") 
  updateButton(session, "v_meteo_btn_ID",style = "primary") 
  updateButton(session, "per_btn_ID",style = "primary") 
  updateButton(session, "dir_btn_ID",style = "primary") 
  updateButton(session, "hs_btn_ID",style = "primary") 
  
  conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
  ekman <- dbGetQuery(conn = conn,statement = "select * from oceano_ekman_table where Latitude >= 35 or Latitude <= 28") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = ifelse(test = Latitude <= 28,yes = "Atlantique Sud",no = "Méditerranée"))
  
  count <- nrow(ekman)
  Missing.values <- sum(is.na(ekman$Indice_upw_Ekman))
  Mean <- round(mean(ekman$Indice_upw_Ekman,na.rm = TRUE),digits = 8)
  Minimum <- round(min(ekman$Indice_upw_Ekman,na.rm = TRUE),digits = 8)
  Maximum <- round(max(ekman$Indice_upw_Ekman,na.rm = TRUE),digits = 8)
  
  quantile5th <- round(quantile(ekman$Indice_upw_Ekman,probs = 0.05,na.rm = TRUE),digits = 8)
  quantile25th <- round(quantile(ekman$Indice_upw_Ekman,probs = 0.25,na.rm = TRUE),digits = 8)
  quantile50th <- round(quantile(ekman$Indice_upw_Ekman,probs = 0.50,na.rm = TRUE),digits = 8)
  quantile75th <- round(quantile(ekman$Indice_upw_Ekman,probs = 0.75,na.rm = TRUE),digits = 8)
  quantile95th <- round(quantile(ekman$Indice_upw_Ekman,probs = 0.95,na.rm = TRUE),digits = 8)
  
  StdDev <- round(sd(ekman$Indice_upw_Ekman,na.rm = TRUE),digits = 8)
  CV <- round(StdDev/Mean,digits = TRUE)
  
  Kurtosis <- round(moments::kurtosis(x = ekman$Indice_upw_Ekman,na.rm = TRUE),digits = 8)
  Skewnes <- round(moments::skewness(x = ekman$Indice_upw_Ekman,na.rm = TRUE),digits = 8)
  
  Somme <- round(sum(ekman$Indice_upw_Ekman,na.rm = TRUE),digits = 8)
  Variance <- round(var(ekman$Indice_upw_Ekman,na.rm = TRUE),digits = 8)
  
  Min.Date <- min(ekman$Date,na.rm = TRUE)
  Max.Date <- max(ekman$Date,na.rm = TRUE)
  
  ekman_tabl_smry <- data.frame("Stats" = c("Count","Missing Values","Mean","Minimum","Maximum","Min.Date","Max.Date"), 
                                "Value" = c(count,Missing.values,Mean,Minimum,Maximum,Min.Date,Max.Date))
  
  ekman_tabl_quant <- data.frame("Quantile Statistics" = c("Minimum","5th Percentile","Q1","Median","Q3","95th Percentile","Maximum"),
                                 "Value" = c(Minimum,quantile5th,quantile25th,quantile50th,quantile75th,quantile95th,Maximum))
  
  ekman_tabl_descr <- data.frame("Descriptive Statistics" = c("Standard Deviation","Coefficient of Variation (CV)","Kurtosis","Mean","Skewness","Sum","Variance"),
                                 "Value" = c(StdDev,CV,Kurtosis,Mean,Skewnes,Somme,Variance))
  
  names(ekman_tabl_descr) <- c("Descriptive Statistics","Value")
  names(ekman_tabl_quant) <- c("Quantile Statistics","Value")
  
  ekman_tabl_smry$Value[6] <- as.character(Min.Date)
  ekman_tabl_smry$Value[7] <- as.character(Max.Date)
  
  output$oceano_var_smry <- renderTable(digits = 6,{
    ekman_tabl_smry
  })
  
  output$oceano_var_plot <- renderPlot({
    ggplot(ekman, aes(Indice_upw_Ekman,group = Zone, fill = Zone)) +
      geom_histogram(aes(y=..density..), alpha=0.5, position="identity",
                     na.rm = TRUE,bins = 100, color = "gray") + geom_density(alpha=.2) +
      theme_minimal(base_size = 20)+
      theme(legend.position="bottom",legend.title = element_blank()) + 
      xlab(label = "") + ylab("") 
  },height = 300)
  
  output$oceano_var_quantiles <- renderTable(digits = 6,{
    ekman_tabl_quant
  },width = "100%")
  
  output$oceano_var_stats <- renderTable(digits = 6,{
    ekman_tabl_descr
  },width = "100%")
  
  DBI::dbDisconnect(conn)
  shinyalert(title = "", immediate = TRUE,
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             timer = 100)
})

### NAO
observeEvent(input$nao_btn_ID,{
  shinyalert(title = "Loading", 
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             showCancelButton = F, 
             showConfirmButton = F, 
             size = "xs")
  output$title_variable_box_ID <- renderText({
    "INDICE: North Atlantic Oscillation"
  })
  updateButton(session, "sst_btn_ID",style = "primary") 
  updateButton(session, "ekman_btn_ID",style = "primary") 
  updateButton(session, "nao_btn_ID",style = "success") 
  updateButton(session, "o2_btn_ID",style = "primary") 
  updateButton(session, "chl_btn_ID",style = "primary") 
  updateButton(session, "pp_btn_ID",style = "primary") 
  updateButton(session, "temp_btn_ID",style = "primary") 
  updateButton(session, "sal_btn_ID",style = "primary") 
  updateButton(session, "u_phys_btn_ID",style = "primary") 
  updateButton(session, "v_phys_btn_ID",style = "primary") 
  updateButton(session, "pres_btn_ID",style = "primary") 
  updateButton(session, "u_meteo_btn_ID",style = "primary") 
  updateButton(session, "v_meteo_btn_ID",style = "primary") 
  updateButton(session, "per_btn_ID",style = "primary") 
  updateButton(session, "dir_btn_ID",style = "primary") 
  updateButton(session, "hs_btn_ID",style = "primary") 
  
  conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
  nao <- dbGetQuery(conn = conn,statement = "select * from oceano_nao_table") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y"))
  
  count <- nrow(nao)
  Missing.values <- sum(is.na(nao$NAO_mean))
  Mean <- round(mean(nao$NAO_mean,na.rm = TRUE),digits = 8)
  Minimum <- round(min(nao$NAO_mean,na.rm = TRUE),digits = 8)
  Maximum <- round(max(nao$NAO_mean,na.rm = TRUE),digits = 8)
  
  quantile5th <- round(quantile(nao$NAO_mean,probs = 0.05,na.rm = TRUE),digits = 8)
  quantile25th <- round(quantile(nao$NAO_mean,probs = 0.25,na.rm = TRUE),digits = 8)
  quantile50th <- round(quantile(nao$NAO_mean,probs = 0.50,na.rm = TRUE),digits = 8)
  quantile75th <- round(quantile(nao$NAO_mean,probs = 0.75,na.rm = TRUE),digits = 8)
  quantile95th <- round(quantile(nao$NAO_mean,probs = 0.95,na.rm = TRUE),digits = 8)
  
  StdDev <- round(mean(nao$NAO_std,na.rm = TRUE),digits = 8)
  CV <- round(StdDev/Mean,digits = TRUE)
  
  Kurtosis <- round(moments::kurtosis(x = nao$NAO_mean,na.rm = TRUE),digits = 8)
  Skewnes <- round(moments::skewness(x = nao$NAO_mean,na.rm = TRUE),digits = 8)
  
  Somme <- round(sum(nao$NAO_mean,na.rm = TRUE),digits = 8)
  Variance <- round(StdDev^2,digits = 8)
  
  Min.Date <- min(nao$Date,na.rm = TRUE)
  Max.Date <- max(nao$Date,na.rm = TRUE)
  
  nao_tabl_smry <- data.frame("Stats" = c("Count","Missing Values","Mean","Minimum","Maximum","Min.Date","Max.Date"), 
                              "Value" = c(count,Missing.values,Mean,Minimum,Maximum,Min.Date,Max.Date))
  
  nao_tabl_quant <- data.frame("Quantile Statistics" = c("Minimum","5th Percentile","Q1","Median","Q3","95th Percentile","Maximum"),
                               "Value" = c(Minimum,quantile5th,quantile25th,quantile50th,quantile75th,quantile95th,Maximum))
  
  nao_tabl_descr <- data.frame("Descriptive Statistics" = c("Standard Deviation","Coefficient of Variation (CV)","Kurtosis","Mean","Skewness","Sum","Variance"),
                               "Value" = c(StdDev,CV,Kurtosis,Mean,Skewnes,Somme,Variance))
  
  names(nao_tabl_descr) <- c("Descriptive Statistics","Value")
  names(nao_tabl_quant) <- c("Quantile Statistics","Value")
  
  nao_tabl_smry$Value[6] <- as.character(Min.Date)
  nao_tabl_smry$Value[7] <- as.character(Max.Date)
  
  output$oceano_var_smry <- renderTable(digits = 6,{
    nao_tabl_smry
  })
  
  output$oceano_var_plot <- renderPlot({
    ggplot(nao, aes(NAO_mean, fill = cut(NAO_mean, 100))) +
      geom_histogram(show.legend = FALSE,na.rm = TRUE,bins = 100,color="darkblue", fill="lightblue") +
      theme_minimal(base_size = 20)+
      theme(legend.position="bottom",legend.title = element_blank()) + 
      xlab(label = "") + ylab("")
  },height = 300)
  
  output$oceano_var_quantiles <- renderTable(digits = 6,{
    nao_tabl_quant
  },width = "100%")
  
  output$oceano_var_stats <- renderTable(digits = 6,{
    nao_tabl_descr
  },width = "100%")
  
  DBI::dbDisconnect(conn)
  shinyalert(title = "", immediate = TRUE,
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             timer = 100)
})

### O2
observeEvent(input$o2_btn_ID,{
  shinyalert(title = "Loading", 
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             showCancelButton = F, 
             showConfirmButton = F, 
             size = "xs")
  output$title_variable_box_ID <- renderText({
    "BIOLOGICAL INDICATORS: OXYGEN DISSOUS"
  })
  updateButton(session, "sst_btn_ID",style = "primary") 
  updateButton(session, "ekman_btn_ID",style = "primary") 
  updateButton(session, "nao_btn_ID",style = "primary") 
  updateButton(session, "o2_btn_ID",style = "success") 
  updateButton(session, "chl_btn_ID",style = "primary") 
  updateButton(session, "pp_btn_ID",style = "primary") 
  updateButton(session, "temp_btn_ID",style = "primary") 
  updateButton(session, "sal_btn_ID",style = "primary") 
  updateButton(session, "u_phys_btn_ID",style = "primary") 
  updateButton(session, "v_phys_btn_ID",style = "primary") 
  updateButton(session, "pres_btn_ID",style = "primary") 
  updateButton(session, "u_meteo_btn_ID",style = "primary") 
  updateButton(session, "v_meteo_btn_ID",style = "primary") 
  updateButton(session, "per_btn_ID",style = "primary") 
  updateButton(session, "dir_btn_ID",style = "primary") 
  updateButton(session, "hs_btn_ID",style = "primary") 
  
  conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
  o2_sud <- dbGetQuery(conn = conn,statement = "select Date, O2_mean, O2_std from oceano_biology_south_table") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = "Atlantique Sud")
  
  o2_med <- dbGetQuery(conn = conn,statement = "select Date, O2_mean, O2_std from oceano_biology_med_table where Latitude >= 35") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = "Méditerranée")
  
  o2 <- o2_sud %>% bind_rows(o2_med)
  
  count <- nrow(o2)
  Missing.values <- sum(is.na(o2$O2_mean))
  Mean <- round(mean(o2$O2_mean,na.rm = TRUE),digits = 8)
  Minimum <- round(min(o2$O2_mean,na.rm = TRUE),digits = 8)
  Maximum <- round(max(o2$O2_mean,na.rm = TRUE),digits = 8)
  
  quantile5th <- round(quantile(o2$O2_mean,probs = 0.05,na.rm = TRUE),digits = 8)
  quantile25th <- round(quantile(o2$O2_mean,probs = 0.25,na.rm = TRUE),digits = 8)
  quantile50th <- round(quantile(o2$O2_mean,probs = 0.50,na.rm = TRUE),digits = 8)
  quantile75th <- round(quantile(o2$O2_mean,probs = 0.75,na.rm = TRUE),digits = 8)
  quantile95th <- round(quantile(o2$O2_mean,probs = 0.95,na.rm = TRUE),digits = 8)
  
  StdDev <- round(mean(o2$O2_std,na.rm = TRUE),digits = 8)
  CV <- round(StdDev/Mean,digits = TRUE)
  
  Kurtosis <- round(moments::kurtosis(x = o2$O2_mean,na.rm = TRUE),digits = 8)
  Skewnes <- round(moments::skewness(x = o2$O2_mean,na.rm = TRUE),digits = 8)
  
  Somme <- round(sum(o2$O2_mean,na.rm = TRUE),digits = 8)
  Variance <- round((StdDev)^2,digits = 8)
  
  Min.Date <- min(o2$Date,na.rm = TRUE)
  Max.Date <- max(o2$Date,na.rm = TRUE)
  
  o2_tabl_smry <- data.frame("Stats" = c("Count","Missing Values","Mean","Minimum","Maximum","Min.Date","Max.Date"), 
                             "Value" = c(count,Missing.values,Mean,Minimum,Maximum,Min.Date,Max.Date))
  
  o2_tabl_quant <- data.frame("Quantile Statistics" = c("Minimum","5th Percentile","Q1","Median","Q3","95th Percentile","Maximum"),
                              "Value" = c(Minimum,quantile5th,quantile25th,quantile50th,quantile75th,quantile95th,Maximum))
  
  o2_tabl_descr <- data.frame("Descriptive Statistics" = c("Standard Deviation","Coefficient of Variation (CV)","Kurtosis","Mean","Skewness","Sum","Variance"),
                              "Value" = c(StdDev,CV,Kurtosis,Mean,Skewnes,Somme,Variance))
  
  names(o2_tabl_descr) <- c("Descriptive Statistics","Value")
  names(o2_tabl_quant) <- c("Quantile Statistics","Value")
  
  o2_tabl_smry$Value[6] <- as.character(Min.Date)
  o2_tabl_smry$Value[7] <- as.character(Max.Date)
  
  output$oceano_var_smry <- renderTable(digits = 6,{
    o2_tabl_smry
  })
  
  output$oceano_var_plot <- renderPlot({
    ggplot(o2, aes(O2_mean, group = Zone, fill = Zone)) +
      geom_histogram(aes(y=..density..), alpha=0.5, position="identity",
                     na.rm = TRUE,bins = 100, color = "gray") + geom_density(alpha=.2) +
      theme_minimal(base_size = 20)+
      theme(legend.position="bottom",legend.title = element_blank()) + 
      xlab(label = "") + ylab("") 
  },height = 300)
  
  output$oceano_var_quantiles <- renderTable(digits = 6,{
    o2_tabl_quant
  },width = "100%")
  
  output$oceano_var_stats <- renderTable(digits = 6,{
    o2_tabl_descr
  },width = "100%")
  
  DBI::dbDisconnect(conn)
  shinyalert(title = "", immediate = TRUE,
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             timer = 100)
})

### CHL
observeEvent(input$chl_btn_ID,{
  shinyalert(title = "Loading", 
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             showCancelButton = F, 
             showConfirmButton = F, 
             size = "xs")
  output$title_variable_box_ID <- renderText({
    "BIOLOGICAL INDICATORS: CHLOROPHYL"
  })
  updateButton(session, "sst_btn_ID",style = "primary") 
  updateButton(session, "ekman_btn_ID",style = "primary") 
  updateButton(session, "nao_btn_ID",style = "primary") 
  updateButton(session, "o2_btn_ID",style = "primary") 
  updateButton(session, "chl_btn_ID",style = "success") 
  updateButton(session, "pp_btn_ID",style = "primary") 
  updateButton(session, "temp_btn_ID",style = "primary") 
  updateButton(session, "sal_btn_ID",style = "primary") 
  updateButton(session, "u_phys_btn_ID",style = "primary") 
  updateButton(session, "v_phys_btn_ID",style = "primary") 
  updateButton(session, "pres_btn_ID",style = "primary") 
  updateButton(session, "u_meteo_btn_ID",style = "primary") 
  updateButton(session, "v_meteo_btn_ID",style = "primary") 
  updateButton(session, "per_btn_ID",style = "primary") 
  updateButton(session, "dir_btn_ID",style = "primary") 
  updateButton(session, "hs_btn_ID",style = "primary") 
  
  conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
  chl_sud <- dbGetQuery(conn = conn,statement = "select Date, CHL_mean, CHL_std from oceano_biology_south_table") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = "Atlantique Sud")
  
  chl_med <- dbGetQuery(conn = conn,statement = "select Date, CHL_mean, CHL_std from oceano_biology_med_table where Latitude >= 35") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = "Méditerranée")
  
  chl <- chl_sud %>% bind_rows(chl_med)
  
  count <- nrow(chl)
  Missing.values <- sum(is.na(chl$CHL_mean))
  Mean <- round(mean(chl$CHL_mean,na.rm = TRUE),digits = 8)
  Minimum <- round(min(chl$CHL_mean,na.rm = TRUE),digits = 8)
  Maximum <- round(max(chl$CHL_mean,na.rm = TRUE),digits = 8)
  
  quantile5th <- round(quantile(chl$CHL_mean,probs = 0.05,na.rm = TRUE),digits = 8)
  quantile25th <- round(quantile(chl$CHL_mean,probs = 0.25,na.rm = TRUE),digits = 8)
  quantile50th <- round(quantile(chl$CHL_mean,probs = 0.50,na.rm = TRUE),digits = 8)
  quantile75th <- round(quantile(chl$CHL_mean,probs = 0.75,na.rm = TRUE),digits = 8)
  quantile95th <- round(quantile(chl$CHL_mean,probs = 0.95,na.rm = TRUE),digits = 8)
  
  StdDev <- round(mean(chl$CHL_std,na.rm = TRUE),digits = 8)
  CV <- round(StdDev/Mean,digits = TRUE)
  
  Kurtosis <- round(moments::kurtosis(x = chl$CHL_mean,na.rm = TRUE),digits = 8)
  Skewnes <- round(moments::skewness(x = chl$CHL_mean,na.rm = TRUE),digits = 8)
  
  Somme <- round(sum(chl$CHL_mean,na.rm = TRUE),digits = 8)
  Variance <- round((StdDev)^2,digits = 8)
  
  Min.Date <- min(chl$Date,na.rm = TRUE)
  Max.Date <- max(chl$Date,na.rm = TRUE)
  
  chl_tabl_smry <- data.frame("Stats" = c("Count","Missing Values","Mean","Minimum","Maximum","Min.Date","Max.Date"), 
                              "Value" = c(count,Missing.values,Mean,Minimum,Maximum,Min.Date,Max.Date))
  
  chl_tabl_quant <- data.frame("Quantile Statistics" = c("Minimum","5th Percentile","Q1","Median","Q3","95th Percentile","Maximum"),
                               "Value" = c(Minimum,quantile5th,quantile25th,quantile50th,quantile75th,quantile95th,Maximum))
  
  chl_tabl_descr <- data.frame("Descriptive Statistics" = c("Standard Deviation","Coefficient of Variation (CV)","Kurtosis","Mean","Skewness","Sum","Variance"),
                               "Value" = c(StdDev,CV,Kurtosis,Mean,Skewnes,Somme,Variance))
  
  names(chl_tabl_descr) <- c("Descriptive Statistics","Value")
  names(chl_tabl_quant) <- c("Quantile Statistics","Value")
  
  chl_tabl_smry$Value[6] <- as.character(Min.Date)
  chl_tabl_smry$Value[7] <- as.character(Max.Date)
  
  output$oceano_var_smry <- renderTable(digits = 6,{
    chl_tabl_smry
  })
  
  output$oceano_var_plot <- renderPlot({
    ggplot(chl, aes(CHL_mean, group = Zone, fill = Zone)) +
      geom_histogram(aes(y=..density..), alpha=0.5, position="identity",
                     na.rm = TRUE,bins = 100, color = "gray") + geom_density(alpha=.2) +
      theme_minimal(base_size = 20)+
      theme(legend.position="bottom",legend.title = element_blank()) + 
      xlab(label = "") + ylab("") 
  },height = 300)
  
  output$oceano_var_quantiles <- renderTable(digits = 6,{
    chl_tabl_quant
  },width = "100%")
  
  output$oceano_var_stats <- renderTable(digits = 6,{
    chl_tabl_descr
  },width = "100%")
  
  DBI::dbDisconnect(conn)
  shinyalert(title = "", immediate = TRUE,
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             timer = 100)
})

### PP
observeEvent(input$pp_btn_ID,{
  shinyalert(title = "Loading", 
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             showCancelButton = F, 
             showConfirmButton = F, 
             size = "xs")
  output$title_variable_box_ID <- renderText({
    "BIOLOGICAL INDICATORS: PRIMARY PRODUCTION"
  })
  updateButton(session, "sst_btn_ID",style = "primary") 
  updateButton(session, "ekman_btn_ID",style = "primary") 
  updateButton(session, "nao_btn_ID",style = "primary") 
  updateButton(session, "o2_btn_ID",style = "primary") 
  updateButton(session, "chl_btn_ID",style = "primary") 
  updateButton(session, "pp_btn_ID",style = "success") 
  updateButton(session, "temp_btn_ID",style = "primary") 
  updateButton(session, "sal_btn_ID",style = "primary") 
  updateButton(session, "u_phys_btn_ID",style = "primary") 
  updateButton(session, "v_phys_btn_ID",style = "primary") 
  updateButton(session, "pres_btn_ID",style = "primary") 
  updateButton(session, "u_meteo_btn_ID",style = "primary") 
  updateButton(session, "v_meteo_btn_ID",style = "primary") 
  updateButton(session, "per_btn_ID",style = "primary") 
  updateButton(session, "dir_btn_ID",style = "primary") 
  updateButton(session, "hs_btn_ID",style = "primary") 
  
  conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
  pp_sud <- dbGetQuery(conn = conn,statement = "select Date, PP_mean, PP_std from oceano_biology_south_table") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = "Atlantique Sud")
  
  pp_med <- dbGetQuery(conn = conn,statement = "select Date, PP_mean, PP_std from oceano_biology_med_table where Latitude >= 35") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = "Méditerranée")
  
  pp <- pp_sud %>% bind_rows(pp_med)
  
  count <- nrow(pp)
  Missing.values <- sum(is.na(pp$PP_mean))
  Mean <- round(mean(pp$PP_mean,na.rm = TRUE),digits = 8)
  Minimum <- round(min(pp$PP_mean,na.rm = TRUE),digits = 8)
  Maximum <- round(max(pp$PP_mean,na.rm = TRUE),digits = 8)
  
  quantile5th <- round(quantile(pp$PP_mean,probs = 0.05,na.rm = TRUE),digits = 8)
  quantile25th <- round(quantile(pp$PP_mean,probs = 0.25,na.rm = TRUE),digits = 8)
  quantile50th <- round(quantile(pp$PP_mean,probs = 0.50,na.rm = TRUE),digits = 8)
  quantile75th <- round(quantile(pp$PP_mean,probs = 0.75,na.rm = TRUE),digits = 8)
  quantile95th <- round(quantile(pp$PP_mean,probs = 0.95,na.rm = TRUE),digits = 8)
  
  StdDev <- round(mean(pp$PP_std,na.rm = TRUE),digits = 8)
  CV <- round(StdDev/Mean,digits = TRUE)
  
  Kurtosis <- round(moments::kurtosis(x = pp$PP_mean,na.rm = TRUE),digits = 8)
  Skewnes <- round(moments::skewness(x = pp$PP_mean,na.rm = TRUE),digits = 8)
  
  Somme <- round(sum(pp$PP_mean,na.rm = TRUE),digits = 8)
  Variance <- round((StdDev)^2,digits = 8)
  
  Min.Date <- min(pp$Date,na.rm = TRUE)
  Max.Date <- max(pp$Date,na.rm = TRUE)
  
  pp_tabl_smry <- data.frame("Stats" = c("Count","Missing Values","Mean","Minimum","Maximum","Min.Date","Max.Date"), 
                             "Value" = c(count,Missing.values,Mean,Minimum,Maximum,Min.Date,Max.Date))
  
  pp_tabl_quant <- data.frame("Quantile Statistics" = c("Minimum","5th Percentile","Q1","Median","Q3","95th Percentile","Maximum"),
                              "Value" = c(Minimum,quantile5th,quantile25th,quantile50th,quantile75th,quantile95th,Maximum))
  
  pp_tabl_descr <- data.frame("Descriptive Statistics" = c("Standard Deviation","Coefficient of Variation (CV)","Kurtosis","Mean","Skewness","Sum","Variance"),
                              "Value" = c(StdDev,CV,Kurtosis,Mean,Skewnes,Somme,Variance))
  
  names(pp_tabl_descr) <- c("Descriptive Statistics","Value")
  names(pp_tabl_quant) <- c("Quantile Statistics","Value")
  
  pp_tabl_smry$Value[6] <- as.character(Min.Date)
  pp_tabl_smry$Value[7] <- as.character(Max.Date)
  
  output$oceano_var_smry <- renderTable(digits = 6,{
    pp_tabl_smry
  })
  
  output$oceano_var_plot <- renderPlot({
    ggplot(pp, aes(PP_mean, group = Zone, fill = Zone)) +
      geom_histogram(aes(y=..density..), alpha=0.5, position="identity",
                     na.rm = TRUE,bins = 100, color = "gray") + geom_density(alpha=.2) +
      theme_minimal(base_size = 20)+
      theme(legend.position="bottom",legend.title = element_blank()) + 
      xlab(label = "") + ylab("") 
  },height = 300)
  
  output$oceano_var_quantiles <- renderTable(digits = 6,{
    pp_tabl_quant
  },width = "100%")
  
  output$oceano_var_stats <- renderTable(digits = 6,{
    pp_tabl_descr
  },width = "100%")
  
  DBI::dbDisconnect(conn)
  shinyalert(title = "", immediate = TRUE,
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             timer = 100)
})

### TEMP
observeEvent(input$temp_btn_ID,{
  shinyalert(title = "Loading", 
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             showCancelButton = F, 
             showConfirmButton = F, 
             size = "xs")
  output$title_variable_box_ID <- renderText({
    "PHYSICAL INDICATORS: TEMPERATURE"
  })
  updateButton(session, "sst_btn_ID",style = "primary") 
  updateButton(session, "ekman_btn_ID",style = "primary") 
  updateButton(session, "nao_btn_ID",style = "primary") 
  updateButton(session, "o2_btn_ID",style = "primary") 
  updateButton(session, "chl_btn_ID",style = "primary") 
  updateButton(session, "pp_btn_ID",style = "primary") 
  updateButton(session, "temp_btn_ID",style = "success") 
  updateButton(session, "sal_btn_ID",style = "primary") 
  updateButton(session, "u_phys_btn_ID",style = "primary") 
  updateButton(session, "v_phys_btn_ID",style = "primary") 
  updateButton(session, "pres_btn_ID",style = "primary") 
  updateButton(session, "u_meteo_btn_ID",style = "primary") 
  updateButton(session, "v_meteo_btn_ID",style = "primary") 
  updateButton(session, "per_btn_ID",style = "primary") 
  updateButton(session, "dir_btn_ID",style = "primary") 
  updateButton(session, "hs_btn_ID",style = "primary") 
  
  conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
  temp_sud <- dbGetQuery(conn = conn,statement = "select Date, TEMP_mean, TEMP_std from oceano_physique_south_table") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = "Atlantique Sud")
  
  temp_med <- dbGetQuery(conn = conn,statement = "select Date, TEMP_mean, TEMP_std from oceano_physique_med_table where Latitude >= 35") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = "Méditerranée")
  
  temp <- temp_sud %>% bind_rows(temp_med)
  
  count <- nrow(temp)
  Missing.values <- sum(is.na(temp$TEMP_mean))
  Mean <- round(mean(temp$TEMP_mean,na.rm = TRUE),digits = 8)
  Minimum <- round(min(temp$TEMP_mean,na.rm = TRUE),digits = 8)
  Maximum <- round(max(temp$TEMP_mean,na.rm = TRUE),digits = 8)
  
  quantile5th <- round(quantile(temp$TEMP_mean,probs = 0.05,na.rm = TRUE),digits = 8)
  quantile25th <- round(quantile(temp$TEMP_mean,probs = 0.25,na.rm = TRUE),digits = 8)
  quantile50th <- round(quantile(temp$TEMP_mean,probs = 0.50,na.rm = TRUE),digits = 8)
  quantile75th <- round(quantile(temp$TEMP_mean,probs = 0.75,na.rm = TRUE),digits = 8)
  quantile95th <- round(quantile(temp$TEMP_mean,probs = 0.95,na.rm = TRUE),digits = 8)
  
  StdDev <- round(mean(temp$TEMP_std,na.rm = TRUE),digits = 8)
  CV <- round(StdDev/Mean,digits = TRUE)
  
  Kurtosis <- round(moments::kurtosis(x = temp$TEMP_mean,na.rm = TRUE),digits = 8)
  Skewnes <- round(moments::skewness(x = temp$TEMP_mean,na.rm = TRUE),digits = 8)
  
  Somme <- round(sum(temp$TEMP_mean,na.rm = TRUE),digits = 8)
  Variance <- round((StdDev)^2,digits = 8)
  
  Min.Date <- min(temp$Date,na.rm = TRUE)
  Max.Date <- max(temp$Date,na.rm = TRUE)
  
  temp_tabl_smry <- data.frame("Stats" = c("Count","Missing Values","Mean","Minimum","Maximum","Min.Date","Max.Date"), 
                               "Value" = c(count,Missing.values,Mean,Minimum,Maximum,Min.Date,Max.Date))
  
  temp_tabl_quant <- data.frame("Quantile Statistics" = c("Minimum","5th Percentile","Q1","Median","Q3","95th Percentile","Maximum"),
                                "Value" = c(Minimum,quantile5th,quantile25th,quantile50th,quantile75th,quantile95th,Maximum))
  
  temp_tabl_descr <- data.frame("Descriptive Statistics" = c("Standard Deviation","Coefficient of Variation (CV)","Kurtosis","Mean","Skewness","Sum","Variance"),
                                "Value" = c(StdDev,CV,Kurtosis,Mean,Skewnes,Somme,Variance))
  
  names(temp_tabl_descr) <- c("Descriptive Statistics","Value")
  names(temp_tabl_quant) <- c("Quantile Statistics","Value")
  
  temp_tabl_smry$Value[6] <- as.character(Min.Date)
  temp_tabl_smry$Value[7] <- as.character(Max.Date)
  
  output$oceano_var_smry <- renderTable(digits = 6,{
    temp_tabl_smry
  })
  
  output$oceano_var_plot <- renderPlot({
    ggplot(temp, aes(TEMP_mean, group = Zone, fill = Zone)) +
      geom_histogram(aes(y=..density..), alpha=0.5, position="identity",
                     na.rm = TRUE,bins = 100, color = "gray") + geom_density(alpha=.2) +
      theme_minimal(base_size = 20)+
      theme(legend.position="bottom",legend.title = element_blank()) + 
      xlab(label = "") + ylab("") 
  },height = 300)
  
  output$oceano_var_quantiles <- renderTable(digits = 6,{
    temp_tabl_quant
  },width = "100%")
  
  output$oceano_var_stats <- renderTable(digits = 6,{
    temp_tabl_descr
  },width = "100%")
  
  DBI::dbDisconnect(conn)
  shinyalert(title = "", immediate = TRUE,
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             timer = 100)
})

### SAL
observeEvent(input$sal_btn_ID,{
  shinyalert(title = "Loading", 
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             showCancelButton = F, 
             showConfirmButton = F, 
             size = "xs")
  output$title_variable_box_ID <- renderText({
    "PHYSICAL INDICATORS: SALINITY"
  })
  updateButton(session, "sst_btn_ID",style = "primary") 
  updateButton(session, "ekman_btn_ID",style = "primary") 
  updateButton(session, "nao_btn_ID",style = "primary") 
  updateButton(session, "o2_btn_ID",style = "primary") 
  updateButton(session, "chl_btn_ID",style = "primary") 
  updateButton(session, "pp_btn_ID",style = "primary") 
  updateButton(session, "temp_btn_ID",style = "primary") 
  updateButton(session, "sal_btn_ID",style = "success") 
  updateButton(session, "u_phys_btn_ID",style = "primary") 
  updateButton(session, "v_phys_btn_ID",style = "primary") 
  updateButton(session, "pres_btn_ID",style = "primary") 
  updateButton(session, "u_meteo_btn_ID",style = "primary") 
  updateButton(session, "v_meteo_btn_ID",style = "primary") 
  updateButton(session, "per_btn_ID",style = "primary") 
  updateButton(session, "dir_btn_ID",style = "primary") 
  updateButton(session, "hs_btn_ID",style = "primary") 
  
  conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
  sal_sud <- dbGetQuery(conn = conn,statement = "select Date, SAL_mean, SAL_std from oceano_physique_south_table") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = "Atlantique Sud")
  
  sal_med <- dbGetQuery(conn = conn,statement = "select Date, SAL_mean, SAL_std from oceano_physique_med_table where Latitude >= 35") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = "Méditerranée")
  
  sal <- sal_sud %>% bind_rows(sal_med)
  
  count <- nrow(sal)
  Missing.values <- sum(is.na(sal$SAL_mean))
  Mean <- round(mean(sal$SAL_mean,na.rm = TRUE),digits = 8)
  Minimum <- round(min(sal$SAL_mean,na.rm = TRUE),digits = 8)
  Maximum <- round(max(sal$SAL_mean,na.rm = TRUE),digits = 8)
  
  quantile5th <- round(quantile(sal$SAL_mean,probs = 0.05,na.rm = TRUE),digits = 8)
  quantile25th <- round(quantile(sal$SAL_mean,probs = 0.25,na.rm = TRUE),digits = 8)
  quantile50th <- round(quantile(sal$SAL_mean,probs = 0.50,na.rm = TRUE),digits = 8)
  quantile75th <- round(quantile(sal$SAL_mean,probs = 0.75,na.rm = TRUE),digits = 8)
  quantile95th <- round(quantile(sal$SAL_mean,probs = 0.95,na.rm = TRUE),digits = 8)
  
  StdDev <- round(mean(sal$SAL_std,na.rm = TRUE),digits = 8)
  CV <- round(StdDev/Mean,digits = TRUE)
  
  Kurtosis <- round(moments::kurtosis(x = sal$SAL_mean,na.rm = TRUE),digits = 8)
  Skewnes <- round(moments::skewness(x = sal$SAL_mean,na.rm = TRUE),digits = 8)
  
  Somme <- round(sum(sal$SAL_mean,na.rm = TRUE),digits = 8)
  Variance <- round((StdDev)^2,digits = 8)
  
  Min.Date <- min(sal$Date,na.rm = TRUE)
  Max.Date <- max(sal$Date,na.rm = TRUE)
  
  sal_tabl_smry <- data.frame("Stats" = c("Count","Missing Values","Mean","Minimum","Maximum","Min.Date","Max.Date"), 
                              "Value" = c(count,Missing.values,Mean,Minimum,Maximum,Min.Date,Max.Date))
  
  sal_tabl_quant <- data.frame("Quantile Statistics" = c("Minimum","5th Percentile","Q1","Median","Q3","95th Percentile","Maximum"),
                               "Value" = c(Minimum,quantile5th,quantile25th,quantile50th,quantile75th,quantile95th,Maximum))
  
  sal_tabl_descr <- data.frame("Descriptive Statistics" = c("Standard Deviation","Coefficient of Variation (CV)","Kurtosis","Mean","Skewness","Sum","Variance"),
                               "Value" = c(StdDev,CV,Kurtosis,Mean,Skewnes,Somme,Variance))
  
  names(sal_tabl_descr) <- c("Descriptive Statistics","Value")
  names(sal_tabl_quant) <- c("Quantile Statistics","Value")
  
  sal_tabl_smry$Value[6] <- as.character(Min.Date)
  sal_tabl_smry$Value[7] <- as.character(Max.Date)
  
  output$oceano_var_smry <- renderTable(digits = 6,{
    sal_tabl_smry
  })
  
  output$oceano_var_plot <- renderPlot({
    ggplot(sal, aes(SAL_mean, group = Zone, fill = Zone)) +
      geom_histogram(aes(y=..density..), alpha=0.5, position="identity",
                     na.rm = TRUE,bins = 100, color = "gray") + geom_density(alpha=.2) +
      theme_minimal(base_size = 20)+
      theme(legend.position="bottom",legend.title = element_blank()) + 
      xlab(label = "") + ylab("") 
  },height = 300)
  
  output$oceano_var_quantiles <- renderTable(digits = 6,{
    sal_tabl_quant
  },width = "100%")
  
  output$oceano_var_stats <- renderTable(digits = 6,{
    sal_tabl_descr
  },width = "100%")
  
  DBI::dbDisconnect(conn)
  shinyalert(title = "", immediate = TRUE,
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             timer = 100)
})

### U
observeEvent(input$u_phys_btn_ID,{
  shinyalert(title = "Loading", 
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             showCancelButton = F, 
             showConfirmButton = F, 
             size = "xs")
  output$title_variable_box_ID <- renderText({
    "PHYSICAL INDICATORS: U"
  })
  updateButton(session, "sst_btn_ID",style = "primary") 
  updateButton(session, "ekman_btn_ID",style = "primary") 
  updateButton(session, "nao_btn_ID",style = "primary") 
  updateButton(session, "o2_btn_ID",style = "primary") 
  updateButton(session, "chl_btn_ID",style = "primary") 
  updateButton(session, "pp_btn_ID",style = "primary") 
  updateButton(session, "temp_btn_ID",style = "primary") 
  updateButton(session, "sal_btn_ID",style = "primary") 
  updateButton(session, "u_phys_btn_ID",style = "success") 
  updateButton(session, "v_phys_btn_ID",style = "primary") 
  updateButton(session, "pres_btn_ID",style = "primary") 
  updateButton(session, "u_meteo_btn_ID",style = "primary") 
  updateButton(session, "v_meteo_btn_ID",style = "primary") 
  updateButton(session, "per_btn_ID",style = "primary") 
  updateButton(session, "dir_btn_ID",style = "primary") 
  updateButton(session, "hs_btn_ID",style = "primary") 
  
  conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
  u_sud <- dbGetQuery(conn = conn,statement = "select Date, U_mean, U_std from oceano_physique_south_table") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = "Atlantique Sud")
  
  u_med <- dbGetQuery(conn = conn,statement = "select Date, U_mean, U_std from oceano_physique_med_table where Latitude >= 35") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = "Méditerranée")
  
  u <- u_sud %>% bind_rows(u_med)
  
  count <- nrow(u)
  Missing.values <- sum(is.na(u$U_mean))
  Mean <- round(mean(u$U_mean,na.rm = TRUE),digits = 8)
  Minimum <- round(min(u$U_mean,na.rm = TRUE),digits = 8)
  Maximum <- round(max(u$U_mean,na.rm = TRUE),digits = 8)
  
  quantile5th <- round(quantile(u$U_mean,probs = 0.05,na.rm = TRUE),digits = 8)
  quantile25th <- round(quantile(u$U_mean,probs = 0.25,na.rm = TRUE),digits = 8)
  quantile50th <- round(quantile(u$U_mean,probs = 0.50,na.rm = TRUE),digits = 8)
  quantile75th <- round(quantile(u$U_mean,probs = 0.75,na.rm = TRUE),digits = 8)
  quantile95th <- round(quantile(u$U_mean,probs = 0.95,na.rm = TRUE),digits = 8)
  
  StdDev <- round(mean(u$U_std,na.rm = TRUE),digits = 8)
  CV <- round(StdDev/Mean,digits = TRUE)
  
  Kurtosis <- round(moments::kurtosis(x = u$U_mean,na.rm = TRUE),digits = 8)
  Skewnes <- round(moments::skewness(x = u$U_mean,na.rm = TRUE),digits = 8)
  
  Somme <- round(sum(u$U_mean,na.rm = TRUE),digits = 8)
  Variance <- round((StdDev)^2,digits = 8)
  
  Min.Date <- min(u$Date,na.rm = TRUE)
  Max.Date <- max(u$Date,na.rm = TRUE)
  
  u_tabl_smry <- data.frame("Stats" = c("Count","Missing Values","Mean","Minimum","Maximum","Min.Date","Max.Date"), 
                            "Value" = c(count,Missing.values,Mean,Minimum,Maximum,Min.Date,Max.Date))
  
  u_tabl_quant <- data.frame("Quantile Statistics" = c("Minimum","5th Percentile","Q1","Median","Q3","95th Percentile","Maximum"),
                             "Value" = c(Minimum,quantile5th,quantile25th,quantile50th,quantile75th,quantile95th,Maximum))
  
  u_tabl_descr <- data.frame("Descriptive Statistics" = c("Standard Deviation","Coefficient of Variation (CV)","Kurtosis","Mean","Skewness","Sum","Variance"),
                             "Value" = c(StdDev,CV,Kurtosis,Mean,Skewnes,Somme,Variance))
  
  names(u_tabl_descr) <- c("Descriptive Statistics","Value")
  names(u_tabl_quant) <- c("Quantile Statistics","Value")
  
  u_tabl_smry$Value[6] <- as.character(Min.Date)
  u_tabl_smry$Value[7] <- as.character(Max.Date)
  
  output$oceano_var_smry <- renderTable(digits = 6,{
    u_tabl_smry
  })
  
  output$oceano_var_plot <- renderPlot({
    ggplot(u, aes(U_mean, group = Zone, fill = Zone)) +
      geom_histogram(aes(y=..density..), alpha=0.5, position="identity",
                     na.rm = TRUE,bins = 100, color = "gray") + geom_density(alpha=.2) +
      theme_minimal(base_size = 20)+
      theme(legend.position="bottom",legend.title = element_blank()) + 
      xlab(label = "") + ylab("") 
  },height = 300)
  
  output$oceano_var_quantiles <- renderTable(digits = 6,{
    u_tabl_quant
  },width = "100%")
  
  output$oceano_var_stats <- renderTable(digits = 6,{
    u_tabl_descr
  },width = "100%")
  
  DBI::dbDisconnect(conn)
  shinyalert(title = "", immediate = TRUE,
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             timer = 100)
})

### V
observeEvent(input$v_phys_btn_ID,{
  shinyalert(title = "Loading", 
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             showCancelButton = F, 
             showConfirmButton = F, 
             size = "xs")
  output$title_variable_box_ID <- renderText({
    "PHYSICAL INDICATORS: V"
  })
  updateButton(session, "sst_btn_ID",style = "primary") 
  updateButton(session, "ekman_btn_ID",style = "primary") 
  updateButton(session, "nao_btn_ID",style = "primary") 
  updateButton(session, "o2_btn_ID",style = "primary") 
  updateButton(session, "chl_btn_ID",style = "primary") 
  updateButton(session, "pp_btn_ID",style = "primary") 
  updateButton(session, "temp_btn_ID",style = "primary") 
  updateButton(session, "sal_btn_ID",style = "primary") 
  updateButton(session, "u_phys_btn_ID",style = "primary") 
  updateButton(session, "v_phys_btn_ID",style = "success") 
  updateButton(session, "pres_btn_ID",style = "primary") 
  updateButton(session, "u_meteo_btn_ID",style = "primary") 
  updateButton(session, "v_meteo_btn_ID",style = "primary") 
  updateButton(session, "per_btn_ID",style = "primary") 
  updateButton(session, "dir_btn_ID",style = "primary") 
  updateButton(session, "hs_btn_ID",style = "primary") 
  
  conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
  v_sud <- dbGetQuery(conn = conn,statement = "select Date, V_mean, V_std from oceano_physique_south_table") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = "Atlantique Sud")
  
  v_med <- dbGetQuery(conn = conn,statement = "select Date, V_mean, V_std from oceano_physique_med_table where Latitude >= 35") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = "Méditerranée")
  
  v <- v_sud %>% bind_rows(v_med)
  
  count <- nrow(v)
  Missing.values <- sum(is.na(v$V_mean))
  Mean <- round(mean(v$V_mean,na.rm = TRUE),digits = 8)
  Minimum <- round(min(v$V_mean,na.rm = TRUE),digits = 8)
  Maximum <- round(max(v$V_mean,na.rm = TRUE),digits = 8)
  
  quantile5th <- round(quantile(v$V_mean,probs = 0.05,na.rm = TRUE),digits = 8)
  quantile25th <- round(quantile(v$V_mean,probs = 0.25,na.rm = TRUE),digits = 8)
  quantile50th <- round(quantile(v$V_mean,probs = 0.50,na.rm = TRUE),digits = 8)
  quantile75th <- round(quantile(v$V_mean,probs = 0.75,na.rm = TRUE),digits = 8)
  quantile95th <- round(quantile(v$V_mean,probs = 0.95,na.rm = TRUE),digits = 8)
  
  StdDev <- round(mean(v$V_std,na.rm = TRUE),digits = 8)
  CV <- round(StdDev/Mean,digits = TRUE)
  
  Kurtosis <- round(moments::kurtosis(x = v$V_mean,na.rm = TRUE),digits = 8)
  Skewnes <- round(moments::skewness(x = v$V_mean,na.rm = TRUE),digits = 8)
  
  Somme <- round(sum(v$V_mean,na.rm = TRUE),digits = 8)
  Variance <- round((StdDev)^2,digits = 8)
  
  Min.Date <- min(v$Date,na.rm = TRUE)
  Max.Date <- max(v$Date,na.rm = TRUE)
  
  v_tabl_smry <- data.frame("Stats" = c("Count","Missing Values","Mean","Minimum","Maximum","Min.Date","Max.Date"), 
                            "Value" = c(count,Missing.values,Mean,Minimum,Maximum,Min.Date,Max.Date))
  
  v_tabl_quant <- data.frame("Quantile Statistics" = c("Minimum","5th Percentile","Q1","Median","Q3","95th Percentile","Maximum"),
                             "Value" = c(Minimum,quantile5th,quantile25th,quantile50th,quantile75th,quantile95th,Maximum))
  
  v_tabl_descr <- data.frame("Descriptive Statistics" = c("Standard Deviation","Coefficient of Variation (CV)","Kurtosis","Mean","Skewness","Sum","Variance"),
                             "Value" = c(StdDev,CV,Kurtosis,Mean,Skewnes,Somme,Variance))
  
  names(v_tabl_descr) <- c("Descriptive Statistics","Value")
  names(v_tabl_quant) <- c("Quantile Statistics","Value")
  
  v_tabl_smry$Value[6] <- as.character(Min.Date)
  v_tabl_smry$Value[7] <- as.character(Max.Date)
  
  output$oceano_var_smry <- renderTable(digits = 6,{
    v_tabl_smry
  })
  
  output$oceano_var_plot <- renderPlot({
    ggplot(v, aes(V_mean, group = Zone, fill = Zone)) +
      geom_histogram(aes(y=..density..), alpha=0.5, position="identity",
                     na.rm = TRUE,bins = 100, color = "gray") + geom_density(alpha=.2) +
      theme_minimal(base_size = 20)+
      theme(legend.position="bottom",legend.title = element_blank()) + 
      xlab(label = "") + ylab("") 
  },height = 300)
  
  output$oceano_var_quantiles <- renderTable(digits = 6,{
    v_tabl_quant
  },width = "100%")
  
  output$oceano_var_stats <- renderTable(digits = 6,{
    v_tabl_descr
  },width = "100%")
  
  DBI::dbDisconnect(conn)
  shinyalert(title = "", immediate = TRUE,
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             timer = 100)
})

### Pres
observeEvent(input$pres_btn_ID,{
  shinyalert(title = "Loading", 
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             showCancelButton = F, 
             showConfirmButton = F, 
             size = "xs")
  output$title_variable_box_ID <- renderText({
    "METEOROLOGICAL INDICATORS: ATMOSPHERIC PRESSURE"
  })
  updateButton(session, "sst_btn_ID",style = "primary") 
  updateButton(session, "ekman_btn_ID",style = "primary") 
  updateButton(session, "nao_btn_ID",style = "primary") 
  updateButton(session, "o2_btn_ID",style = "primary") 
  updateButton(session, "chl_btn_ID",style = "primary") 
  updateButton(session, "pp_btn_ID",style = "primary") 
  updateButton(session, "temp_btn_ID",style = "primary") 
  updateButton(session, "sal_btn_ID",style = "primary") 
  updateButton(session, "u_phys_btn_ID",style = "primary") 
  updateButton(session, "v_phys_btn_ID",style = "primary") 
  updateButton(session, "pres_btn_ID",style = "success") 
  updateButton(session, "u_meteo_btn_ID",style = "primary") 
  updateButton(session, "v_meteo_btn_ID",style = "primary") 
  updateButton(session, "per_btn_ID",style = "primary") 
  updateButton(session, "dir_btn_ID",style = "primary") 
  updateButton(session, "hs_btn_ID",style = "primary") 
  
  conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
  pres_sud <- dbGetQuery(conn = conn,statement = "select Date, Pres_mean, Pres_std from oceano_meteo_south_table") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = "Atlantique Sud")
  
  pres_med <- dbGetQuery(conn = conn,statement = "select Date, Pres_mean, Pres_std from oceano_meteo_med_table where Latitude >= 35") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = "Méditerranée")
  
  pres <- pres_sud %>% bind_rows(pres_med)
  
  count <- nrow(pres)
  Missing.values <- sum(is.na(pres$Pres_mean))
  Mean <- round(mean(pres$Pres_mean,na.rm = TRUE),digits = 8)
  Minimum <- round(min(pres$Pres_mean,na.rm = TRUE),digits = 8)
  Maximum <- round(max(pres$Pres_mean,na.rm = TRUE),digits = 8)
  
  quantile5th <- round(quantile(pres$Pres_mean,probs = 0.05,na.rm = TRUE),digits = 8)
  quantile25th <- round(quantile(pres$Pres_mean,probs = 0.25,na.rm = TRUE),digits = 8)
  quantile50th <- round(quantile(pres$Pres_mean,probs = 0.50,na.rm = TRUE),digits = 8)
  quantile75th <- round(quantile(pres$Pres_mean,probs = 0.75,na.rm = TRUE),digits = 8)
  quantile95th <- round(quantile(pres$Pres_mean,probs = 0.95,na.rm = TRUE),digits = 8)
  
  StdDev <- round(mean(pres$Pres_std,na.rm = TRUE),digits = 8)
  CV <- round(StdDev/Mean,digits = TRUE)
  
  Kurtosis <- round(moments::kurtosis(x = pres$Pres_mean,na.rm = TRUE),digits = 8)
  Skewnes <- round(moments::skewness(x = pres$Pres_mean,na.rm = TRUE),digits = 8)
  
  Somme <- round(sum(pres$Pres_mean,na.rm = TRUE),digits = 8)
  Variance <- round((StdDev)^2,digits = 8)
  
  Min.Date <- min(pres$Date,na.rm = TRUE)
  Max.Date <- max(pres$Date,na.rm = TRUE)
  
  pres_tabl_smry <- data.frame("Stats" = c("Count","Missing Values","Mean","Minimum","Maximum","Min.Date","Max.Date"), 
                               "Value" = c(count,Missing.values,Mean,Minimum,Maximum,Min.Date,Max.Date))
  
  pres_tabl_quant <- data.frame("Quantile Statistics" = c("Minimum","5th Percentile","Q1","Median","Q3","95th Percentile","Maximum"),
                                "Value" = c(Minimum,quantile5th,quantile25th,quantile50th,quantile75th,quantile95th,Maximum))
  
  pres_tabl_descr <- data.frame("Descriptive Statistics" = c("Standard Deviation","Coefficient of Variation (CV)","Kurtosis","Mean","Skewness","Sum","Variance"),
                                "Value" = c(StdDev,CV,Kurtosis,Mean,Skewnes,Somme,Variance))
  
  names(pres_tabl_descr) <- c("Descriptive Statistics","Value")
  names(pres_tabl_quant) <- c("Quantile Statistics","Value")
  
  pres_tabl_smry$Value[6] <- as.character(Min.Date)
  pres_tabl_smry$Value[7] <- as.character(Max.Date)
  
  output$oceano_var_smry <- renderTable(digits = 6,{
    pres_tabl_smry
  })
  
  output$oceano_var_plot <- renderPlot({
    ggplot(pres, aes(Pres_mean, group = Zone, fill = Zone)) +
      geom_histogram(aes(y=..density..), alpha=0.5, position="identity",
                     na.rm = TRUE,bins = 100, color = "gray") + geom_density(alpha=.2) +
      theme_minimal(base_size = 20)+
      theme(legend.position="bottom",legend.title = element_blank()) + 
      xlab(label = "") + ylab("") 
  },height = 300)
  
  output$oceano_var_quantiles <- renderTable(digits = 6,{
    pres_tabl_quant
  },width = "100%")
  
  output$oceano_var_stats <- renderTable(digits = 6,{
    pres_tabl_descr
  },width = "100%")
  
  DBI::dbDisconnect(conn)
  shinyalert(title = "", immediate = TRUE,
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             timer = 100)
})

### U
observeEvent(input$u_meteo_btn_ID,{
  shinyalert(title = "Loading", 
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             showCancelButton = F, 
             showConfirmButton = F, 
             size = "xs")
  output$title_variable_box_ID <- renderText({
    "METEOROLOGICAL INDICATORS: U"
  })
  updateButton(session, "sst_btn_ID",style = "primary") 
  updateButton(session, "ekman_btn_ID",style = "primary") 
  updateButton(session, "nao_btn_ID",style = "primary") 
  updateButton(session, "o2_btn_ID",style = "primary") 
  updateButton(session, "chl_btn_ID",style = "primary") 
  updateButton(session, "pp_btn_ID",style = "primary") 
  updateButton(session, "temp_btn_ID",style = "primary") 
  updateButton(session, "sal_btn_ID",style = "primary") 
  updateButton(session, "u_phys_btn_ID",style = "primary") 
  updateButton(session, "v_phys_btn_ID",style = "primary") 
  updateButton(session, "pres_btn_ID",style = "primary") 
  updateButton(session, "u_meteo_btn_ID",style = "success") 
  updateButton(session, "v_meteo_btn_ID",style = "primary") 
  updateButton(session, "per_btn_ID",style = "primary") 
  updateButton(session, "dir_btn_ID",style = "primary") 
  updateButton(session, "hs_btn_ID",style = "primary") 
  
  conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
  u_sud <- dbGetQuery(conn = conn,statement = "select Date, U_mean, U_std from oceano_meteo_south_table") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = "Atlantique Sud")
  
  u_med <- dbGetQuery(conn = conn,statement = "select Date, U_mean, U_std from oceano_meteo_med_table where Latitude >= 35") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = "Méditerranée")
  
  u <- u_sud %>% bind_rows(u_med)
  
  count <- nrow(u)
  Missing.values <- sum(is.na(u$U_mean))
  Mean <- round(mean(u$U_mean,na.rm = TRUE),digits = 8)
  Minimum <- round(min(u$U_mean,na.rm = TRUE),digits = 8)
  Maximum <- round(max(u$U_mean,na.rm = TRUE),digits = 8)
  
  quantile5th <- round(quantile(u$U_mean,probs = 0.05,na.rm = TRUE),digits = 8)
  quantile25th <- round(quantile(u$U_mean,probs = 0.25,na.rm = TRUE),digits = 8)
  quantile50th <- round(quantile(u$U_mean,probs = 0.50,na.rm = TRUE),digits = 8)
  quantile75th <- round(quantile(u$U_mean,probs = 0.75,na.rm = TRUE),digits = 8)
  quantile95th <- round(quantile(u$U_mean,probs = 0.95,na.rm = TRUE),digits = 8)
  
  StdDev <- round(mean(u$U_std,na.rm = TRUE),digits = 8)
  CV <- round(StdDev/Mean,digits = TRUE)
  
  Kurtosis <- round(moments::kurtosis(x = u$U_mean,na.rm = TRUE),digits = 8)
  Skewnes <- round(moments::skewness(x = u$U_mean,na.rm = TRUE),digits = 8)
  
  Somme <- round(sum(u$U_mean,na.rm = TRUE),digits = 8)
  Variance <- round((StdDev)^2,digits = 8)
  
  Min.Date <- min(u$Date,na.rm = TRUE)
  Max.Date <- max(u$Date,na.rm = TRUE)
  
  u_tabl_smry <- data.frame("Stats" = c("Count","Missing Values","Mean","Minimum","Maximum","Min.Date","Max.Date"), 
                            "Value" = c(count,Missing.values,Mean,Minimum,Maximum,Min.Date,Max.Date))
  
  u_tabl_quant <- data.frame("Quantile Statistics" = c("Minimum","5th Percentile","Q1","Median","Q3","95th Percentile","Maximum"),
                             "Value" = c(Minimum,quantile5th,quantile25th,quantile50th,quantile75th,quantile95th,Maximum))
  
  u_tabl_descr <- data.frame("Descriptive Statistics" = c("Standard Deviation","Coefficient of Variation (CV)","Kurtosis","Mean","Skewness","Sum","Variance"),
                             "Value" = c(StdDev,CV,Kurtosis,Mean,Skewnes,Somme,Variance))
  
  names(u_tabl_descr) <- c("Descriptive Statistics","Value")
  names(u_tabl_quant) <- c("Quantile Statistics","Value")
  
  u_tabl_smry$Value[6] <- as.character(Min.Date)
  u_tabl_smry$Value[7] <- as.character(Max.Date)
  
  output$oceano_var_smry <- renderTable(digits = 6,{
    u_tabl_smry
  })
  
  output$oceano_var_plot <- renderPlot({
    ggplot(u, aes(U_mean, group = Zone, fill = Zone)) +
      geom_histogram(aes(y=..density..), alpha=0.5, position="identity",
                     na.rm = TRUE,bins = 100, color = "gray") + geom_density(alpha=.2) +
      theme_minimal(base_size = 20) + 
      theme(legend.position="bottom",legend.title = element_blank()) + 
      xlab(label = "") + ylab("") 
  },height = 300)
  
  output$oceano_var_quantiles <- renderTable(digits = 6,{
    u_tabl_quant
  },width = "100%")
  
  output$oceano_var_stats <- renderTable(digits = 6,{
    u_tabl_descr
  },width = "100%")
  
  DBI::dbDisconnect(conn)
  shinyalert(title = "", immediate = TRUE,
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             timer = 100)
})

### V
observeEvent(input$v_meteo_btn_ID,{
  shinyalert(title = "Loading", 
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             showCancelButton = F, 
             showConfirmButton = F, 
             size = "xs")
  output$title_variable_box_ID <- renderText({
    "METEOROLOGICAL INDICATORS: V"
  })
  updateButton(session, "sst_btn_ID",style = "primary") 
  updateButton(session, "ekman_btn_ID",style = "primary") 
  updateButton(session, "nao_btn_ID",style = "primary") 
  updateButton(session, "o2_btn_ID",style = "primary") 
  updateButton(session, "chl_btn_ID",style = "primary") 
  updateButton(session, "pp_btn_ID",style = "primary") 
  updateButton(session, "temp_btn_ID",style = "primary") 
  updateButton(session, "sal_btn_ID",style = "primary") 
  updateButton(session, "u_phys_btn_ID",style = "primary") 
  updateButton(session, "v_phys_btn_ID",style = "primary") 
  updateButton(session, "pres_btn_ID",style = "primary") 
  updateButton(session, "u_meteo_btn_ID",style = "primary") 
  updateButton(session, "v_meteo_btn_ID",style = "success") 
  updateButton(session, "per_btn_ID",style = "primary") 
  updateButton(session, "dir_btn_ID",style = "primary") 
  updateButton(session, "hs_btn_ID",style = "primary") 
  
  conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
  v_sud <- dbGetQuery(conn = conn,statement = "select Date, V_mean, V_std from oceano_meteo_south_table") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = "Atlantique Sud")
  
  v_med <- dbGetQuery(conn = conn,statement = "select Date, V_mean, V_std from oceano_meteo_med_table where Latitude >= 35") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = "Méditerranée")
  
  v <- v_sud %>% bind_rows(v_med)
  
  count <- nrow(v)
  Missing.values <- sum(is.na(v$V_mean))
  Mean <- round(mean(v$V_mean,na.rm = TRUE),digits = 8)
  Minimum <- round(min(v$V_mean,na.rm = TRUE),digits = 8)
  Maximum <- round(max(v$V_mean,na.rm = TRUE),digits = 8)
  
  quantile5th <- round(quantile(v$V_mean,probs = 0.05,na.rm = TRUE),digits = 8)
  quantile25th <- round(quantile(v$V_mean,probs = 0.25,na.rm = TRUE),digits = 8)
  quantile50th <- round(quantile(v$V_mean,probs = 0.50,na.rm = TRUE),digits = 8)
  quantile75th <- round(quantile(v$V_mean,probs = 0.75,na.rm = TRUE),digits = 8)
  quantile95th <- round(quantile(v$V_mean,probs = 0.95,na.rm = TRUE),digits = 8)
  
  StdDev <- round(mean(v$V_std,na.rm = TRUE),digits = 8)
  CV <- round(StdDev/Mean,digits = TRUE)
  
  Kurtosis <- round(moments::kurtosis(x = v$V_mean,na.rm = TRUE),digits = 8)
  Skewnes <- round(moments::skewness(x = v$V_mean,na.rm = TRUE),digits = 8)
  
  Somme <- round(sum(v$V_mean,na.rm = TRUE),digits = 8)
  Variance <- round((StdDev)^2,digits = 8)
  
  Min.Date <- min(v$Date,na.rm = TRUE)
  Max.Date <- max(v$Date,na.rm = TRUE)
  
  v_tabl_smry <- data.frame("Stats" = c("Count","Missing Values","Mean","Minimum","Maximum","Min.Date","Max.Date"), 
                            "Value" = c(count,Missing.values,Mean,Minimum,Maximum,Min.Date,Max.Date))
  
  v_tabl_quant <- data.frame("Quantile Statistics" = c("Minimum","5th Percentile","Q1","Median","Q3","95th Percentile","Maximum"),
                             "Value" = c(Minimum,quantile5th,quantile25th,quantile50th,quantile75th,quantile95th,Maximum))
  
  v_tabl_descr <- data.frame("Descriptive Statistics" = c("Standard Deviation","Coefficient of Variation (CV)","Kurtosis","Mean","Skewness","Sum","Variance"),
                             "Value" = c(StdDev,CV,Kurtosis,Mean,Skewnes,Somme,Variance))
  
  names(v_tabl_descr) <- c("Descriptive Statistics","Value")
  names(v_tabl_quant) <- c("Quantile Statistics","Value")
  
  v_tabl_smry$Value[6] <- as.character(Min.Date)
  v_tabl_smry$Value[7] <- as.character(Max.Date)
  
  output$oceano_var_smry <- renderTable(digits = 6,{
    v_tabl_smry
  })
  
  output$oceano_var_plot <- renderPlot({
    ggplot(v, aes(V_mean, group = Zone, fill = Zone)) +
      geom_histogram(aes(y=..density..), alpha=0.5, position="identity",
                     na.rm = TRUE,bins = 100, color = "gray") + geom_density(alpha=.2) +
      theme_minimal(base_size = 20)+
      theme(legend.position="bottom",legend.title = element_blank()) + 
      xlab(label = "") + ylab("") 
  },height = 300)
  
  output$oceano_var_quantiles <- renderTable(digits = 6,{
    v_tabl_quant
  },width = "100%")
  
  output$oceano_var_stats <- renderTable(digits = 6,{
    v_tabl_descr
  },width = "100%")
  
  DBI::dbDisconnect(conn)
  shinyalert(title = "", immediate = TRUE,
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             timer = 100)
})

### Per
observeEvent(input$per_btn_ID,{
  shinyalert(title = "Loading", 
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             showCancelButton = F, 
             showConfirmButton = F, 
             size = "xs")
  output$title_variable_box_ID <- renderText({
    "METEOROLOGICAL INDICATORS: PER"
  })
  updateButton(session, "sst_btn_ID",style = "primary") 
  updateButton(session, "ekman_btn_ID",style = "primary") 
  updateButton(session, "nao_btn_ID",style = "primary") 
  updateButton(session, "o2_btn_ID",style = "primary") 
  updateButton(session, "chl_btn_ID",style = "primary") 
  updateButton(session, "pp_btn_ID",style = "primary") 
  updateButton(session, "temp_btn_ID",style = "primary") 
  updateButton(session, "sal_btn_ID",style = "primary") 
  updateButton(session, "u_phys_btn_ID",style = "primary") 
  updateButton(session, "v_phys_btn_ID",style = "primary") 
  updateButton(session, "pres_btn_ID",style = "primary") 
  updateButton(session, "u_meteo_btn_ID",style = "primary") 
  updateButton(session, "v_meteo_btn_ID",style = "primary") 
  updateButton(session, "per_btn_ID",style = "success") 
  updateButton(session, "dir_btn_ID",style = "primary") 
  updateButton(session, "hs_btn_ID",style = "primary") 
  
  conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
  per_sud <- dbGetQuery(conn = conn,statement = "select Date, Per_mean, Per_std from oceano_meteo_south_table") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = "Atlantique Sud")
  
  per_med <- dbGetQuery(conn = conn,statement = "select Date, Per_mean, Per_std from oceano_meteo_med_table where Latitude >= 35") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = "Méditerranée")
  
  per <- per_sud %>% bind_rows(per_med)
  
  count <- nrow(per)
  Missing.values <- sum(is.na(per$Per_mean))
  Mean <- round(mean(per$Per_mean,na.rm = TRUE),digits = 8)
  Minimum <- round(min(per$Per_mean,na.rm = TRUE),digits = 8)
  Maximum <- round(max(per$Per_mean,na.rm = TRUE),digits = 8)
  
  quantile5th <- round(quantile(per$Per_mean,probs = 0.05,na.rm = TRUE),digits = 8)
  quantile25th <- round(quantile(per$Per_mean,probs = 0.25,na.rm = TRUE),digits = 8)
  quantile50th <- round(quantile(per$Per_mean,probs = 0.50,na.rm = TRUE),digits = 8)
  quantile75th <- round(quantile(per$Per_mean,probs = 0.75,na.rm = TRUE),digits = 8)
  quantile95th <- round(quantile(per$Per_mean,probs = 0.95,na.rm = TRUE),digits = 8)
  
  StdDev <- round(mean(per$Per_std,na.rm = TRUE),digits = 8)
  CV <- round(StdDev/Mean,digits = TRUE)
  
  Kurtosis <- round(moments::kurtosis(x = per$Per_mean,na.rm = TRUE),digits = 8)
  Skewnes <- round(moments::skewness(x = per$Per_mean,na.rm = TRUE),digits = 8)
  
  Somme <- round(sum(per$Per_mean,na.rm = TRUE),digits = 8)
  Variance <- round((StdDev)^2,digits = 8)
  
  Min.Date <- min(per$Date,na.rm = TRUE)
  Max.Date <- max(per$Date,na.rm = TRUE)
  
  per_tabl_smry <- data.frame("Stats" = c("Count","Missing Values","Mean","Minimum","Maximum","Min.Date","Max.Date"), 
                              "Value" = c(count,Missing.values,Mean,Minimum,Maximum,Min.Date,Max.Date))
  
  per_tabl_quant <- data.frame("Quantile Statistics" = c("Minimum","5th Percentile","Q1","Median","Q3","95th Percentile","Maximum"),
                               "Value" = c(Minimum,quantile5th,quantile25th,quantile50th,quantile75th,quantile95th,Maximum))
  
  per_tabl_descr <- data.frame("Descriptive Statistics" = c("Standard Deviation","Coefficient of Variation (CV)","Kurtosis","Mean","Skewness","Sum","Variance"),
                               "Value" = c(StdDev,CV,Kurtosis,Mean,Skewnes,Somme,Variance))
  
  names(per_tabl_descr) <- c("Descriptive Statistics","Value")
  names(per_tabl_quant) <- c("Quantile Statistics","Value")
  
  per_tabl_smry$Value[6] <- as.character(Min.Date)
  per_tabl_smry$Value[7] <- as.character(Max.Date)
  
  output$oceano_var_smry <- renderTable(digits = 6,{
    per_tabl_smry
  })
  
  output$oceano_var_plot <- renderPlot({
    ggplot(per, aes(Per_mean, group = Zone, fill = Zone)) +
      geom_histogram(aes(y=..density..), alpha=0.5, position="identity",
                     na.rm = TRUE,bins = 100, color = "gray") + geom_density(alpha=.2) +
      theme_minimal(base_size = 20)+
      theme(legend.position="bottom",legend.title = element_blank()) + 
      xlab(label = "") + ylab("") 
  },height = 300)
  
  output$oceano_var_quantiles <- renderTable(digits = 6,{
    per_tabl_quant
  },width = "100%")
  
  output$oceano_var_stats <- renderTable(digits = 6,{
    per_tabl_descr
  },width = "100%")
  
  DBI::dbDisconnect(conn)
  shinyalert(title = "", immediate = TRUE,
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             timer = 100)
})

### Dir
observeEvent(input$dir_btn_ID,{
  shinyalert(title = "Loading", 
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             showCancelButton = F, 
             showConfirmButton = F, 
             size = "xs")
  output$title_variable_box_ID <- renderText({
    "METEOROLOGICAL INDICATORS: DIR"
  })
  updateButton(session, "sst_btn_ID",style = "primary") 
  updateButton(session, "ekman_btn_ID",style = "primary") 
  updateButton(session, "nao_btn_ID",style = "primary") 
  updateButton(session, "o2_btn_ID",style = "primary") 
  updateButton(session, "chl_btn_ID",style = "primary") 
  updateButton(session, "pp_btn_ID",style = "primary") 
  updateButton(session, "temp_btn_ID",style = "primary") 
  updateButton(session, "sal_btn_ID",style = "primary") 
  updateButton(session, "u_phys_btn_ID",style = "primary") 
  updateButton(session, "v_phys_btn_ID",style = "primary") 
  updateButton(session, "pres_btn_ID",style = "primary") 
  updateButton(session, "u_meteo_btn_ID",style = "primary") 
  updateButton(session, "v_meteo_btn_ID",style = "primary") 
  updateButton(session, "per_btn_ID",style = "primary") 
  updateButton(session, "dir_btn_ID",style = "success") 
  updateButton(session, "hs_btn_ID",style = "primary") 
  
  conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
  dir_sud <- dbGetQuery(conn = conn,statement = "select Date, Dir_mean, Dir_std from oceano_meteo_south_table") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = "Atlantique Sud")
  
  dir_med <- dbGetQuery(conn = conn,statement = "select Date, Dir_mean, Dir_std from oceano_meteo_med_table where Latitude >= 35") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = "Méditerranée")
  
  dir <- dir_sud %>% bind_rows(dir_med)
  
  count <- nrow(dir)
  Missing.values <- sum(is.na(dir$Dir_mean))
  Mean <- round(mean(dir$Dir_mean,na.rm = TRUE),digits = 8)
  Minimum <- round(min(dir$Dir_mean,na.rm = TRUE),digits = 8)
  Maximum <- round(max(dir$Dir_mean,na.rm = TRUE),digits = 8)
  
  quantile5th <- round(quantile(dir$Dir_mean,probs = 0.05,na.rm = TRUE),digits = 8)
  quantile25th <- round(quantile(dir$Dir_mean,probs = 0.25,na.rm = TRUE),digits = 8)
  quantile50th <- round(quantile(dir$Dir_mean,probs = 0.50,na.rm = TRUE),digits = 8)
  quantile75th <- round(quantile(dir$Dir_mean,probs = 0.75,na.rm = TRUE),digits = 8)
  quantile95th <- round(quantile(dir$Dir_mean,probs = 0.95,na.rm = TRUE),digits = 8)
  
  StdDev <- round(mean(dir$Dir_std,na.rm = TRUE),digits = 8)
  CV <- round(StdDev/Mean,digits = TRUE)
  
  Kurtosis <- round(moments::kurtosis(x = dir$Dir_mean,na.rm = TRUE),digits = 8)
  Skewnes <- round(moments::skewness(x = dir$Dir_mean,na.rm = TRUE),digits = 8)
  
  Somme <- round(sum(dir$Dir_mean,na.rm = TRUE),digits = 8)
  Variance <- round((StdDev)^2,digits = 8)
  
  Min.Date <- min(dir$Date,na.rm = TRUE)
  Max.Date <- max(dir$Date,na.rm = TRUE)
  
  dir_tabl_smry <- data.frame("Stats" = c("Count","Missing Values","Mean","Minimum","Maximum","Min.Date","Max.Date"), 
                              "Value" = c(count,Missing.values,Mean,Minimum,Maximum,Min.Date,Max.Date))
  
  dir_tabl_quant <- data.frame("Quantile Statistics" = c("Minimum","5th Percentile","Q1","Median","Q3","95th Percentile","Maximum"),
                               "Value" = c(Minimum,quantile5th,quantile25th,quantile50th,quantile75th,quantile95th,Maximum))
  
  dir_tabl_descr <- data.frame("Descriptive Statistics" = c("Standard Deviation","Coefficient of Variation (CV)","Kurtosis","Mean","Skewness","Sum","Variance"),
                               "Value" = c(StdDev,CV,Kurtosis,Mean,Skewnes,Somme,Variance))
  
  names(dir_tabl_descr) <- c("Descriptive Statistics","Value")
  names(dir_tabl_quant) <- c("Quantile Statistics","Value")
  
  dir_tabl_smry$Value[6] <- as.character(Min.Date)
  dir_tabl_smry$Value[7] <- as.character(Max.Date)
  
  output$oceano_var_smry <- renderTable(digits = 6,{
    dir_tabl_smry
  })
  
  output$oceano_var_plot <- renderPlot({
    ggplot(dir, aes(Dir_mean, group = Zone, fill = Zone)) +
      geom_histogram(aes(y=..density..), alpha=0.5, position="identity",
                     na.rm = TRUE,bins = 100, color = "gray") + geom_density(alpha=.2) +
      theme_minimal(base_size = 20)+
      theme(legend.position="bottom",legend.title = element_blank()) + 
      xlab(label = "") + ylab("") 
  },height = 300)
  
  output$oceano_var_quantiles <- renderTable(digits = 6,{
    dir_tabl_quant
  },width = "100%")
  
  output$oceano_var_stats <- renderTable(digits = 6,{
    dir_tabl_descr
  },width = "100%")
  
  DBI::dbDisconnect(conn)
  shinyalert(title = "", immediate = TRUE,
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             timer = 100)
})

### Hs
observeEvent(input$hs_btn_ID,{
  shinyalert(title = "Loading", 
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             showCancelButton = F, 
             showConfirmButton = F, 
             size = "xs")
  output$title_variable_box_ID <- renderText({
    "METEOROLOGICAL INDICATORS: HS"
  })
  updateButton(session, "sst_btn_ID",style = "primary") 
  updateButton(session, "ekman_btn_ID",style = "primary") 
  updateButton(session, "nao_btn_ID",style = "primary") 
  updateButton(session, "o2_btn_ID",style = "primary") 
  updateButton(session, "chl_btn_ID",style = "primary") 
  updateButton(session, "pp_btn_ID",style = "primary") 
  updateButton(session, "temp_btn_ID",style = "primary") 
  updateButton(session, "sal_btn_ID",style = "primary") 
  updateButton(session, "u_phys_btn_ID",style = "primary") 
  updateButton(session, "v_phys_btn_ID",style = "primary") 
  updateButton(session, "pres_btn_ID",style = "primary") 
  updateButton(session, "u_meteo_btn_ID",style = "primary") 
  updateButton(session, "v_meteo_btn_ID",style = "primary") 
  updateButton(session, "per_btn_ID",style = "primary") 
  updateButton(session, "dir_btn_ID",style = "primary") 
  updateButton(session, "hs_btn_ID",style = "success") 
  
  conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
  hs_sud <- dbGetQuery(conn = conn,statement = "select Date, Hs_mean, Hs_std from oceano_meteo_south_table") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = "Atlantique Sud")
  
  hs_med <- dbGetQuery(conn = conn,statement = "select Date, Hs_mean, Hs_std from oceano_meteo_med_table where Latitude >= 35") %>%
    mutate(Date = fromChronToDate(Date)) %>% 
    tidyr::separate(Date,into = c("Date","Time"),sep = " ") %>% dplyr::select(-Time) %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    mutate(Zone = "Méditerranée")
  
  hs <- hs_sud %>% bind_rows(hs_med)
  
  count <- nrow(hs)
  Missing.values <- sum(is.na(hs$Hs_mean))
  Mean <- round(mean(hs$Hs_mean,na.rm = TRUE),digits = 8)
  Minimum <- round(min(hs$Hs_mean,na.rm = TRUE),digits = 8)
  Maximum <- round(max(hs$Hs_mean,na.rm = TRUE),digits = 8)
  
  quantile5th <- round(quantile(hs$Hs_mean,probs = 0.05,na.rm = TRUE),digits = 8)
  quantile25th <- round(quantile(hs$Hs_mean,probs = 0.25,na.rm = TRUE),digits = 8)
  quantile50th <- round(quantile(hs$Hs_mean,probs = 0.50,na.rm = TRUE),digits = 8)
  quantile75th <- round(quantile(hs$Hs_mean,probs = 0.75,na.rm = TRUE),digits = 8)
  quantile95th <- round(quantile(hs$Hs_mean,probs = 0.95,na.rm = TRUE),digits = 8)
  
  StdDev <- round(mean(hs$Hs_std,na.rm = TRUE),digits = 8)
  CV <- round(StdDev/Mean,digits = TRUE)
  
  Kurtosis <- round(moments::kurtosis(x = hs$Hs_mean,na.rm = TRUE),digits = 8)
  Skewnes <- round(moments::skewness(x = hs$Hs_mean,na.rm = TRUE),digits = 8)
  
  Somme <- round(sum(hs$Hs_mean,na.rm = TRUE),digits = 8)
  Variance <- round((StdDev)^2,digits = 8)
  
  Min.Date <- min(hs$Date,na.rm = TRUE)
  Max.Date <- max(hs$Date,na.rm = TRUE)
  
  hs_tabl_smry <- data.frame("Stats" = c("Count","Missing Values","Mean","Minimum","Maximum","Min.Date","Max.Date"), 
                             "Value" = c(count,Missing.values,Mean,Minimum,Maximum,Min.Date,Max.Date))
  
  hs_tabl_quant <- data.frame("Quantile Statistics" = c("Minimum","5th Percentile","Q1","Median","Q3","95th Percentile","Maximum"),
                              "Value" = c(Minimum,quantile5th,quantile25th,quantile50th,quantile75th,quantile95th,Maximum))
  
  hs_tabl_descr <- data.frame("Descriptive Statistics" = c("Standard Deviation","Coefficient of Variation (CV)","Kurtosis","Mean","Skewness","Sum","Variance"),
                              "Value" = c(StdDev,CV,Kurtosis,Mean,Skewnes,Somme,Variance))
  
  names(hs_tabl_descr) <- c("Descriptive Statistics","Value")
  names(hs_tabl_quant) <- c("Quantile Statistics","Value")
  
  hs_tabl_smry$Value[6] <- as.character(Min.Date)
  hs_tabl_smry$Value[7] <- as.character(Max.Date)
  
  output$oceano_var_smry <- renderTable(digits = 6,{
    hs_tabl_smry
  })
  
  output$oceano_var_plot <- renderPlot({
    ggplot(hs, aes(Hs_mean, group = Zone, fill = Zone)) +
      geom_histogram(aes(y=..density..), alpha=0.5, position="identity",
                     na.rm = TRUE,bins = 100, color = "gray") + geom_density(alpha=.2) +
      theme_minimal(base_size = 20)+
      theme(legend.position="bottom",legend.title = element_blank()) + 
      xlab(label = "") + ylab("") 
  },height = 300)
  
  output$oceano_var_quantiles <- renderTable(digits = 6,{
    hs_tabl_quant
  },width = "100%")
  
  output$oceano_var_stats <- renderTable(digits = 6,{
    hs_tabl_descr
  },width = "100%")
  
  DBI::dbDisconnect(conn)
  shinyalert(title = "", immediate = TRUE,
             text = "", 
             type = "info", 
             closeOnEsc = F, 
             closeOnClickOutside = F, 
             timer = 100)
})