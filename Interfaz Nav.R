source("binary_logistic_regression.R")

ui = page_sidebar(
  theme = bs_theme(
    version = 5,
    bootswatch = "lux",
    primary = "#691c32",
    secondary = "#b38e5d",
    success = "#1ABC9C",
    info = "#3498DB",
    warning = "#F39C12",
    danger = "#E74C3C",
    base_font = font_google("Roboto"),
    heading_font = font_google("Montserrat"),
    code_font = font_google("Fira Code")
  ),
  #title = "Visualizador Regresión Logística Multivariada",
  
  tags$style(HTML("
    html, body, .container-fluid {
      height: 100%;
      margin: 0; !important;
      padding: 0;!important;
    }
    
    .main.bslib-gap-spacing.html-fill-container{
      height: 100%;
      margin: 0; !important;
      padding: 0;!important;
    }
    
    .mitarjeta {
      height: 45vh;
    }
    .mitarjeta .plotly {
      height: 100% !important;
    }
  ")),
  
  sidebar = sidebar(
    h4("Regresión Logistica Multivariada"),
    selectInput(
      inputId = "variable",
      label = "Selecciona una variable",
      choices = names(binary.data2025)[!names(binary.data2025) %in% 
                  c("P19", "SbjNum", "Latitude", "Longitude", "PESOF", "P20", "edad")]
    )
  ),
  
  # Aquí añadimos el tabset
  tabsetPanel(
    tabPanel(
      "Predicciones",
      card(
        full_screen = TRUE,
        class = "mitarjeta",
        card_header(textOutput("titulo_2025")),
        plotlyOutput("grafica_predicciones_2025", height = "100%")
      ),
      card(
        full_screen = TRUE,
        class = "mitarjeta",
        card_header(textOutput("titulo_2024")),
        plotlyOutput("grafica_predicciones_2024", height = "100%")
      )
    ),
    tabPanel(
      "Log-odds",
      card(
        full_screen = TRUE,
        class = "mitarjeta",
        card_header(textOutput("log_titulo_2025")),
        plotlyOutput("grafica_logodds_2025", height = "100%")
      ),
      card(
        full_screen = TRUE,
        class = "mitarjeta",
        card_header(textOutput("log_titulo_2024")),
        plotlyOutput("grafica_logodds_2024", height = "100%")
      ),
    ),
    tabPanel(
      "PDP",
      card(
        full_screen = TRUE,
        class = "mitarjeta",
        card_header(textOutput("pdp_titulo_2025")),
        plotlyOutput("grafica_pdp_2025", height = "100%")
      ),
      card(
        full_screen = TRUE,
        class = "mitarjeta",
        card_header(textOutput("pdp_titulo_2024")),
        plotlyOutput("grafica_pdp_2024", height = "100%")
      ),
    ),
  )
)


server = function(input, output, session) {
  
  # Activa el panel interactivo para cambiar el tema
  #bs_themer()
  
  ###########
  # Títulos #
  ###########
  
  # Predicciones
  
  output$titulo_2025 = renderText({
    paste("Probabilidades de predicciones 2025 para la variable",
          gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase()
    )
  })
  
  output$titulo_2024 = renderText({
    paste("Probabilidades de predicciones 2024 para la variable",
          gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase()
    )
  })
  
  # Log-Odds
  
  output$log_titulo_2025 = renderText({
    paste("Log-Odds 2025 para la variable",
          gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase()
    )
  })
  
  output$log_titulo_2024 = renderText({
    paste("Log-Odds 2024 para la variable",
          gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase()
    )
  })
  
  # PDP
  
  output$pdp_titulo_2025 = renderText({
    paste("Log-Odds 2025 para la variable",
          gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase()
    )
  })
  
  output$pdp_titulo_2024 = renderText({
    paste("Log-Odds 2024 para la variable",
          gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase()
    )
  })
  
  
  
  
  ##############
  # Datos 2025 #
  ##############
  datos_prediccion_2025 = reactive({
      var_seleccionada = input$variable
      nuevo = list()
      
      for (var in names(binary.data2025[2:37])) {
        if (var == "edad") {
          nuevo[[var]] = mean(binary.data2025$edad)
        } else if (var == var_seleccionada && is.factor(binary.data2025[[var]])) {
          nuevo[[var]] = levels(binary.data2025[[var]])
        } else if (is.factor(binary.data2025[[var]])) {
          nuevo[[var]] = levels(binary.data2025[[var]])[1]
        } else {
          nuevo[[var]] = binary.data2025[[var]][1]
        } 
      }
    
    prediccion_datos_2025 = as.data.frame(nuevo, stringsAsFactors = FALSE)
    prediccion_datos_2025$prediccion = predict(mylogit2025, newdata = prediccion_datos_2025, type = "response")
    
    coeficientes_2025 = coefficients(mylogit2025)
    coeficientes_2025 = data.frame(coeficientes = coefficients(mylogit2025))
    coeficientes_2025$nombre = row.names(coeficientes_2025)
    row.names(coeficientes_2025) = NULL
    intercep = coeficientes_2025$coeficientes[coeficientes_2025$nombre == "(Intercept)"]
    coeficientes_2025 = coeficientes_2025[grep(pattern = var_seleccionada, x = coeficientes_2025$nombre),]
    coeficientes_2025$nombre = gsub(pattern = var_seleccionada, replacement = "", x = coeficientes_2025$nombre, ignore.case = T)
    prediccion_datos_2025 = merge(x = prediccion_datos_2025, y = coeficientes_2025, by.x = var_seleccionada, by.y = "nombre", all.x = T)
    prediccion_datos_2025 = prediccion_datos_2025 |> 
      dplyr::mutate(log_odds = LogitInv(coeficientes))
    
    pdp_2025 = mylogit2025 |> 
      partial(pred.var = var_seleccionada)|> 
      dplyr::arrange(dplyr::desc(yhat)) |> 
      as.data.frame()
    
    prediccion_datos_2025 = merge(x = prediccion_datos_2025, y = pdp_2025, by = var_seleccionada, all.x = T)
    
    prediccion_datos_2025
  })
  
  grafica_predicciones_2025 = reactive({
    datos_prediccion_2025() |> dplyr::select(input$variable, prediccion)
  })
  
  
  ##############
  # Datos 2024 #
  ##############
  
  datos_prediccion_2024 = reactive({
    var_seleccionada = input$variable
    nuevo = list()
    
    for (var in names(binary.data2024[2:37])) {
      if (var == "edad") {
        nuevo[[var]] = mean(binary.data2024$edad)
      } else if (var == var_seleccionada && is.factor(binary.data2024[[var]])) {
        nuevo[[var]] = levels(binary.data2024[[var]])
      } else if (is.factor(binary.data2024[[var]])) {
        nuevo[[var]] = levels(binary.data2024[[var]])[1]
      } else {
        nuevo[[var]] = binary.data2024[[var]][1]
      }
    }
    
    prediccion_datos_2024 = as.data.frame(nuevo, stringsAsFactors = FALSE)
    prediccion_datos_2024$prediccion = predict(mylogit2024, newdata = prediccion_datos_2024, type = "response")
    
    coeficientes_2024 = coefficients(mylogit2024)
    coeficientes_2024 = data.frame(coeficientes = coefficients(mylogit2024))
    coeficientes_2024$nombre = row.names(coeficientes_2024)
    row.names(coeficientes_2024) = NULL
    
    intercep = coeficientes_2024$coeficientes[coeficientes_2024$nombre == "(Intercept)"]
    coeficientes_2024 = coeficientes_2024[grep(pattern = var_seleccionada, x = coeficientes_2024$nombre),]
    coeficientes_2024$nombre = gsub(pattern = var_seleccionada, replacement = "", x = coeficientes_2024$nombre, ignore.case = T)
    prediccion_datos_2024 = merge(x = prediccion_datos_2024, y = coeficientes_2024, by.x = var_seleccionada, by.y = "nombre", all.x = T)
    prediccion_datos_2024 = prediccion_datos_2024 |> 
      dplyr::mutate(log_odds = LogitInv(coeficientes))
    
    pdp_2024 = mylogit2024 |> 
      partial(pred.var = var_seleccionada)|> 
      dplyr::arrange(dplyr::desc(yhat)) |> 
      as.data.frame()
    
    prediccion_datos_2024 = merge(x = prediccion_datos_2024, y = pdp_2024, by = var_seleccionada, all.x = T)
    
    prediccion_datos_2024
    
  })
  
  grafica_predicciones_2024 = reactive({
    datos_prediccion_2024() |> dplyr::select(input$variable, prediccion)
  })
  
  ############
  # Gráficas #
  ############
  
  #Predicciones
  
  output$grafica_predicciones_2025 = renderPlotly({
    gg = ggplot(grafica_predicciones_2025(), aes_string(x = input$variable, y = "prediccion")) +
      geom_col(fill = "#691c32", colour = "#b38e5d", size = 0.5) +
      geom_text(aes(label = round(prediccion, 2)), 
                color = "white",        
                fontface = "bold",     
                size = 4,
                position = position_stack(vjust = 0.5)) +      
      labs(x = gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase(), y = "Predicción") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "bold"),
            axis.title.x = element_text(color = "black", face = "bold"))
    ggplotly(gg)
  })
  
  output$grafica_predicciones_2024 = renderPlotly({
    gg = ggplot(grafica_predicciones_2024(), aes_string(x = input$variable, y = "prediccion")) +
      geom_col(fill = "#691c32", colour = "#b38e5d", size = 0.5) +
      geom_text(aes(label = round(prediccion, 2)), 
                color = "white",        
                fontface = "bold",     
                size = 4,
                position = position_stack(vjust = 0.5)) +             
      labs(x = gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase(), y = "Predicción") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "bold"),
            axis.title.x = element_text(color = "black", face = "bold"))
    ggplotly(gg)
  })
  
  ## Log Odds
  output$grafica_logodds_2025 = renderPlotly({
    gg = ggplot(datos_prediccion_2025(), aes_string(x = input$variable, y = "log_odds")) +
      geom_col(fill = "#691c32", colour = "#b38e5d", size = 0.5) +
      geom_text(aes(label = round(log_odds, 2)), 
                color = "white",        
                fontface = "bold",     
                size = 4,
                position = position_stack(vjust = 0.5)) +      
      labs(x = gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase(), y = "Predicción") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "bold"),
            axis.title.x = element_text(color = "black", face = "bold"))
    ggplotly(gg)
  })
  
  output$grafica_logodds_2024 = renderPlotly({
    gg = ggplot(datos_prediccion_2024(), aes_string(x = input$variable, y = "log_odds")) +
      geom_col(fill = "#691c32", colour = "#b38e5d", size = 0.5) +
      geom_text(aes(label = round(log_odds, 2)), 
                color = "white",        
                fontface = "bold",     
                size = 4,
                position = position_stack(vjust = 0.5)) +      
      labs(x = gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase(), y = "Predicción") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "bold"),
            axis.title.x = element_text(color = "black", face = "bold"))
    ggplotly(gg)
  })
  
  
  ### pdp
  
  output$grafica_pdp_2025 = renderPlotly({
    gg = ggplot(datos_prediccion_2025(), aes_string(x = input$variable, y = "yhat")) +
      geom_col(fill = "#691c32", colour = "#b38e5d", size = 0.5) +
      geom_text(aes(label = round(yhat, 2)), 
                color = "white",        
                fontface = "bold",     
                size = 4,
                position = position_stack(vjust = 0.5)) +      
      labs(x = gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase(), y = "Predicción") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "bold"),
            axis.title.x = element_text(color = "black", face = "bold"))
    ggplotly(gg)
  })
  
  output$grafica_pdp_2024 = renderPlotly({
    gg = ggplot(datos_prediccion_2024(), aes_string(x = input$variable, y = "yhat")) +
      geom_col(fill = "#691c32", colour = "#b38e5d", size = 0.5) +
      geom_text(aes(label = round(yhat, 2)), 
                color = "white",        
                fontface = "bold",     
                size = 4,
                position = position_stack(vjust = 0.5)) +      
      labs(x = gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase(), y = "Predicción") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "bold"),
            axis.title.x = element_text(color = "black", face = "bold"))
    ggplotly(gg)
  })
  
  
}

shinyApp(ui = ui, server = server)

