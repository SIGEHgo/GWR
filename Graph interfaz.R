library(shiny)
library(bslib)
library(plotly)
library(thematic) 
library(pdp)
library(DescTools)
library(shinyWidgets)  # Checar se ve con varias cosas interesantes



#source("binary_logistic_regression.R")

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
      height: 92vh;
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
      choices = names(binary.data2025)[!names(binary.data2025) %in% c("P19", "SbjNum", "Latitude", "Longitude", "PESOF", "P20", "edad")]
    ),
    page_fillable(
      accordion(
        accordion_panel(title = "Mas información", 
                        uiOutput("contenido_acordion"),
                        value = "panel1"),
        id = "acordion",
        open = "panel1",     # Panel abierto por defecto
        multiple = FALSE # Solo uno abierto a la vez
      ),
      verbatimTextOutput("sel")
    )
  ),
  
  # Aquí añadimos el tabset
  tabsetPanel(
    tabPanel(
      title = "Predicciones",
      card(
        full_screen = TRUE,
        class = "mitarjeta",
        card_header(textOutput("titulo_predicciones")),
        plotlyOutput("grafica_predicciones", height = "100%")
      )
    ),
    tabPanel(
      title = "Exponencial de los coeficientes",
      card(
        full_screen = TRUE,
        class = "mitarjeta",
        card_header(textOutput("log_titulo")),
        plotlyOutput("grafica_logodds", height = "100%")
      )
    ),
    tabPanel(
      title = "PDP",
      card(
        full_screen = TRUE,
        class = "mitarjeta",
        card_header(textOutput("pdp_titulo")),
        plotlyOutput("grafica_pdp", height = "100%")
      )
    ),
    id = "nav"
  )
)


server = function(input, output, session) {
  
  # Activa el panel interactivo para cambiar el tema
  #bs_themer()
  
  ###########
  # Títulos #
  ###########
  
  # Predicciones
  
  output$titulo_predicciones = renderText({
    paste("Probabilidades de predicciones para la variable",
          gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase()
    )
  })
  
  # Log-Odds
  
  output$log_titulo = renderText({
    paste("Exponencial de los coeficientes para la variable",
          gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase()
    )
  })
  
  # PDP
  
  output$pdp_titulo = renderText({
    paste("PDP para la variable",
          gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase()
    )
  })
  
  
  ##############
  # Datos 2025 #
  ##############
  datos = reactive({
    
    ### 2025
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
      dplyr::mutate(log_odds = exp(coeficientes))   # Odds ratio
    
    pdp_2025 = mylogit2025 |> 
      partial(pred.var = var_seleccionada)|> 
      dplyr::arrange(dplyr::desc(yhat)) |> 
      as.data.frame()
    
    prediccion_datos_2025 = merge(x = prediccion_datos_2025, y = pdp_2025, by = var_seleccionada, all.x = T)
    prediccion_datos_2025 = prediccion_datos_2025 |> 
      dplyr::mutate(año_datos = 2025)
    
    ### 2024
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
      dplyr::mutate(log_odds = exp(coeficientes))
    
    pdp_2024 = mylogit2024 |> 
      partial(pred.var = var_seleccionada)|> 
      dplyr::arrange(dplyr::desc(yhat)) |> 
      as.data.frame()
    
    prediccion_datos_2024 = merge(x = prediccion_datos_2024, y = pdp_2024, by = var_seleccionada, all.x = T)
    
    prediccion_datos_2024 = prediccion_datos_2024 |> 
      dplyr::mutate(año_datos = 2024)
    
    datos = dplyr::bind_rows(prediccion_datos_2025, prediccion_datos_2024)
    datos
  })
  
  ############
  # Gráficas #
  ############
  
  #Predicciones
  
  output$grafica_predicciones = renderPlotly({
    gg = ggplot(datos(), aes(x = .data[[input$variable]], y = prediccion, fill = factor(año_datos))) +
      geom_bar(stat = "identity", position = position_dodge()) +
      scale_fill_manual(values = c("#691c32","#b38e5d")) + 
      geom_text(aes(label = round(prediccion, 2), y = prediccion / 2),  
                position = position_dodge(width = 0.9),
                size = 4,
                color = "white") +
      labs(x = gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase(), 
           y = "Predicción",
           fill = "Año") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "bold"),
            axis.title.x = element_text(color = "black", face = "bold"))
    ggplotly(gg)
  })
  
  ### Log-Odds
  output$grafica_logodds = renderPlotly({
    gg = ggplot(datos(), aes(x = .data[[input$variable]], y = log_odds, fill = factor(año_datos))) +
      geom_bar(stat = "identity", position = position_dodge()) +
      scale_fill_manual(values = c("#691c32","#b38e5d")) + 
      geom_text(aes(label = round(log_odds, 2), y = log_odds / 2),  
                position = position_dodge(width = 0.9),
                size = 4,
                color = "white") +
      labs(x = gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase(), 
           y = "Predicción",
           fill = "Año") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "bold"),
            axis.title.x = element_text(color = "black", face = "bold"))
    ggplotly(gg)
  })
  
  ### PDP
  output$grafica_pdp = renderPlotly({
    gg = ggplot(datos(), aes(x = .data[[input$variable]], y = yhat, fill = factor(año_datos))) +
      geom_bar(stat = "identity", position = position_dodge()) +
      scale_fill_manual(values = c("#691c32","#b38e5d")) + 
      geom_text(aes(label = round(yhat, 2), y = yhat / 2),  
                position = position_dodge(width = 0.9),
                size = 4,
                color = "white") +
      labs(x = gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase(), 
           y = "Predicción",
           fill = "Año") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "bold"),
            axis.title.x = element_text(color = "black", face = "bold"))
    ggplotly(gg)
  })
  
  output$contenido_acordion = renderUI({
    if (input$nav == "Predicciones") {
      "Informacion de Predicciones"
    } else if (input$nav == "Exponencial de los coeficientes") {
      "Informacion de Exponencial de los coeficientes"
    } else if (input$nav == "PDP") {
      "Informacion de PDP"
    } else {
      "Todo mal"
    }
  })

}

shinyApp(ui = ui, server = server)

