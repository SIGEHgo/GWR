library(shiny)
library(bslib)
library(plotly)
library(thematic) 

# Activa thematic para que ggplotly y ggplot usen la paleta del tema
#thematic_shiny(font = "auto")

### grafica_2025 los coeficientes del regresion + el intercept
source("binary_logistic_regression.R")

ui = page_sidebar(
  theme = bs_theme(
    version = 5,
    bootswatch = "lux",   # O prueba "cosmo" o "minty"
    primary = "#691c32",     # Azul oscuro elegante
    secondary = "#b38e5d",   # Verde esmeralda
    success = "#1ABC9C",
    info = "#3498DB",
    warning = "#F39C12",
    danger = "#E74C3C",
    base_font = font_google("Roboto"),
    heading_font = font_google("Montserrat"),
    code_font = font_google("Fira Code")
  ),
  title = "Visualizador Regresión Logística Multivariada",
  
  tags$style(HTML("
    .mitarjeta {
      height: 50vh;
    }
    .mitarjeta .plotly {
      height: 100% !important;
    }
  ")),
  
  sidebar = sidebar(
    h4("Configuración"),
    selectInput(
      inputId = "variable",
      label = "Selecciona una variable",
      choices = names(binary.data2025)[!names(binary.data2025) %in% c("P19", "SbjNum", "Latitude", "Longitude", "PESOF", "P20", "edad")]
    )
  ),
  
  card(
    full_screen = TRUE,
    class = "mitarjeta",
    card_header(textOutput("titulo_2025")),
    plotlyOutput("grafica_2025", height = "100%")
  ),
  
  card(
    full_screen = TRUE,
    class = "mitarjeta",
    card_header(textOutput("titulo_2024")),
    plotlyOutput("grafica_2024", height = "100%")
  )
)

server = function(input, output, session) {
  
  # Activa el panel interactivo para cambiar el tema
  #bs_themer()
  
  # Títulos
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
  
  # Datos 2025
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
    prediccion_datos_2025
  })
  
  grafica_2025 = reactive({
    datos_prediccion_2025() |> dplyr::select(input$variable, prediccion)
  })
  
  # Datos 2024
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
    prediccion_datos_2024
  })
  
  grafica_2024 = reactive({
    datos_prediccion_2024() |> dplyr::select(input$variable, prediccion)
  })
  
  # Gráficas
  output$grafica_2025 = renderPlotly({
    gg = ggplot(grafica_2025(), aes_string(x = input$variable, y = "prediccion")) +
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
  
  output$grafica_2024 = renderPlotly({
    gg = ggplot(grafica_2024(), aes_string(x = input$variable, y = "prediccion")) +
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
}

shinyApp(ui = ui, server = server)
