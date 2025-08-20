library(shiny)
library(bslib)
library(plotly) 

### Graficar los coeficientes del regresion + el intercept

source("binary_logistic_regression.R")

ui = page_sidebar(
  title = "Visualizador Regresión Logística Multivariada",
  sidebar = sidebar(
    selectInput(
      inputId = "variable",
      label = "Escoge una variable",
      choices = names(binary.data2025)[!names(binary.data2025) %in% c("P19", "SbjNum", "Latitude", "Longitude", "PESOF", "P20", "edad")]
    )
  ),
  h3(textOutput("texto", container = span)),
  tableOutput("tabla_prediccion"),
  plotlyOutput(outputId = "grafica")
)

server = function(input, output, session) {
  datos_prediccion = reactive({
    var_seleccionada = input$variable
    nuevo = list()
    
    for (var in names(binary.data2025[2:37])) {
      if (var == "edad") {
        nuevo[[var]] = mean(binary.data2025$edad)
      } else if (var == var_seleccionada && is.factor(binary.data2025[[var]])) { # Seleccionada
        nuevo[[var]] = levels(binary.data2025[[var]])
      } else if (is.factor(binary.data2025[[var]])) {                            # Factores tomar el primero
        nuevo[[var]] = levels(binary.data2025[[var]])[1]
      } else {
        nuevo[[var]] = binary.data2025[[var]][1]                                 # Por si no tiene factor tomar el primero
      }
    }
    
    prediccion_datos = as.data.frame(nuevo, stringsAsFactors = F)
    prediccion_datos$prediccion = predict(mylogit2025, newdata = prediccion_datos, type = "response")
    
    coeficientes = coefficients(mylogit2025)
    coeficientes = as.data.frame(coeficientes)
    coeficientes$nombre = row.names(coeficientes)
    row.names(coeficientes) = NULL
    intercep = coeficientes$coeficientes[coeficientes$nombre == "(Intercept)"]
    coeficientes = coeficientes[grep(pattern = var_seleccionada, x = coeficientes$nombre),]
    coeficientes$nombre = gsub(pattern = var_seleccionada, replacement = "", x = coeficientes$nombre, ignore.case = T)
    prediccion_datos = merge(x = prediccion_datos, y = coeficientes, by.x = var_seleccionada, by.y = "nombre", all.x = T)
    prediccion_datos = prediccion_datos |> 
      dplyr::mutate(log_odds = LogitInv(coeficientes))
  })
  
  grafica = reactive({
    datos_prediccion() |> 
      dplyr::select(input$variable, prediccion)
  })
  
  
  output$texto = renderText({
    paste( 
      "Variable:", input$variable, 
      "con factores:",
      paste(levels(binary.data2025[[input$variable]]), collapse = ", ")
    )
  })
  
  output$tabla_prediccion = renderTable({
    datos_prediccion()
  })
  
  output$grafica = renderPlotly({
    library(ggplot2)
    gg = ggplot(grafica(), aes_string(x = input$variable, y = "prediccion")) +
      geom_col(fill = "steelblue") +
      geom_text(aes(label = round(prediccion, 6)), vjust = -0.5) +
      labs(
        title = "Predicción por Zona Metropolitana",
        x = input$variable,
        y = "Predicción"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    library(plotly)
    ggplotly(gg)
  })
}

shinyApp(ui = ui, server = server)

