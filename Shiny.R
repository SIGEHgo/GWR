library(shiny)
library(bslib)

source("binary_logistic_regression.R")

ui = page_sidebar(
  title = "Visualizador Regresion Logistica Multivariada",
  
  sidebar = sidebar(
    selectInput(
      inputId = "escoger_anio",
      label = "Escoge un año:",
      choices = c("2024", "2025"),
      selected = "2025"
    ),
    selectInput(
      inputId = "variable_input",
      label = "Escoge una variable",
      choices = NULL
    )
  ),
  
  h3(textOutput("caption", container = span))
)

server = function(input, output, session) {
  
  # Reactive para la seleccion
  binary = reactive({
    if (input$escoger_anio == "2024") {
      binary.data2024
    } else {
      binary.data2025
    }
  })
  
  # Actualizar las opciones del selectInput apartir de lo dado del Input escoger_anio
  observeEvent(input$escoger_anio, {
    updateSelectInput(
      session,
      inputId = "variable_input",
      choices = names(binary())[!names(binary()) %in% c("P19", "SbjNum", "Latitude", "Longitude", "PESOF", "P20", "edad")]
    )
  })
  
  output$caption = renderText({
    paste(
      "Año", input$escoger_anio,
      "y variable", input$variable_input,
      "donde tiene como factores",
      paste(levels(binary()[[input$variable_input]]), collapse = ", ")
    )
  })
}

shinyApp(ui = ui, server = server)




