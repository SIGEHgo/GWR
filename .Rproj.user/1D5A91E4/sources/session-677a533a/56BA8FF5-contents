library(shiny)
library(bslib)
library(plotly)
library(thematic) 
library(pdp)
library(DescTools)
library(shinyWidgets)  

source("binary_logistic_regression.R")
source("Multinomial carga.R")

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
    tags$img(
      src = "https://raw.githubusercontent.com/Eduardo-Alanis-Garcia/Js/main/Planeacion_dorado.png",     
      height = "40px",      # Ajusta el tamaño
      width = "auto",       
      style = "display: block; margin: 0 auto;" # Centrar en el sidebar
    ),
    
    
    h4("Regresión Logistica Multivariada"),
    selectInput(
      inputId = "variable",
      label = "Selecciona una variable",
      choices = names(binary.data2025)[!names(binary.data2025) %in% c("P19", "SbjNum", "Latitude", "Longitude", "PESOF", "P20", "edad")]
    ),
    p(textOutput("descripciones")), 
    actionButton("modal", "Más información")
  ),
  
  # Aquí añadimos el tabset
  tabsetPanel(
    tabPanel(
      title = "Odds Ratio",
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
    tabPanel(
      title = "Odds Ratio Multivariado",
      card(
        full_screen = TRUE,
        class = "mitarjeta",
        card_header(textOutput("odd_ratio_multivariado")),
        plotlyOutput("grafica_odd_ratio_2025", height = "50%"),
        plotlyOutput("grafica_odd_ratio_2024", height = "50%")
      )
    ),
    tabPanel(
      title = "PDP Multivariado",
      card(
        full_screen = TRUE,
        class = "mitarjeta",
        card_header(textOutput("pdp_multivariado")),
        plotlyOutput("grafica_pdp_multivariado_2025", height = "50%"),
        plotlyOutput("grafica_pdp_multivariado_2024", height = "50%")
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
  
  # Log-Odds
  
  output$log_titulo = renderText({
    paste("Odds Ratio para la variable",
          gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase()
    )
  })
  
  # PDP
  
  output$pdp_titulo = renderText({
    paste("Partial Dependence Plots para la variable",
          gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase()
    )
  })
  
  # Odds Ratio Multivariado 
  
  output$odd_ratio_multivariado = renderText({
    paste("Odds Ratio Multivariado para la variable",
          gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase()
    )
  })
  
  # PDP Multivariado
  
  output$pdp_multivariado = renderText({
    paste("Partial Dependence Plots Multivariado para la variable",
          gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase()
    )
  })
  
  ##############
  # Datos 2025 #
  ##############
  datos = reactive({
    
    var_seleccionada = input$variable
    # 2025
    
    pdp_2025 = mylogit2025 |> 
      partial(pred.var = var_seleccionada, prob = T)|> 
      dplyr::arrange(dplyr::desc(yhat)) |> 
      as.data.frame() |> 
      dplyr::rename(nombre = var_seleccionada)
    
    coeficientes_2025 = coef_2025 |> 
      dplyr::mutate(nombre = row.names(coef_2025)) |> 
      dplyr::select(nombre, Estimate) |> 
      dplyr::rename(odds_ratio = Estimate) |> 
      dplyr::filter(grepl(pattern = var_seleccionada, x = nombre)) |> 
      dplyr::mutate(nombre = gsub(pattern = var_seleccionada, replacement = "", x = nombre, ignore.case = T),
                    nombre = stringr::str_squish(nombre),
                    odds_ratio = exp(odds_ratio)) |> 
      dplyr::relocate(nombre, .before = odds_ratio) 
    
    datos_2025 = merge(x = pdp_2025, y = coeficientes_2025, by = "nombre", all.x = T)
    datos_2025 = datos_2025 |> 
      dplyr::mutate(años_datos = 2025)
    
    
    # 2024
    pdp_2024 = mylogit2024 |> 
      partial(pred.var = var_seleccionada, prob = T)|> 
      dplyr::arrange(dplyr::desc(yhat)) |> 
      as.data.frame() |> 
      dplyr::rename(nombre = var_seleccionada)
    
    coeficientes_2024 = coef_2024 |> 
      dplyr::mutate(nombre = row.names(coef_2024)) |> 
      dplyr::select(nombre, Estimate) |> 
      dplyr::rename(odds_ratio = Estimate) |> 
      dplyr::filter(grepl(pattern = var_seleccionada, x = nombre)) |> 
      dplyr::mutate(nombre = gsub(pattern = var_seleccionada, replacement = "", x = nombre, ignore.case = T),
                    nombre = stringr::str_squish(nombre),
                    odds_ratio = exp(odds_ratio)) |> 
      dplyr::relocate(nombre, .before = odds_ratio) 
    
    datos_2024 = merge(x = pdp_2024, y = coeficientes_2024, by = "nombre", all.x = T)
    datos_2024 = datos_2024 |> 
      dplyr::mutate(años_datos = 2024)
    
    datos = dplyr::bind_rows(datos_2025, datos_2024)
    datos = datos |> 
      dplyr::relocate(años_datos, .after = nombre) |> 
      dplyr::mutate(odds_ratio = dplyr::if_else(condition = is.na(odds_ratio), true = 1, false = odds_ratio))
    
    datos

  })
  
  
  datos_multivariado_2025 = reactive({
    var_seleccionada = input$variable
    pdp_multi_2025_desaprueba = multi_mylogit2025 |> 
      partial(pred.var = var_seleccionada, which.class = "Desaprueba", train = multinomial.data2025, prob = T) |> 
      dplyr::arrange(dplyr::desc(yhat)) |> 
      as.data.frame() |> 
      dplyr::mutate(grupo = "Desaprueba")
    
    pdp_multi_2025_aprueba = multi_mylogit2025 |> 
      partial(pred.var = var_seleccionada, which.class = "Aprueba", train = multinomial.data2025, prob = T) |> 
      dplyr::arrange(dplyr::desc(yhat)) |> 
      as.data.frame() |> 
      dplyr::mutate(grupo = "Aprueba")
    
    pdp_multi_2025 = dplyr::bind_rows(pdp_multi_2025_desaprueba, pdp_multi_2025_aprueba)
    pdp_multi_2025 = pdp_multi_2025  |> 
      dplyr::rename(nombre = var_seleccionada) |> 
      dplyr::mutate(id = paste0(nombre, "_", grupo))
    
    multivariada_coeficientes_2025 = com_2025 |> 
      dplyr::mutate(grupo = row.names(com_2025)) |> 
      tidyr::pivot_longer(
        cols = -grupo,          
        names_to = "nombre",  
        values_to = "coeficiente_valor"             
      ) |> 
      dplyr::relocate(grupo, .after = coeficiente_valor) |> 
      dplyr::filter(grepl(pattern = var_seleccionada, x = nombre)) |> 
      dplyr::mutate(nombre = sub(pattern = var_seleccionada, replacement = "", x = nombre),
                    nombre = stringr::str_squish(nombre),
                    coeficiente_valor = exp(coeficiente_valor),
                    id = paste0(nombre, "_", grupo)) |> 
      dplyr::rename(odds_ratio = coeficiente_valor) |> 
      dplyr::select(-grupo, -nombre) 
    
    
    datos_multivariado = merge(pdp_multi_2025, y = multivariada_coeficientes_2025, by = "id", all.x = T)
    datos_multivariado = datos_multivariado |> 
      dplyr::select(-id) |> 
      dplyr::relocate(odds_ratio, .before  = yhat) |> 
      dplyr::relocate(grupo, .after = nombre) |> 
      dplyr::mutate(odds_ratio = dplyr::if_else(condition = is.na(odds_ratio), true = 1, false = odds_ratio))
    
    
    datos_multivariado
  })
  
  
  datos_multivariado_2024 = reactive({
    var_seleccionada = input$variable
    pdp_multi_2024_desaprueba = multi_mylogit2024 |> 
      partial(pred.var = var_seleccionada, which.class = "Desaprueba", train = multinomial.data2024, prob = T) |> 
      dplyr::arrange(dplyr::desc(yhat)) |> 
      as.data.frame() |> 
      dplyr::mutate(grupo = "Desaprueba")
    
    pdp_multi_2024_aprueba = multi_mylogit2024 |> 
      partial(pred.var = var_seleccionada, which.class = "Aprueba", train = multinomial.data2024, prob = T) |> 
      dplyr::arrange(dplyr::desc(yhat)) |> 
      as.data.frame() |> 
      dplyr::mutate(grupo = "Aprueba")
    
    pdp_multi_2024 = dplyr::bind_rows(pdp_multi_2024_desaprueba, pdp_multi_2024_aprueba)
    pdp_multi_2024 = pdp_multi_2024  |> 
      dplyr::rename(nombre = var_seleccionada) |> 
      dplyr::mutate(id = paste0(nombre, "_", grupo))
    
    multivariada_coeficientes_2024 = com_2024 |> 
      dplyr::mutate(grupo = row.names(com_2024)) |> 
      tidyr::pivot_longer(
        cols = -grupo,          
        names_to = "nombre",  
        values_to = "coeficiente_valor"             
      ) |> 
      dplyr::relocate(grupo, .after = coeficiente_valor) |> 
      dplyr::filter(grepl(pattern = var_seleccionada, x = nombre)) |> 
      dplyr::mutate(nombre = sub(pattern = var_seleccionada, replacement = "", x = nombre),
                    nombre = stringr::str_squish(nombre),
                    coeficiente_valor = exp(coeficiente_valor),
                    id = paste0(nombre, "_", grupo)) |> 
      dplyr::rename(odds_ratio = coeficiente_valor) |> 
      dplyr::select(-grupo, -nombre) 
    
    
    datos_multivariado = merge(pdp_multi_2024, y = multivariada_coeficientes_2024, by = "id", all.x = T)
    datos_multivariado = datos_multivariado |> 
      dplyr::select(-id) |> 
      dplyr::relocate(odds_ratio, .before  = yhat) |> 
      dplyr::relocate(grupo, .after = nombre) |> 
      dplyr::mutate(odds_ratio = dplyr::if_else(condition = is.na(odds_ratio), true = 1, false = odds_ratio))
    
    
    datos_multivariado
  })
  
  
  
  
  ############
  # Gráficas #
  ############
  
  ### Odds Ratio
  output$grafica_logodds = renderPlotly({
    gg = ggplot(datos(), aes(x = nombre, y = odds_ratio, fill = factor(años_datos))) +
      geom_bar(stat = "identity", position = position_dodge()) +
      scale_fill_manual(values = c("#691c32","#b38e5d")) + 
      geom_text(aes(label = round(odds_ratio, 2), y = odds_ratio / 2),  
                position = position_dodge(width = 0.9),
                size = 4,
                color = "white") +
      labs(x = gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase(), 
           y = "Valor",
           fill = "Año") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "bold"),
            axis.title.x = element_text(color = "black", face = "bold")) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red")
    ggplotly(gg) |>  
      config(
        modeBarButtonsToRemove = list("select2d", "lasso2d","hoverClosestCartesian", "hoverCompareCartesian","toggleSpikelines"),
        scrollZoom = TRUE,
        displaylogo = FALSE,
        doubleClick = "reset",
        locale = "es"
      )
  })
  
  ### PDP
  output$grafica_pdp = renderPlotly({
    gg = ggplot(datos(), aes(x = nombre, y = yhat, fill = factor(años_datos))) +
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
    ggplotly(gg) |>  
      config(
        modeBarButtonsToRemove = list("select2d", "lasso2d","hoverClosestCartesian", "hoverCompareCartesian","toggleSpikelines"),
        scrollZoom = TRUE,
        displaylogo = FALSE,
        doubleClick = "reset",
        locale = "es"
      )
  })
  
  
  ### Odds Ratio Multivariado
  output$grafica_odd_ratio_2025 = renderPlotly({
    gg = ggplot(datos_multivariado_2025(), aes(x = nombre, y = odds_ratio, fill = factor(grupo))) +
      geom_bar(stat = "identity", position = position_dodge()) +
      scale_fill_manual(values = c("#691c32","#b38e5d")) + 
      geom_text(aes(label = round(odds_ratio, 2), y = odds_ratio / 2),  
                position = position_dodge(width = 0.9),
                size = 4,
                color = "white") +
      labs(x = gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase(), 
           y = "Predicción",
           fill = "Año 2025") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "bold"),
            axis.title.x = element_text(color = "black", face = "bold")) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") 
    ggplotly(gg) |>  
      config(
        modeBarButtonsToRemove = list("select2d", "lasso2d","hoverClosestCartesian", "hoverCompareCartesian","toggleSpikelines"),
        scrollZoom = TRUE,
        displaylogo = FALSE,
        doubleClick = "reset",
        locale = "es"
      )
  })
  
  output$grafica_odd_ratio_2024 = renderPlotly({
    gg = ggplot(datos_multivariado_2024(), aes(x = nombre, y = odds_ratio, fill = factor(grupo))) +
      geom_bar(stat = "identity", position = position_dodge()) +
      scale_fill_manual(values = c("#691c32","#b38e5d")) + 
      geom_text(aes(label = round(odds_ratio, 2), y = odds_ratio / 2),  
                position = position_dodge(width = 0.9),
                size = 4,
                color = "white") +
      labs(x = gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase(), 
           y = "Predicción",
           fill = "Año 2024") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "bold"),
            axis.title.x = element_text(color = "black", face = "bold")) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red")
    ggplotly(gg) |>  
      config(
        modeBarButtonsToRemove = list("select2d", "lasso2d","hoverClosestCartesian", "hoverCompareCartesian","toggleSpikelines"),
        scrollZoom = TRUE,
        displaylogo = FALSE,
        doubleClick = "reset",
        locale = "es"
      )
  })
  
  
  
  ### PDP Multivariado
  output$grafica_pdp_multivariado_2025 = renderPlotly({
    gg = ggplot(datos_multivariado_2025(), aes(x = nombre, y = yhat, fill = factor(grupo))) +
      geom_bar(stat = "identity", position = position_dodge()) +
      scale_fill_manual(values = c("#691c32","#b38e5d")) + 
      geom_text(aes(label = round(yhat, 2), y = yhat / 2),  
                position = position_dodge(width = 0.9),
                size = 4,
                color = "white") +
      labs(x = gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase(), 
           y = "Predicción",
           fill = "Año 2025") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "bold"),
            axis.title.x = element_text(color = "black", face = "bold"))
    ggplotly(gg) |>  
      config(
        modeBarButtonsToRemove = list("select2d", "lasso2d","hoverClosestCartesian", "hoverCompareCartesian","toggleSpikelines"),
        scrollZoom = TRUE,
        displaylogo = FALSE,
        doubleClick = "reset",
        locale = "es"
      )
  })
  
  output$grafica_pdp_multivariado_2024 = renderPlotly({
    gg = ggplot(datos_multivariado_2024(), aes(x = nombre, y = yhat, fill = factor(grupo))) +
      geom_bar(stat = "identity", position = position_dodge()) +
      scale_fill_manual(values = c("#691c32","#b38e5d")) + 
      geom_text(aes(label = round(yhat, 2), y = yhat / 2),  
                position = position_dodge(width = 0.9),
                size = 4,
                color = "white") +
      labs(x = gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase(), 
           y = "Predicción",
           fill = "Año 2024") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "bold"),
            axis.title.x = element_text(color = "black", face = "bold"))
    ggplotly(gg) |>  
      config(
        modeBarButtonsToRemove = list("select2d", "lasso2d","hoverClosestCartesian", "hoverCompareCartesian","toggleSpikelines"),
        scrollZoom = TRUE,
        displaylogo = FALSE,
        doubleClick = "reset",
        locale = "es"
      )
  })
    
  
  observeEvent(input$modal, {
    showModal(
      modalDialog(
        title = input$nav,
        uiOutput("contenido_acordion"),
        easyClose = TRUE,
        size = "l",
        footer = modalButton("Cerrar")
      )
    )
  })
  
  
  
  output$contenido_acordion = renderUI({
    if (input$nav == "Predicciones") {
      withMathJax(  
        HTML("
          <p>Sea \\(y\\) una variable dicotómica y sean \\(x_1, x_2, \\dots, x_p\\) nuestras variables.  
          El modelo de regresión logística se expresa como:</p>
          \\[
          \\log\\left( \\frac{P(y=1)}{P(y=0)} \\right) 
          = \\beta_0 + \\beta_1 x_1 + \\beta_2 x_2 + \\dots + \\beta_p x_p
          \\]
          <p>donde \\(\\beta_0, \\beta_1, \\dots, \\beta_p\\) son los coeficientes del modelo. Es decir, estamos viendo las probabilidades que suceda dicho evento.</p>
        ")
      )
    } else if (input$nav == "Odds Ratio") {
      withMathJax(  
        HTML("
          <p>Sea \\(y\\) una variable dicotómica y sean \\(x_1, x_2, \\dots, x_p\\) nuestras variables.  
          El modelo de regresión logística se expresa como:</p>
          
          \\[
          \\log\\left( \\frac{P(y = \\text{Aprueba})}{P(y = \\text{Desaprueba})} \\right) 
          = \\beta_0 + \\beta_1 x_1 + \\beta_2 x_2 + \\dots + \\beta_p x_p
          \\]
          
          <p>donde \\(\\beta_0, \\beta_1, \\dots, \\beta_p\\) son los coeficientes del modelo.</p>
          
          <p>Aplicando la función exponencial en ambos lados de la ecuación:</p>
          
          $$
          \\begin{align*}
          \\frac{P(y = \\text{Aprueba})}{P(y = \\text{Desaprueba}))} 
          &= e^{ \\beta_0 + \\beta_1 x_1 + \\beta_2 x_2 + \\dots + \\beta_p x_p } \\\\
          &= e^{\\beta_0} \\cdot \\left( e^{\\beta_1} \\right)^{x_1} \\cdot \\left( e^{\\beta_2} \\right)^{x_2} \\cdots \\left( e^{\\beta_p} \\right)^{x_p}
          \\end{align*}
          $$
          
          <p>Donde:</p>
          <ul>
          <li>\\(\\exp(\\beta_0)\\) representa las probabilidades <em>odds</em> del resultado cuando todas las variables de entrada son iguales a cero.</li>
          <li>Cada \\(\\exp(\\beta_i)\\) representa el <em>odds ratio</em> asociado con un incremento de una unidad en \\(x_i\\).</li>
          </ul>
          
          <p>Ademas:</p>
          <ul>
          <li>Un valor de <em>odds ratio</em> igual a 1 indica ausencia de asociación entre las variables.</li>
          <li>Valores menores que 1 señalan una asociación negativa.</li>
          <li>Valores mayores que 1 indican una asociación positiva.</li>
          </ul>
        ")
      )
    } else if (input$nav == "PDP") {
      withMathJax(  
        HTML("
          <p>Las <strong>Partial Dependence Plots (PDP)</strong> permiten visualizar la relación marginal entre una variable y la predicción del modelo, promediando el efecto de todas las demás variables.</p>
      
          <p>Es decir, para cada valor de la variable de interés, se sustituye ese valor en todas las observaciones, se calcula la predicción del modelo y luego se promedian los resultados. Esto revela cómo cambia la predicción esperada al modificar únicamente esa variable.</p>
      
          <p>Sea \\( \\mathbf{x} = \\{x_1, x_2, \\dots, x_p\\} \\) el vector de predictores en un modelo con función de predicción \\( \\hat{f}(\\mathbf{x}) \\). Si particionamos \\( \\mathbf{x} \\) en un subconjunto de interés \\( \\mathbf{z}_s \\) y su complemento \\( \\mathbf{z}_c = \\mathbf{x} \\setminus \\mathbf{z}_s \\), entonces la <em>dependencia parcial</em> se define como:</p>
      
          \\[
          f_{s}(\\mathbf{z}_{s}) = \\mathbb{E}_{\\mathbf{z}_{c}} \\left[ \\hat{f}(\\mathbf{z}_{s}, \\mathbf{z}_{c}) \\right] 
          = \\int \\hat{f}(\\mathbf{z}_{s}, \\mathbf{z}_{c}) \\, p_{c}(\\mathbf{z}_{c}) \\, d\\mathbf{z}_{c}
          \\]
      
          <p>donde \\( p_c(\\mathbf{z}_c) \\) es la densidad marginal de \\( \\mathbf{z}_c \\).</p>
      
          <p>En la práctica, esta expectativa se estima con los datos de entrenamiento como:</p>
      
          \\[
          \\bar{f}_{s}(\\mathbf{z}_{s}) = \\frac{1}{n} \\sum_{i=1}^{n} \\hat{f}(\\mathbf{z}_{s}, \\mathbf{z}_{i,c})
          \\]
      
          <p>donde \\( \\mathbf{z}_{i,c} \\) representa los valores observados de las demás variables en la muestra.</p>
      
          <p>Así, el PDP muestra cómo la variable de interés afecta la predicción, manteniendo constantes (en promedio) las demás variables del modelo.</p>
          
          <p>Ademas la funcion trabajada aplica un transformacion de los datos a la escala de probabilidad, es decir, tenemos la probabilidad promedio predicha del evento de interés para cada valor, considerando los valores promedio de todas las demás variables del modelo.</p>
        ")
      )
    } else if (input$nav == "Odds Ratio Multivariado") {
      withMathJax(  
        HTML("
          <p> Un modelo logístico multinomial se basa en una categoría de referencia definida y ejecuta un modelo lineal generalizado sobre los logaritmos de las razones de probabilidades (log-odds) de pertenencia a cada una de las demás categorías frente a la categoría de referencia. </p>
          <p> Sea \\(X\\) el vector de variables de entrada y sea \\(y\\) la variable categorica que toma los valores  <em> Ni aprueba ni desaprueba </em>, <em> Aprueba </em> o <em> Desaprueba </em>, con  <em> Ni aprueba ni desaprueba </em>  como referencia, el modelo de regresión logística multinomial calculará:</p>
          
          \\[
          \\log\\left( \\frac{P(y = \\text{Aprueba})}{P(y = \\text{Ni aprueba ni desaprueba})} \\right) 
          = \\alpha X
          \\]
          
          <p>y</p>
          
          \\[
          \\log\\left( \\frac{P(y = \\text{Desaprueba})}{P(y = \\text{Ni aprueba ni desaprueba})} \\right) 
          = \\beta X
          \\]
          
          <p>para diferentes vectores de coeficientes \\(\\alpha\\) y \\( \\beta \\).</p>
          
          <p>Donde:</p>
          <ul>
          <li>\\(\\exp(\\alpha_0)\\) y \\(\\exp(\\beta_0)\\) representan las probabilidades <em>odds</em> del resultado cuando todas las variables de entrada son iguales a cero.</li>
          <li> Cada \\(\\exp(\\alpha_i)\\) y \\(\\exp(\\beta_i)\\) representa el <em>odds ratio</em> asociado con un incremento de una unidad en \\(x_i\\).</li>
          </ul>
          
          <p>Ademas:</p>
          <ul>
          <li>Un valor de <em>odds ratio</em> igual a 1 indica ausencia de asociación entre las variables.</li>
          <li>Valores menores que 1 señalan una asociación negativa.</li>
          <li>Valores mayores que 1 indican una asociación positiva.</li>
          </ul>
        ")
      )
    } else if (input$nav == "PDP Multivariado") {
      withMathJax(  
        HTML("
          <p>Las <strong>Partial Dependence Plots (PDP)</strong> permiten visualizar la relación marginal entre una variable y la predicción del modelo, promediando el efecto de todas las demás variables.</p>
      
          <p>Es decir, para cada valor de la variable de interés, se sustituye ese valor en todas las observaciones, se calcula la predicción del modelo y luego se promedian los resultados. Esto revela cómo cambia la predicción esperada al modificar únicamente esa variable.</p>
      
          <p>Sea \\( \\mathbf{x} = \\{x_1, x_2, \\dots, x_p\\} \\) el vector de predictores en un modelo con función de predicción \\( \\hat{f}(\\mathbf{x}) \\). Si particionamos \\( \\mathbf{x} \\) en un subconjunto de interés \\( \\mathbf{z}_s \\) y su complemento \\( \\mathbf{z}_c = \\mathbf{x} \\setminus \\mathbf{z}_s \\), entonces la <em>dependencia parcial</em> se define como:</p>
      
          \\[
          f_{s}(\\mathbf{z}_{s}) = \\mathbb{E}_{\\mathbf{z}_{c}} \\left[ \\hat{f}(\\mathbf{z}_{s}, \\mathbf{z}_{c}) \\right] 
          = \\int \\hat{f}(\\mathbf{z}_{s}, \\mathbf{z}_{c}) \\, p_{c}(\\mathbf{z}_{c}) \\, d\\mathbf{z}_{c}
          \\]
      
          <p>donde \\( p_c(\\mathbf{z}_c) \\) es la densidad marginal de \\( \\mathbf{z}_c \\).</p>
      
          <p>En la práctica, esta expectativa se estima con los datos de entrenamiento como:</p>
      
          \\[
          \\bar{f}_{s}(\\mathbf{z}_{s}) = \\frac{1}{n} \\sum_{i=1}^{n} \\hat{f}(\\mathbf{z}_{s}, \\mathbf{z}_{i,c})
          \\]
      
          <p>donde \\( \\mathbf{z}_{i,c} \\) representa los valores observados de las demás variables en la muestra.</p>
      
          <p>Así, el PDP muestra cómo la variable de interés afecta la predicción, manteniendo constantes (en promedio) las demás variables del modelo.</p>
          
          <p>Ademas la funcion trabajada aplica un transformacion de los datos a la escala de probabilidad, es decir, tenemos la probabilidad promedio predicha del evento de interés para cada valor, considerando los valores promedio de todas las demás variables del modelo.</p>
        ")
      )
    } else {
      "No encontro el nav"
    }
  })
  
  
  output$descripciones = renderText({
    if (input$nav == "Odds Ratio"){
      maximo = max(datos()$odds_ratio[datos()$odds_ratio != 1], na.rm = TRUE) 
      pos_max = which(datos()$odds_ratio == maximo) 
      paste0("En igualdad de condiciones, la variable ",
             gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase(),
             " con respuesta ", 
             "\"", datos()$nombre[pos_max], "\"", 
             " en el año ", datos()$años_datos[pos_max], " tiene valor de odds ratio de ", maximo |>  round(digits = 2),
             ", lo cual significa que ",
             dplyr::if_else(condition = maximo >= 1, true = "aumenta", false = "reduce"),
             " las probabilidades relativas de aprobar el trabajo realizado por el gobernador en ",
             dplyr::if_else(condition = abs(round((1 - maximo) * 100, 2)) >= 100, 
                            true = "más del doble.",
                            false = paste0(abs(round((1 - round(x = maximo, digits = 2)) * 100, 2)), "%."))
             )
    } else if (input$nav == "PDP") {
      maximo = max(datos()$yhat, na.rm = TRUE) 
      pos_max = which(datos()$yhat == maximo) 
      paste0("En ", datos()$años_datos[pos_max], " las personas que en la variable ", 
             "\"", gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase(), "\"",
             " dieron como respuesta ", "\"", datos()$nombre[pos_max], "\"",
             " presentan una probabilidad promedio predicha de ", round(x = maximo, digits = 2), " de aprobar, lo que significa que en promedio, ",
             round(x = maximo, digits = 1)*10, " de cada 10 tienden a mostrar aprobación.")
      
    } else if (input$nav == "Odds Ratio Multivariado") {
      maximo = max(datos_multivariado_2025()$odds_ratio[datos_multivariado_2025()$odds_ratio != 1], na.rm = TRUE) 
      pos_max = which(datos_multivariado_2025()$odds_ratio == maximo) 
      paste0("En igualdad de condiciones, la variable ", 
             gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase(),
             " con respuesta ",
             "\"", datos_multivariado_2025()$nombre[pos_max], "\"",
             " en el año 2025 y apartado de ", datos_multivariado_2025()$grupo[pos_max] |>  tolower(), " tiene valor de odds ratio de ", maximo |>  round(digits = 2),
             ", lo cual significa que ",
             dplyr::if_else(condition = maximo >= 1, true = "aumenta", false = "reduce"),
             " las probabilidades relativas de ", 
             dplyr::if_else(condition = datos_multivariado_2025()$grupo[pos_max] |>  tolower() == "aprueba", true = "aprobar", false = "desaprobar"),
             " el trabajo realizado por el gobernador en ",
             dplyr::if_else(condition = abs(round((1 - maximo) * 100, 2)) >= 100, 
                            true = "más del doble.",
                            false = paste0(abs(round((1 - round(x = maximo, digits = 2)) * 100, 2)), "%."))
             )
    } else if (input$nav == "PDP Multivariado") {
      maximo = max(datos_multivariado_2025()$yhat, na.rm = TRUE) 
      pos_max = which(datos_multivariado_2025()$yhat == maximo) 
      paste0("En 2025 las personas que en la variable ", 
             "\"", gsub("_", " ", input$variable) |> stringr::str_squish() |> tools::toTitleCase(), "\"",
             " dieron como respuesta ", "\"", datos_multivariado_2025()$nombre[pos_max], "\"",
             " presentan una probabilidad promedio predicha de ", round(x = maximo, digits = 2), " de ",
             dplyr::if_else(condition = datos_multivariado_2025()$grupo[pos_max] |>  tolower() == "aprueba", true = "aprobar", false = "desaprobar"),
             ", lo que significa que en promedio, ",
             round(x = maximo, digits = 1)*10, " de cada 10 tienden a mostrar ",
             dplyr::if_else(condition = datos_multivariado_2025()$grupo[pos_max] |>  tolower() == "aprueba", true = "aprobación.", false = "desaprobación."))
    }else {
      "No hay nada"
    }

  })
}

shinyApp(ui = ui, server = server)

