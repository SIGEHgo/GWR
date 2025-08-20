var_seleccionada = "zona_metropolitana"
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
prediccion_datos$prediccion= predict.glm(mylogit2025, newdata = prediccion_datos, type='response')
prediccion_datos
all(lapply(prediccion_datos, is.factor) == F)

graficas = prediccion_datos |> 
  dplyr::select(zona_metropolitana ,prediccion)
plot(graficas)

library(ggplot2)
gg = ggplot(graficas, aes(x = zona_metropolitana, y = prediccion)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(prediccion, 6)), vjust = -0.5) +
  labs(
    title = "Predicción por Zona Metropolitana",
    x = "Zona Metropolitana",
    y = "Predicción"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(plotly)
gg |> ggplotly()

























######################

var_seleccionada = "zona_metropolitana"
prediccion_datos_2025 = expand.grid(
  var_seleccionada = levels(binary.data2025[[var_seleccionada]]),
  lapply(setdiff(names(binary.data2025), var_seleccionada), function(v) {
    if (is.factor(binary.data2025[[v]])) {
      levels(binary.data2025[[v]])[1]
    } else if (v == "edad") {
      mean(binary.data2025$edad)
    } else {
      mean(binary.data2025[[v]], na.rm = TRUE)
    }
  })
)
