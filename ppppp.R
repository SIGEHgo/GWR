library(pdp)
# 2025
var_seleccionada = "ocupacion"
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



gg = ggplot(datos, aes(x = nombre, y = odds_ratio, fill = factor(años_datos))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("#691c32","#b38e5d")) + 
  {
    if (nrow(datos) < 12) {
      geom_text(
        aes(label = round(odds_ratio, 2), y = odds_ratio / 2),  
        position = position_dodge(width = 0.9),
        size = 4,
        color = "white"
      )
    } else {
      geom_text(
        aes(label = round(odds_ratio, 2), y = odds_ratio / 2),  
        position = position_dodge(width = 0.9),
        size = 2,
        color = "white"
      )
    }
  } +
  labs(x = gsub("_", " ", "Hola Hola") |> stringr::str_squish() |> tools::toTitleCase(), 
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


dplyr
