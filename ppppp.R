library(pdp)
# 2025
var_seleccionada = "donde_hay_mas_corrupcion"
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



datos


