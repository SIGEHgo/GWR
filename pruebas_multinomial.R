var_seleccionada = "Sexo"

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


a = summary(multi_mylogit2025)$coefficients |>  as.data.frame()

multivariada_coeficientes_2025 = a |> 
  dplyr::mutate(grupo = row.names(a)) |> 
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
  dplyr::relocate(grupo, .after = nombre)  |> 
  dplyr::mutate(odds_ratio = dplyr::if_else(condition = is.na(odds_ratio), true = 1, false = odds_ratio))

