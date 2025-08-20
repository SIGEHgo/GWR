library(foreign)
datos = foreign::read.spss("2025/Datos/Base de datos (con geolocalizacion y codificacion) 2025.SAV",to.data.frame = T)
datos[is.na(datos)]='-'

variable_labels_2025 = attr(datos, "variable.labels")
datos_variables=as.data.frame(matrix(c(colnames(datos),variable_labels_2025),ncol=2,nrow=length(colnames(datos)),byrow = F))

factores = datos |> 
  dplyr::select(tidyselect::where(is.factor)) |> 
  dplyr::mutate(dplyr::across(tidyselect::where(is.factor), forcats::fct_drop)) |> 
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = "variable",
                      values_to = "factores") |> 
  dplyr::distinct() 

factores = factores |> 
  dplyr::group_by(variable) |> 
  dplyr::summarise(
    factores = paste(factores, collapse = ", ")
  ) |> 
  dplyr::ungroup()

datos_variables = merge(x = datos_variables, y = factores, by.x = "V1", by.y = "variable", all.x = T)




zonas = readxl::read_excel("../../Importantes_documentos_usar/Banco de datos infografias _Eduardo.xlsx")
zonas = zonas |> 
  dplyr::filter(!is.na(Región)) |> 
  dplyr::select(Municipio, `Zona Metropolitana`) |> 
  dplyr::mutate(`Zona Metropolitana` = dplyr::if_else(condition = is.na(`Zona Metropolitana`), true = "Sin zona", false = `Zona Metropolitana`))

datos = merge(x = datos, y = zonas, by.x = "MPO_INEGI", by.y = "Municipio", all.x = T)

datos = datos |>  
  dplyr::select(P19, SbjNum, MPO_INEGI, `Zona Metropolitana`, Edad, ESC, OCUP, NSER, INGR, P15, P16, P27, P30A1, P39, P40, P45, P51, P64A6, P64B6, P85, P86A1, P99A1, P101A1:P101A9, PESOF,Latitude, Longitude,  
                P8A1, P8A2, P9, P12, P23A1, P26, P52)



################
### Factores ###
################

# Ejemplo sav_2025$OCUP=factor(sav_2025$OCUP,levels = c(levels(sav_2025$OCUP)[12],levels(sav_2025$OCUP)[1:11]))

# datos |> dplyr::select(where(is.factor)) # Para ver columnas que tienen factor

datos = datos |>
  dplyr::mutate(dplyr::across(tidyselect::where(is.factor), forcats::fct_drop))            # Elimina el nivel si no es usa

datos = datos |> 
  dplyr::mutate(
    P19 = factor(x = P19, levels = c(levels(P19)[length(levels(P19))], levels(P19)[1:(length(levels(P19)) - 1)])),
    ESC = factor(x = ESC, levels = c(levels(ESC)[length(levels(ESC))], levels(ESC)[1:(length(levels(ESC)) - 1)])),
    OCUP = factor(x = OCUP, levels = c(levels(OCUP)[length(levels(OCUP))], levels(OCUP)[1:(length(levels(OCUP)) - 1)])),
    INGR = factor(x = INGR, levels = c(levels(INGR)[length(levels(INGR))], levels(INGR)[1:(length(levels(INGR)) - 1)])),
    P15 = factor(x = P15, levels = c(levels(P15)[length(levels(P15))], levels(P15)[1:(length(levels(P15)) - 1)])),
    P16 = factor(x = P16, levels = c(levels(P16)[length(levels(P16))], levels(P16)[1:(length(levels(P16)) - 1)])),
    P27 = factor(x = P27, levels = c(levels(P27)[length(levels(P27))], levels(P27)[1:(length(levels(P27)) - 1)])),
    P30A1 = factor(x = P30A1, levels = c(levels(P30A1)[length(levels(P30A1))], levels(P30A1)[1:(length(levels(P30A1)) - 1)])),
    P39 = factor(x = P39, levels = c(levels(P39)[length(levels(P39))], levels(P39)[1:(length(levels(P39)) - 1)])),
    P40 = factor(x = P40, levels = c(levels(P40)[length(levels(P40))], levels(P40)[1:(length(levels(P40)) - 1)])),
    P45 = factor(x = P45, levels = c(levels(P45)[length(levels(P45))], levels(P45)[1:(length(levels(P45)) - 1)])),
    P51 = factor(x = P51, levels = c(levels(P51)[length(levels(P51))], levels(P51)[1:(length(levels(P51)) - 1)])),
    P85 = factor(x = P85, levels = c(levels(P85)[length(levels(P85))], levels(P85)[1:(length(levels(P85)) - 1)])),
    P86A1 = factor(x = P86A1, levels = c(levels(P86A1)[length(levels(P86A1))], levels(P86A1)[1:(length(levels(P86A1)) - 1)])),
    P99A1 = factor(x = P99A1, levels = c(levels(P99A1)[length(levels(P99A1))], levels(P99A1)[1:(length(levels(P99A1)) - 1)])),
    P8A1 = factor(x = P8A1, levels = c(levels(P8A1)[length(levels(P8A1))], levels(P8A1)[1:(length(levels(P8A1)) - 1)])),
    P8A2 = factor(x = P8A2, levels = c(levels(P8A2)[length(levels(P8A2))], levels(P8A2)[1:(length(levels(P8A2)) - 1)])),
    P9 = factor(x = P9, levels = c(levels(P9)[length(levels(P9))], levels(P9)[1:(length(levels(P9)) - 1)])),
    P12 = factor(x = P12, levels = c(levels(P12)[length(levels(P12))], levels(P12)[1:(length(levels(P12)) - 1)])),
    P26 = factor(x = P26, levels = c(levels(P26)[length(levels(P26))], levels(P26)[1:(length(levels(P26)) - 1)]))
  )


### Filtracion de las variables
which(datos_variables$V1 %in% names(datos)) |>  length()     # No se encuentra zona metropolitana
datos_variables = datos_variables[which(datos_variables$V1 %in% names(datos)), ]
### Pensar en como evitar un dplyr::case_when()


###################
### Renombrado ####
###################

datos = datos |> 
  dplyr::rename(inseguridad_comparada_año_anterior = P15,
                principal_problema_colonia = P16,
                seguridad_publica = P27,
                principal_delito= P30A1,
                corrupcion_en_gobierno_estatal = P39,
                donde_hay_mas_corrupcion = P40,
                corrupcion_comparar_morena_con_pri = P45,
                pobreza = P51,
                afectado_deterioro_calles_avenidas = P64A6,
                gobierno_ayuda_contra_deterioro_calles_avenidas = P64B6,
                salario_suficiente = P85,
                servicio_que_afecta_economia = P86A1,
                
                medio_de_comunicacion_informa_noticias = P99A1,
                facebook = P101A1,
                whatsapp = P101A2,
                instagram = P101A3,
                tiktok = P101A4,
                youtube = P101A5,
                twitter_X = P101A6,
                television = P101A7,
                radio = P101A8,
                periodicos = P101A9,
                
                situacion_municipio = P8A1,
                situacion_estado = P8A2,
                situacion_estado_comparado_año_anterior = P9,
                principal_problema_estado = P12,
                principal_atributo_gobernador = P23A1,
                confianza_gobernador = P26,
                hogar_recibe_algun_ayuda_del_gobierno = P52,
                
                #municipio = MPO_INEGI,
                zona_metropolitana = `Zona Metropolitana`,
                edad = Edad,
                escolaridad = ESC,
                ocupacion = OCUP,
                nivel_socieconomico = NSER,
                ingresos = INGR
  )







###########################
# 2025           Nuevo Nombre
# P15            inseguridad_comparada_un_anio
# P16            principal_problema_colonia
# P27            seguridad_publica_mejorar_igual_empeorar
# P30A1          principal_delito_ultimo_anio
# P39            corrupcion_considera_en_gobierno_estatal
# P40            donde_hay_mas_corrupcion    
# P45            corrupcion_comparar_morena_con_pri
# P51            pobreza_mejorado_igual_empeorado
# P64A6          afectado_deterioro_calles_avenidas       
# P64B6          gobierno_ayuda_contra_deterioro_calles_avenidas
# P85            salario_suficiente
# P86A1          servicio_afecta_mas_economia

# P99A1          medio_de_comunicacion_informa_noticias
# P101A1         facebook
# P101A2         whatsapp
# P101A3         instagram
# P101A4         tiktok
# P101A5         youtube
# P101A6         twitter_X
# P101A7         television
# P101A8         radio
# P101A9         periodicos

# P8A1           situacion_municipio
# P8A2           situacion_estado
# P9             estado_mejor_igual_peor
# P12            principal_problema_estado
# P23A1          principal_atributo_gobernador
# P26            confianza_gobernador
# P52            hogar_recibe_algun_ayuda_del_gobierno





### Borrar variables del Global Environment
rm(factores, zonas, variable_labels_2025)

