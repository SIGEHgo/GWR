library(foreign)
datos = foreign::read.spss("2024/Datos/Base 437 Hidalgo Julio 2024.SAV", to.data.frame = T)
datos[is.na(datos)]='-'

variable_labels_2024 = attr(datos, "variable.labels")
datos_variables = as.data.frame(matrix(c(colnames(datos),variable_labels_2024),ncol=2,nrow=length(colnames(datos)),byrow = F))

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





mun = sf::read_sf("../../Importantes_documentos_usar/Municipios/municipiosjair.shp")
mun = mun |>  
  dplyr::select(NOM_MUN, geometry)

zonas = readxl::read_excel("../../Importantes_documentos_usar/Banco de datos infografias _Eduardo.xlsx")
zonas = zonas |> 
  dplyr::filter(!is.na(Región)) |> 
  dplyr::select(Municipio, `Zona Metropolitana`) |> 
  dplyr::mutate(`Zona Metropolitana` = dplyr::if_else(condition = is.na(`Zona Metropolitana`), true = "Sin zona", false = `Zona Metropolitana`))

zonas = merge(x = zonas, y = mun, by.x = "Municipio", by.y = "NOM_MUN", all.x = T)
zonas = zonas |> 
  dplyr::rename(MPO_INEGI = Municipio) |> 
  sf::st_as_sf(crs = sf::st_crs(mun))


sitio = datos |> 
  dplyr::select(SbjNum, Latitude, Longitude) |> 
  sf::st_as_sf(coords = c("Longitude", "Latitude") , crs = sf::st_crs(mun))

sitio = sf::st_join(x = sitio, y = zonas, join = sf::st_within) 
sitio = sitio |>  sf::st_drop_geometry() 
  
datos = merge(x = datos, y = sitio, by = "SbjNum", all.x = T)

###
# 2025   2024                           Nuevo Nombre
# P15    P16                            
# P16    P17
# P27    P29
# P30A1  P32A1    
# P39    P43
# P40    P44
# P45    P50
# P51    P55
# P64A6  P71A6
# P64B6  P71B6
# P85    P97
# P86A1  P98A1
# P99A1  P100A1
# P101A1:P101A9 P102A1:P102A9

# P8A1   P8A1
# P8A2   P8A2
# P9     P9
# P12    P13   
# P23A1  P25A1
# P26    P27
# P52R1  P56


datos = datos |>  
  dplyr::select(P20,SbjNum, MPO_INEGI, `Zona Metropolitana`, Edad, ESC, OCUP, NSER, INGR, P16, P17, P29, P32A1, P43, P44, P50, P55, P71A6, P71B6, P97,  P98A1, P100A1, P102A1:P102A9, PESOF,Latitude,Longitude,
                P8A1, P8A2, P9, P13, P25A1, P27, P56) |> 
  dplyr::rename(P15 = P16,
                P16 = P17,
                P27 = P29,
                P30A1 = P32A1,
                P39 = P43,
                P40 = P44,
                P45 = P50,
                P51 = P55,
                P64A6 = P71A6,
                P64B6 = P71B6,
                P85 = P97,
                P86A1 = P98A1,
                P99A1 = P100A1,
                P101A1 = P102A1,             
                P101A2 = P102A2,
                P101A3 = P102A3,
                P101A4 = P102A4,
                P101A5 = P102A5,
                P101A6 = P102A6,
                P101A7 = P102A7,
                P101A8 = P102A8,
                P101A9 = P102A9,            
                
                P8A1 = P8A1,
                P8A2 = P8A2,
                P9 = P9,
                P12 = P13,
                P23A1 = P25A1,
                P26 = P27,
                P52 = P56,
                P19 = P20
                ) |> 
  dplyr::mutate(dplyr::across(tidyselect::where(is.factor), forcats::fct_drop)) 


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




####
### Borrar variables del Global Environment
rm(factores, zonas, variable_labels_2024, mun, sitio)
