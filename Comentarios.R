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


### Columnas que tienen factor #####
levels(datos$ESC)       # Modificar
levels(datos$OCUP)      # Modificar
levels(datos$NSER)
levels(datos$INGR)      # Modificar
levels(datos$P15)       # Modificar
levels(datos$P16)       # Modificar
levels(datos$P27)       # Modificar
levels(datos$P30A1)     # Modificar
levels(datos$P39)       # Modificar
levels(datos$P40)       # Modificar
levels(datos$P45)       # Modificar
levels(datos$P51)       # Modificar
levels(datos$P64A6)
levels(datos$P85)       # Modificar
levels(datos$P86A1)     # Modificar
levels(datos$P99A1)     # Modificar
levels(datos$P101A1) # Los mismos del P101A1 al P101A9
levels(datos$P101A9) # Los mismos del P101A1 al P101A9

levels(datos$P8A1)      # Modificar
levels(datos$P8A2)      # Modificar
levels(datos$P9)        # Modificar
levels(datos$P12)       # Modificar
levels(datos$P23A1)
levels(datos$P26)       # Modificar
levels(datos$P52)       

levels(datos$P19)
