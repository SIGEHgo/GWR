binary.data2025$donde_hay_mas_corrupcion |> levels() |> sort()
binary.data2025$donde_hay_mas_corrupcion=as.character(binary.data2025$donde_hay_mas_corrupcion)
binary.data2025$donde_hay_mas_corrupcion[binary.data2025$donde_hay_mas_corrupcion=='ADMINISTRACIÓN PÚBLICA ESTATAL'] ='Administración Estatal'
binary.data2025$donde_hay_mas_corrupcion[binary.data2025$donde_hay_mas_corrupcion=='ADMINISTRACIÓN PÚBLICA FEDERAL/NACIONAL'] ='Administración Federal'
binary.data2025$donde_hay_mas_corrupcion[binary.data2025$donde_hay_mas_corrupcion=='ADMINISTRACIÓN PÚBLICA MUNICIPAL'] ='Administración Municipal'
binary.data2025$donde_hay_mas_corrupcion[binary.data2025$donde_hay_mas_corrupcion=='CORRUPCIÓN EN FUNCIONARIOS PÚBLICOS'] ='Corrupción en Funcionario Público'
binary.data2025$donde_hay_mas_corrupcion[binary.data2025$donde_hay_mas_corrupcion=='ECONOMÍA'] ='Economía'
binary.data2025$donde_hay_mas_corrupcion[binary.data2025$donde_hay_mas_corrupcion=='EN CUALQUIER LADO'] ='En Cualquier Lado'
binary.data2025$donde_hay_mas_corrupcion[binary.data2025$donde_hay_mas_corrupcion=='GOBIERNO EN GENERAL'] ='El Gobierno'
binary.data2025$donde_hay_mas_corrupcion[binary.data2025$donde_hay_mas_corrupcion=='LA POLÍTICA'] ='Negociaciones Políticas'
binary.data2025$donde_hay_mas_corrupcion[binary.data2025$donde_hay_mas_corrupcion=='MANEJO DE RECURSOS PÚBLICOS'] ='Administración de Recursos'
binary.data2025$donde_hay_mas_corrupcion[binary.data2025$donde_hay_mas_corrupcion=='NO SÉ'] ='No sé'
binary.data2025$donde_hay_mas_corrupcion[binary.data2025$donde_hay_mas_corrupcion=='OBRAS PÚBLICAS E INFRAESTRUCTURA'] ='Obras Públicas'
binary.data2025$donde_hay_mas_corrupcion[binary.data2025$donde_hay_mas_corrupcion=='POLICÍA Y SEGURIDAD PÚBLICA'] ='Policía y Seguridad'
binary.data2025$donde_hay_mas_corrupcion[binary.data2025$donde_hay_mas_corrupcion=='SECTOR EDUCACIÓN'] ='Educación'
binary.data2025$donde_hay_mas_corrupcion[binary.data2025$donde_hay_mas_corrupcion=='SECTOR LABORAL'] ='Trabajo'
binary.data2025$donde_hay_mas_corrupcion[binary.data2025$donde_hay_mas_corrupcion=='SECTOR SALUD'] ='Salud'
binary.data2025$donde_hay_mas_corrupcion[binary.data2025$donde_hay_mas_corrupcion=='SECTOR TRANSPORTE'] ='Transporte'
binary.data2025$donde_hay_mas_corrupcion[binary.data2025$donde_hay_mas_corrupcion=='SISTEMA JUDICIAL'] ='Justicia y Tribunales'
binary.data2025$donde_hay_mas_corrupcion[binary.data2025$donde_hay_mas_corrupcion=='SOCIEDAD Y FAMILIA'] ='Familia / Sociedad'
binary.data2025$donde_hay_mas_corrupcion[binary.data2025$donde_hay_mas_corrupcion=='TRÁMITES BUROCRÁTICOS Y SERVICIOS PÚBLICOS'] ='Trámites y Servicios'
binary.data2025$donde_hay_mas_corrupcion[binary.data2025$donde_hay_mas_corrupcion=='TRÁNSITO Y MULTAS'] ='Tránsito'

binary.data2025$donde_hay_mas_corrupcion=factor(binary.data2025$donde_hay_mas_corrupcion,levels =  binary.data2025$donde_hay_mas_corrupcion |> unique())




binary.data2024$donde_hay_mas_corrupcion[binary.data2024$donde_hay_mas_corrupcion=='Administración Nacional']='Administración Federal'





binary.data2025$principal_problema_colonia |> levels() |> sort()
binary.data2024$principal_problema_colonia |> levels() |> sort()

binary.data2025$principal_problema_colonia=as.character(binary.data2025$principal_problema_colonia)
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='AGUA POTABLE'] ='Agua Potable'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='ALUMBRADO PÚBLICO'] ='Alumbrado Público'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='ANIMALES CALLEJEROS'] ='ANIMALES CALLEJEROS'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='BASURA'] ='Basura'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='CONTAMINACIÓN'] ='Contaminación'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='CORRUPCIÓN'] ='Corrupción'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='CRIMEN ORGANIZADO'] ='Crimen Organizado'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='DESEMPEÑO DE LAS AUTORIDADES'] ='Administración del Gobierno'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='DESEMPLEO'] ='Desempleo'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='DRENAJE'] ='Drenaje'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='ECONOMÍA'] ='Economía'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='EDUCACIÓN'] ='Educación'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='ENERGÍA ELÉCTRICA EN DOMICILIO'] ='Electricidad'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='FALTA DE VIGILANCIA'] ='Falta de Vigilancia'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='INFRAESTRUCTURA'] ='Infraestructura'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='INFRAESTRUCTURA VIAL / MANTENIMIENTO EN CALLES'] ='Infraestructura Vial'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='INSEGURIDAD / DELINCUENCIA'] ='Inseguridad'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='LA SOCIEDAD'] ='Sociedad'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='MIGRACIÓN'] ='Migración'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='NARCOTRÁFICO'] ='Narcotráfico'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='NINGUNO'] ='Ninguno'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='NO SÉ'] ='No sé'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='POBREZA'] ='Pobreza'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='PROGRAMAS DE APOYO'] ='Programas de apoyo'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='ROBOS Y ASALTOS'] ='Robos'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='SERVICIOS MÉDICOS'] ='Servicios Médicos'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='SERVICIOS PÚBLICOS'] ='Servicios Públicos'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='TELECOMUNICACIONES'] ='Telecomunicaciones'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='TODO'] ='TODO'
binary.data2025$principal_problema_colonia[binary.data2025$principal_problema_colonia=='TRANSPORTE'] ='Transporte'

binary.data2025$principal_problema_colonia=factor(binary.data2025$principal_problema_colonia,levels = unique(binary.data2025$principal_problema_colonia))


binary.data2024$principal_problema_colonia[binary.data2024$principal_problema_colonia=='Mantenimiento en calles']='Infraestructura Vial'
binary.data2024$principal_problema_colonia[binary.data2024$principal_problema_colonia=='Delincuencia']='Inseguridad'
binary.data2024$principal_problema_colonia[binary.data2024$principal_problema_colonia=='Robos']='Robos'
binary.data2024$principal_problema_colonia[binary.data2024$principal_problema_colonia=='Asaltos']='Robos'





binary.data2025$principal_problema_estado |> levels() |> sort()
binary.data2024$principal_problema_estado |> levels() |> sort()

binary.data2025$principal_problema_estado=as.character(binary.data2025$principal_problema_estado)
binary.data2025$principal_problema_estado[binary.data2025$principal_problema_estado=='CORRUPCIÓN'] ='Corrupción'
#binary.data2025$principal_problema_estado[binary.data2025$principal_problema_estado=='AGUA POTABLE'] ='Derechos Humanos'
#binary.data2025$principal_problema_estado[binary.data2025$principal_problema_estado=='AGUA POTABLE'] ='Desigualdad Social'
binary.data2025$principal_problema_estado[binary.data2025$principal_problema_estado=='ECONOMÍA'] ='Economía'
binary.data2025$principal_problema_estado[binary.data2025$principal_problema_estado=='EDUCACIÓN'] ='Educación'
binary.data2025$principal_problema_estado[binary.data2025$principal_problema_estado=='DESEMPLEO'] ='Empleo'
binary.data2025$principal_problema_estado[binary.data2025$principal_problema_estado=='ADMINISTRACIÓN DEL GOBIERNO'] ='Ineficiencia y Mala Gestión'
binary.data2025$principal_problema_estado[binary.data2025$principal_problema_estado=='INFRAESTRUCTURA'] ='Infraestructura'
binary.data2025$principal_problema_estado[binary.data2025$principal_problema_estado=='INSEGURIDAD / DELINCUENCIA'] ='Inseguridad'
#binary.data2025$principal_problema_estado[binary.data2025$principal_problema_estado=='AGUA POTABLE'] ='Justicia'
binary.data2025$principal_problema_estado[binary.data2025$principal_problema_estado=='CAMBIO CLIMÁTICO'] ='Medio Ambiente'
binary.data2025$principal_problema_estado[binary.data2025$principal_problema_estado=='NINGUNO'] ='Ninguno'
binary.data2025$principal_problema_estado[binary.data2025$principal_problema_estado=='NO SÉ'] ='No sé'
binary.data2025$principal_problema_estado[binary.data2025$principal_problema_estado=='LA SOCIEDAD'] ='Problemas Sociales'
binary.data2025$principal_problema_estado[binary.data2025$principal_problema_estado=='PROGRAMAS DE APOYO'] ='Programas Sociales'
binary.data2025$principal_problema_estado[binary.data2025$principal_problema_estado=='SERVICIOS MÉDICOS'] ='Salud'
binary.data2025$principal_problema_estado[binary.data2025$principal_problema_estado=='SERVICIOS PÚBLICOS'] ='Servicios Públicos'
binary.data2025$principal_problema_estado[binary.data2025$principal_problema_estado=='TODO'] ='Todo'

binary.data2025$principal_problema_estado=factor(binary.data2025$principal_problema_estado,levels = unique(binary.data2025$principal_problema_estado))







binary.data2025$medio_de_comunicacion_informa_noticias=as.character(binary.data2025$medio_de_comunicacion_informa_noticias)
binary.data2025$medio_de_comunicacion_informa_noticias=stringr::str_to_sentence(binary.data2025$medio_de_comunicacion_informa_noticias)

binary.data2024$medio_de_comunicacion_informa_noticias=as.character(binary.data2024$medio_de_comunicacion_informa_noticias)
binary.data2024$medio_de_comunicacion_informa_noticias=stringr::str_to_sentence(binary.data2024$medio_de_comunicacion_informa_noticias)

binary.data2025$medio_de_comunicacion_informa_noticias |> unique() |> sort()
binary.data2024$medio_de_comunicacion_informa_noticias |> unique() |> sort()

binary.data2025$medio_de_comunicacion_informa_noticias[binary.data2025$medio_de_comunicacion_informa_noticias =='Noticieros']='Televisión'

binary.data2025$medio_de_comunicacion_informa_noticias=factor(binary.data2025$medio_de_comunicacion_informa_noticias,
                                                              levels = unique(binary.data2025$medio_de_comunicacion_informa_noticias))

binary.data2024$medio_de_comunicacion_informa_noticias=factor(binary.data2024$medio_de_comunicacion_informa_noticias,
                                                              levels = unique(binary.data2024$medio_de_comunicacion_informa_noticias))
