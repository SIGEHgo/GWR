#########Modelo multinomial logístico
#####Variable respuesta: Aprobación del gobernador (Aprueba/Desaprueba/
#####                                               Ni aprueba ni desaprueba/ No sé)

##### 6060 respuestas

#####Variables independientes: 
multinomial.data2025 = Pro_binary.data2025
# multinomial.data2025=multinomial.data2025 |> 
#   dplyr::filter(ocupacion!='No respuesta') |> 
#   dplyr::mutate(dplyr::across(tidyselect::where(is.factor), forcats::fct_drop),
#                 ocupacion = relevel(x = ocupacion, ref = "Hogar")) 
multinomial.data2025$P19[multinomial.data2025$P19=='No sé']='Ni aprueba ni desaprueba'

multinomial.data2025 = multinomial.data2025 |> 
  dplyr::mutate(dplyr::across(tidyselect::where(is.factor), forcats::fct_drop),
                P19 = factor(x = P19, levels = c("Ni aprueba ni desaprueba", "Aprueba", "Desaprueba")))


multinomial.data2025$P19 |>  unique()

levels(multinomial.data2025$P19)

library(nnet)
# multi_mylogit2025= multinom(
#   reformulate(names(multinomial.data2025)[c(2:6,11,15,16,17
#                                             #,18#Gobierno ayuda a los baches
#                                             ,19,22:30,31,32,33,35,36,37)],"P19"),
#   weights = multinomial.data2025$PESOF, ##Pesos normalizados
#   data = multinomial.data2025,
#   model = TRUE)




multi_mylogit2025= multinom(
  reformulate(names(multinomial.data2025)[c(2:37)],"P19"),
  weights = multinomial.data2025$PESOF, ##Pesos normalizados
  data = multinomial.data2025,
  model = TRUE)

com_2025 = summary(multi_mylogit2025)$coefficients |>  as.data.frame()
PseudoR2_2025_multi=PseudoR2(multi_mylogit2025, which = c("CoxSnell","Nagelkerke","McFadden"))




############
### 2024 ###
############
multinomial.data2024 = Pro_binary.data2024 |> 
  dplyr::rename(P19 = P20) |> 
  dplyr::mutate(P19 = stringr::str_squish(P19))
  # |> dplyr::filter(ocupacion!='No respuesta') |> 
  # dplyr::mutate(dplyr::across(tidyselect::where(is.factor), forcats::fct_drop),
  #               ocupacion = relevel(x = ocupacion, ref = "Hogar")) 
multinomial.data2024$P19[multinomial.data2024$P19=='No sé']='Ni aprueba ni desaprueba'
multinomial.data2024 = multinomial.data2024 |> 
  dplyr::mutate(dplyr::across(tidyselect::where(is.factor), forcats::fct_drop),
                P19 = factor(x = P19, levels = c("Ni aprueba ni desaprueba", "Aprueba", "Desaprueba")))

multinomial.data2024$P19 |>  unique()
  
multi_mylogit2024= multinom(
  reformulate(names(multinomial.data2024)[c(2:37)],"P19"),
  weights = multinomial.data2024$PESOF, ##Pesos normalizados
  data = multinomial.data2024,
  model = TRUE)

com_2024 = summary(multi_mylogit2024)$coefficients |>  as.data.frame()
PseudoR2_2024_multi=PseudoR2(multi_mylogit2024, which = c("CoxSnell","Nagelkerke","McFadden"))

