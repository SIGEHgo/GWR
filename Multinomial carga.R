#########Modelo multinomial logístico
#####Variable respuesta: Aprobación del gobernador (Aprueba/Desaprueba/
#####                                               Ni aprueba ni desaprueba/ No sé)

##### 6060 respuestas

#####Variables independientes: 
multinomial.data2025 = Pro_binary.data2025 
multinomial.data2025$P19[multinomial.data2025$P19=='No sé']='Ni aprueba ni desaprueba'
multinomial.data2025$P19=factor(multinomial.data2025$P19,levels =levels(multinomial.data2025$P19)[c(3,2,4)] )



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




############
### 2024 ###
############
multinomial.data2024 = Pro_binary.data2024
multinomial.data2024$P19[multinomial.data2024$P19=='No sé']='Ni aprueba ni desaprueba'
multinomial.data2024$P19=factor(multinomial.data2024$P19,levels =levels(multinomial.data2024$P19)[c(3,2,4)] )


multi_mylogit2024= multinom(
  reformulate(names(multinomial.data2024)[c(2:37)],"P19"),
  weights = multinomial.data2024$PESOF, ##Pesos normalizados
  data = multinomial.data2024,
  model = TRUE)

com_2024 = summary(multi_mylogit2024)$coefficients |>  as.data.frame()

