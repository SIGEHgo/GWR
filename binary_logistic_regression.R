source("leer_encuestas.R")
source("preprocesamiento_datos.R")

# Modelo binomial logístico
# Variable respuesta: Aprobación del gobernador (Sí/No)

# 4836 respuestas

# Variables independientes: 

#chisq.test(binary.data$inseguridad_comparada_año_anterior, binary.data$seguridad_publica)
#install.packages("ggcorrplot")
# library(ggcorrplot)
# model.matrix(~0+., data=binary.data |> dplyr::select(P19,`Zona Metropolitana`,Sexo,ESC)) %>% 
#   cor(use="pairwise.complete.obs") %>% 
#   ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)


########################2025
binary.data2025 = sav_2025 |> 
  dplyr::select(-SbjNum,-MPO_INEGI,-Latitude,-Longitude) |> 
  dplyr::mutate(zona_metropolitana=factor(zona_metropolitana) ) |> 
  dplyr::relocate(PESOF,.after = dplyr::last_col()) |> dplyr::mutate(PESOF=round(PESOF/24,0))
binary.data2025$gobierno_ayuda_contra_deterioro_calles_avenidas[binary.data2025$gobierno_ayuda_contra_deterioro_calles_avenidas |> is.na()]='No sé'
binary.data2025$donde_hay_mas_corrupcion[binary.data2025$donde_hay_mas_corrupcion |> is.na()]='NO SÉ'

binary.data2025=binary.data2025 |> 
  dplyr::filter(escolaridad!='No respuesta') |> 
  dplyr::mutate(escolaridad=factor(as.character(escolaridad))) |> 
  dplyr::filter(servicio_que_afecta_economia!='Gastos médicos')|> 
  dplyr::mutate(servicio_que_afecta_economia=factor(as.character(servicio_que_afecta_economia)))


binary.data2025$servicio_que_afecta_economia = relevel(x = binary.data2025$servicio_que_afecta_economia, ref = "No sé")

########################2024
binary.data2024 =sav_2024 |> 
  dplyr::select(-Latitude,-Longitude) |> 
  dplyr::mutate(zona_metropolitana=factor(zona_metropolitana) ) |> 
  dplyr::relocate(PESOF,.after = dplyr::last_col()) |> dplyr::mutate(PESOF=round(PESOF/24,0))
binary.data2024$gobierno_ayuda_contra_deterioro_calles_avenidas[binary.data2024$gobierno_ayuda_contra_deterioro_calles_avenidas |> is.na()]='No sé'
binary.data2024$donde_hay_mas_corrupcion[binary.data2024$donde_hay_mas_corrupcion |> is.na()]='No sé'



lapply(binary.data2024[,2:37], levels)
lapply(binary.data2025[,2:37], levels)


source("homologar_respuestas_entre_años.R")


###### Inicia Enrique
source("Procesar_2024.R")
source("Procesar_2025.R")

#### Termina

binary.data2024 = Pro_binary.data2024 |> 
  dplyr::filter(P19%in%c("Aprueba","Desaprueba")) |> 
  dplyr::mutate(P19=factor(ifelse(P19=='Aprueba',1,0),levels=0:1))


binary.data2025 = Pro_binary.data2025 |> 
  dplyr::filter(P19%in%c("Aprueba","Desaprueba")) |> 
  dplyr::mutate(P19=factor(ifelse(P19=='Aprueba',1,0),levels=0:1)) 





mylogit2025=glm(reformulate(names(binary.data2025)[c(2:37)],"P19"),
            family = binomial(link = "logit"), 
            data = binary.data2025,
            weights = binary.data2025$PESOF
            )

coef_2025 = summary(mylogit2025)$coefficients |>  as.data.frame()

#
# dplyr::mutate(dplyr::across(c(ESC, OCUP, INGR, P15, P16, P27, P30A1, 
#                               P39, P40, P45, P51, P86A1, P99A1, P8A1, 
#                               P8A2, P9, P12, P26 ), forcats::fct_drop))
#

#summary2025=summary(mylogit2025)
#coeficientes_significantes2025=summary2025[["coefficients"]][summary2025[["coefficients"]][,4]<0.01,]

#Pseudo- R cuadrada
#library(DescTools)
#PseudoR2_2025=PseudoR2(mylogit2025, which = c("CoxSnell","Nagelkerke","McFadden"))

# pscl::pR2(mylogit)
# logLik(mylogit)
# 
# 





########################Esta parte es para repetir renglones tantas veces como PESOF entero
# binary.data=binary.data |>  
#   tidyr::uncount(PESOF)


mylogit2024=glm(reformulate(names(binary.data2024)[2:37],"P19"),
            family = binomial(link = "logit"), 
            data = binary.data2024, 
            weights = binary.data2024$PESOF)

#
# dplyr::mutate(dplyr::across(c(ESC, OCUP, INGR, P15, P16, P27, P30A1, 
#                               P39, P40, P45, P51, P86A1, P99A1, P8A1, 
#                               P8A2, P9, P12, P26 ), forcats::fct_drop))

coef_2024 = summary(mylogit2024)$coefficients |>  as.data.frame()
#

#summary2024 = summary(mylogit2024)
#coeficientes_significantes2024 = summary2024[["coefficients"]][summary2024[["coefficients"]][,4]<0.01,]
# confint(mylogit)
# 
# #install.packages("aod")
# library(aod)
# wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:16)
# wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 17:26)
# wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 27:29)
# wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 30:31)
# 
# model.full = glm( P19~ Edad+Sexo+ESC+OCUP+NSER,
#                   data=ejemplo.data,
#                   family = binomial())


#Pseudo- R cuadrada
#library(DescTools)
#PseudoR2_2025=PseudoR2(mylogit2024, which = c("CoxSnell","Nagelkerke","McFadden"))

#coeficientes_full2025=summary2025$coefficients |> as.data.frame()
#coeficientes_full2024=summary2024$coefficients |> as.data.frame()

# coeficientes_full2025=coeficientes_full2025 |> 
#   dplyr::filter(`Pr(>|z|)`<0.05)
# coeficientes_full2024=coeficientes_full2024 |> 
#   dplyr::filter(`Pr(>|z|)`<0.05)
# 
# 
# coeficientes_comparacion=merge(coeficientes_full2025 |> 
#                                  dplyr::mutate(coef=rownames(coeficientes_full2025)),coeficientes_full2024 |> 
#                                  dplyr::mutate(coef=rownames(coeficientes_full2024)),
#                                by='coef',all=T,suffixes = c("_2025","_2024"))
# 
# coeficientes_comparacion$DIFF2025_2024=(coeficientes_comparacion$Estimate_2025)-coeficientes_comparacion$Estimate_2024
# 
# coeficientes_comparacion_significativos_2025=coeficientes_comparacion |>
#   dplyr::filter(`Pr(>|z|)_2025`<0.1)
# 
# library(plotly)
# library(ggplot2)
# library(dplyr)
# 
# ggp <- ggplot(coeficientes_comparacion_significativos_2025, 
#               aes(x = reorder(coef, -DIFF2025_2024), y = DIFF2025_2024)) +
#   geom_bar(stat = "identity", fill = "black") +
#   coord_flip() +
#   ylab(" ") +
#   xlab(" ") 
# ggp |> ggplotly()
