source("leer_encuestas.R")
source("preprocesamiento_datos.R")

#########Modelo multinomial logístico
#####Variable respuesta: Aprobación del gobernador (Aprueba/Desaprueba/
#####                                               Ni aprueba ni desaprueba/ No sé)

##### 6060 respuestas

#####Variables independientes: 
multinomial.data2025 = sav_2025 |> 
  dplyr::select(-SbjNum,-MPO_INEGI,-Latitude,-Longitude) |> 
  dplyr::mutate(zona_metropolitana=factor(zona_metropolitana) ) |> 
  dplyr::relocate(PESOF,.after = dplyr::last_col()) |> dplyr::mutate(PESOF=round(PESOF/24,0))

multinomial.data2025$P19[multinomial.data2025$P19=='No sé']='Ni aprueba ni desaprueba'
multinomial.data2025$P19=factor(multinomial.data2025$P19,levels =levels(multinomial.data2025$P19)[c(3,2,4)] )



levels(multinomial.data2025$P19)

library(nnet)
multi_mylogit2025= multinom(
  reformulate(names(multinomial.data2025)[c(2:6,11,15,16,17
                                        #,18#Gobierno ayuda a los baches
                                        ,19,22:30,31,32,33,35,36,37)],"P19"),
  weights = multinomial.data2025$PESOF, ##Pesos normalizados
  data = multinomial.data2025,
  model = TRUE)
summary(multi_mylogit2025)

var_seleccionada = "Sexo"


p = summary(multi_mylogit2025)$coefficients |> as.data.frame()

multivariada_cocientes = p |> 
  dplyr::mutate(grupo = row.names(p)) |> 
  tidyr::pivot_longer(
    cols = -grupo,          
    names_to = "nombre",  
    values_to = "coeficiente_valor"             
  ) |> 
  dplyr::relocate(grupo, .after = coeficiente_valor) |> 
  dplyr::filter(grepl(pattern = var_seleccionada, x = nombre)) |> 
  dplyr::mutate(nombre = gsub(pattern = var_seleccionada, replacement = "", x = nombre),
                id = paste0(nombre, "_", grupo)) 




# convert to p-values
p_values = (1 - pnorm(abs(summary(multi_mylogit2025)$coefficients/summary(multi_mylogit2025)$standard.errors)))*2 
p_values = p_values |>  as.data.frame()
multivariada_p_valor = p_values |> 
  dplyr::mutate(grupo = row.names(p_values)) |> 
  tidyr::pivot_longer(
    cols = -grupo,          
    names_to = "nombre",  
    values_to = "p_valor"             
  ) |> 
  dplyr::relocate(grupo, .after = p_valor) |> 
  dplyr::filter(grepl(pattern = var_seleccionada, x = nombre)) |> 
  dplyr::mutate(nombre = gsub(pattern = var_seleccionada, replacement = "", x = nombre),
                id = paste0(nombre, "_", grupo)) |> 
  dplyr::select(id, p_valor)
  

multivariada = merge(x = multivariada_cocientes, y = multivariada_p_valor, by = "id", all.x = T)
multivariada = multivariada |>  dplyr::select(-id) |> 
  dplyr::relocate(p_valor, .after = coeficiente_valor) |> 
  dplyr::relocate(grupo, .after = nombre)


# Prediccion
multi_mylogit2025= multinom(
  reformulate(names(multinomial.data2025)[c(2:6,11,15,16,17
                                        #,18#Gobierno ayuda a los baches
                                        ,19,22:30,31,32,33,35,36,37)],"P19"),
  weights = multinomial.data2025$PESOF, ##Pesos normalizados
  data = multinomial.data2025,
  model = TRUE)
summary(multi_mylogit2025)


var_seleccionada = "zona_metropolitana"
nuevo = list()
for (var in names(multinomial.data2025[c(2:6,11,15,16,17,19,22:30,31,32,33,35,36,37)])) {
  if (var == "edad") {
    nuevo[[var]] = mean(multinomial.data2025$edad)
  } else if (var == var_seleccionada && is.factor(multinomial.data2025[[var]])) {
    nuevo[[var]] = levels(multinomial.data2025[[var]])
  } else if (is.factor(multinomial.data2025[[var]])) {
    nuevo[[var]] = levels(multinomial.data2025[[var]])[1]
  } else {
    nuevo[[var]] = multinomial.data2025[[var]][1]
  }
}

prediccion_multi = as.data.frame(nuevo, stringsAsFactors = FALSE)
prediccion_multi$prediccion = predict(multi_mylogit2025, newdata = prediccion_multi, type = "probs")


















###########
### PDP ###
###########

pdp_multi_d = multi_mylogit2025 |> 
  partial(pred.var = var_seleccionada, which.class = "Desaprueba", train = multinomial.data2025) |> 
  dplyr::arrange(dplyr::desc(yhat)) |> 
  as.data.frame() |> 
  dplyr::mutate(grupo = "Desaprueba")


pdp_multi_a = multi_mylogit2025 |> 
  partial(pred.var = var_seleccionada, which.class = "Aprueba", train = multinomial.data2025) |> 
  dplyr::arrange(dplyr::desc(yhat)) |> 
  as.data.frame() |> 
  dplyr::mutate(grupo = "Aprueba")

pdp_multi = dplyr::bind_rows(pdp_multi_d, pdp_multi_a)











####################
### Prueba 2024 ####
####################

#####Variables independientes: 
multinomial.data2024 = sav_2024 |>
  dplyr::mutate(zona_metropolitana=factor(zona_metropolitana) ) |> 
  dplyr::relocate(PESOF,.after = dplyr::last_col()) |> dplyr::mutate(PESOF=round(PESOF/24,0))

multinomial.data2024$P19[multinomial.data2024$P19=='No sé']='Ni aprueba ni desaprueba'
multinomial.data2024$P19=factor(multinomial.data2024$P19,levels =levels(multinomial.data2024$P19)[c(3,2,4)] )

multinomial.data2024 = multinomial.data2024 |> 
  dplyr::select(-P20, -Latitude, -Longitude) |> 
  dplyr::relocate(P19, .before = zona_metropolitana)


multi_mylogit2024= multinom(
  reformulate(names(multinomial.data2024)[c(2:6,11,15,16,17
                                            #,18#Gobierno ayuda a los baches
                                            ,19,22:30,31,32,33,35,36,37)],"P19"),
  weights = multinomial.data2024$PESOF, ##Pesos normalizados
  data = multinomial.data2024,
  model = TRUE)
summary(multi_mylogit2024)


var_seleccionada = "zona_metropolitana"
nuevo = list()
for (var in names(multinomial.data2024[c(2:6,11,15,16,17,19,22:30,31,32,33,35,36,37)])) {
  if (var == "edad") {
    nuevo[[var]] = mean(multinomial.data2024$edad)
  } else if (var == var_seleccionada && is.factor(multinomial.data2024[[var]])) {
    nuevo[[var]] = levels(multinomial.data2024[[var]])
  } else if (is.factor(multinomial.data2024[[var]])) {
    nuevo[[var]] = levels(multinomial.data2024[[var]])[1]
  } else {
    nuevo[[var]] = multinomial.data2024[[var]][1]
  }
}

prediccion_multi = as.data.frame(nuevo, stringsAsFactors = FALSE)
prediccion_multi$prediccion = predict(multi_mylogit2024, newdata = prediccion_multi, type = "probs")











































































































############################ Otro modelo

coefs_c_to_b <- summary(multi_mylogit2025)$coefficients[1, ] -  summary(multi_mylogit2025)$coefficients[2, ]

data.frame(coefs_c_to_b)





multinomial.data2025 = sav_2025 |> 
  dplyr::select(-SbjNum,-MPO_INEGI,-Latitude,-Longitude) |> 
  dplyr::mutate(zona_metropolitana=factor(zona_metropolitana) ) |> 
  dplyr::relocate(PESOF,.after = dplyr::last_col()) |> dplyr::mutate(PESOF=round(PESOF/24,0))

multinomial.data2025$P19[multinomial.data2025$P19=='No sé']='Ni aprueba ni desaprueba'
multinomial.data2025$P19=factor(multinomial.data2025$P19,levels =levels(multinomial.data2025$P19)[c(3,2,4)] )


levels(multinomial.data2025$P19)

multinomial.data2025$P19 = relevel(multinomial.data2025$P19, ref = "Desaprueba")
multinomial.data2025$P19 = relevel(multinomial.data2025$P19, ref = "Aprueba")


multinomial.data2025 = multinomial.data2025 |> 
  dplyr::mutate(
    #P19 = factor(x = P19, levels = c("Desaprueba", "Aprueba","Ni aprueba ni desaprueba")),
    P19 = factor(x = P19, levels = c("Aprueba", "Desaprueba","Ni aprueba ni desaprueba"))
  )
levels(multinomial.data2025$P19)

multi_mylogit2025= multinom(
  reformulate(names(multinomial.data2025)[c(2:6,11,15,16,17
                                        #,18#Gobierno ayuda a los baches
                                        ,19,22:30,31,32,33,35,36,37)],"P19"),
  weights = multinomial.data2025$PESOF, ##Pesos normalizados
  data = multinomial.data2025,
  model = TRUE)
summary(multi_mylogit2025)$coefficients

head(data.frame(coefs_c_to_b))

























install.packages("sjPlot")
sjPlot::tab_model(mylogit2025,transform = NULL,show.intercept = T,show.est = T,show.r2 = T)
sjPlot::plot_model(mylogit2025, sort.est = TRUE,vline.color = "red"
                   #,transform = "plogis"
                   ,show.values = TRUE, value.offset = c(0.3,0.8),value.size = 3)

c(summary(mylogit2025)$coefficients[1,]-summary(mylogit2025)$coefficients[2,])

#Pseudo R-cuadrado
library(DescTools)
PseudoR2(mylogit2025, which = c("CoxSnell","Nagelkerke","McFadden"))







gg = ggplot(multivariada_cocientes, aes(x = var_seleccionada, y = coeficiente_valor, fill = factor(grupo))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("#691c32","#b38e5d")) + 
  geom_text(aes(label = round(coeficiente_valor, 2), y = coeficiente_valor / 2),  
            position = position_dodge(width = 0.9),
            size = 4,
            color = "white") +
  labs(x = gsub("_", " ", var_seleccionada) |> stringr::str_squish() |> tools::toTitleCase(), 
       y = "Predicción",
       fill = "Año") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "bold"),
        axis.title.x = element_text(color = "black", face = "bold"))
ggplotly(gg) |>  
  config(
    modeBarButtonsToRemove = list("select2d", "lasso2d","hoverClosestCartesian", "hoverCompareCartesian","toggleSpikelines"),
    scrollZoom = TRUE,
    displaylogo = FALSE,
    doubleClick = "reset",
    locale = "es"
  )  





  
  

















































######
multinomial.data2025 = sav_2025 |> 
  dplyr::select(-SbjNum,-MPO_INEGI,-Latitude,-Longitude) |> 
  dplyr::mutate(zona_metropolitana=factor(zona_metropolitana) ) |> 
  dplyr::relocate(PESOF,.after = dplyr::last_col()) |> dplyr::mutate(PESOF=round(PESOF/24,0))

multinomial.data2025$P19[multinomial.data2025$P19=='No sé']='Ni aprueba ni desaprueba'
multinomial.data2025$P19=factor(multinomial.data2025$P19,levels =levels(multinomial.data2025$P19)[c(3,2,4)] )



levels(multinomial.data2025$P19)

library(nnet)
multi_mylogit2025= multinom(
  reformulate(names(multinomial.data2025)[c(2:6,11,15,16,17
                                        #,18#Gobierno ayuda a los baches
                                        ,19,22:30,31,32,33,35,36,37)],"P19"),
  weights = multinomial.data2025$PESOF, ##Pesos normalizados
  data = multinomial.data2025,
  model = TRUE,)

levels(multinomial.data2025$P19)

summary(multi_mylogit2025) # El modelo te da In(P(y = Aprueba)/P(y = Ni aprueba ni desaprueba)) y In(P(y = Desaprueba)/P(y = Ni aprueba ni desaprueba))
summary(multi_mylogit2025)$coefficients[1, ] # Te da los coeficientes de In(P(y = Aprueba)/P(y = Ni aprueba ni desaprueba))
summary(multi_mylogit2025)$coefficients[2, ] # Te da los coeficientes de In(P(y = Desaprueba)/P(y = Ni aprueba ni desaprueba))


interes = summary(multi_mylogit2025)$coefficients[1, ] -  summary(multi_mylogit2025)$coefficients[2, ]  # In(P(y = Aprueba)/P(y = Desaprueba))


multinomial.data2025 = multinomial.data2025 |> 
  dplyr::mutate(
    P19 = factor(x = P19, levels = c("Desaprueba", "Aprueba","Ni aprueba ni desaprueba")),
  )
levels(multinomial.data2025$P19)

multi_mylogit2025= multinom(
  reformulate(names(multinomial.data2025)[c(2:6,11,15,16,17
                                        #,18#Gobierno ayuda a losinte baches
                                        ,19,22:30,31,32,33,35,36,37)],"P19"),
  weights = multinomial.data2025$PESOF, ##Pesos normalizados
  data = multinomial.data2025,
  model = TRUE)


summary(multi_mylogit2025)$coefficients[1, ] |>  head()
interes |>  head()


