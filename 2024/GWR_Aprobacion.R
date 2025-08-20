#Rutina completa 

#Selección preliminar de variables: Muchas
dat=read_sf("GWR/WP/base.shp")
library(haven)
datos = read_sav("mapas/Winnie pooh/Regresión lineal/Base Hidalgo Julio 2024 con MPO.SAV" )
colnames(datos)
base_0 = data.frame(datos$MPO_INEGI,datos$P29, datos$P43, datos$P55, datos$P61, datos$P81,datos$P73A14, datos$P28, datos$P20 ,datos$P8A2,
                    datos[,97:105],datos$P40A1,datos$P40A2,datos$P40A3,datos[,66:73],datos[,42:44],datos$PESOF)
colnames(base_0)=c("CVE_MUN","Seguridad","-Corrupcion","Pobreza","Educación","Salud","Transparencia","Aprobación Presidente","Aprobación Gobernador","Percepción Situación del Estado",
                   "Apoyos Pobreza","Apoyos Madres solteras","Apoyo a artesanos",
                   "Apoyos para vivienda","Apoyos médicos a adultos mayores",
                   "Apoyos a agricultores","Apoyos a productores",
                   "Apoyos a personas con discapacidad",
                   "Apoyos a deportistas",
                   "Mejora de Situacion a Falta de policías","Mejora de Situación Actos de corrupción o extorsión por la policía",
                   "Mejora de Sit. Venta de drogas ilegales en su colonia",
                   "Criminales Atrapados",
                   "Criminales Encarcelados",
                   "Número de Patrullas",
                   "Capacitación de Policías",
                   "Salarios de Policías",
                   "Combate a la corrupción en la policía",
                   "Honestidad en la Policía",
                   "Respuesta de la Policía",
                   "Identifica Honestidad",
                   "Identifica Cumplimiento",
                   "Identifica Equipo de Trabajo",
                   "PESOF")
base_0=cbind(base_0,dat[,32])

base_0=dat
base_0$CVE_MUN=sprintf("%03d",base_0$CVE_MUN)
lapply(base_0,class)
#De manera preliminar, eliminamos las observaciones que pudieran incluir la respuesta "No sé"
base_1 = base_0[apply(base_0[,2:34], MARGIN = 1, FUN = function(x){ all(x !=99 & !is.na(x)) }),]
#1019/6100=16% de las observaciones originales

#Procedemos con una rutina de AIC 
modelo_completo=lm(`Aprobación Gobernador`~ .,base_1[,2:33],weights = base_1$PESOF)
summary(modelo_completo)
AIC=MASS::stepAIC(modelo_completo,direction = "backward")
#Nos quedamos con:
#`Aprobación Gobernador` ~ Seguridad + `-Corrupcion` + Pobreza + 
#                     Educación + Salud + Transparencia + `Aprobación Presidente` + 
#                     `Percepción Situación del Estado` + `Apoyos Pobreza` + 
#                     `Apoyos médicos a adultos mayores` + `Apoyos a agricultores` + 
#                     `Salarios de Policías` + `Respuesta de la Policía`)

#Volvemos a la base original y buscamos más observaciones completas:


base_2=base_0[,c("CVE_MUN","Seguridad",
                 "-Corrupcion",
                 "Educación",
                 "Salud",
                 "Transparencia",
                 "Aprobación Presidente",
                 "Percepción Situación del Estado",
                 "Apoyos Pobreza",
                 "Apoyos a agricultores",
                 "Salarios de Policías","Aprobación Gobernador",
                 "Respuesta de la Policía",
                 "Identifica Honestidad",
                 "Identifica Cumplimiento",
                 "Identifica Equipo de Trabajo"
                 ,"PESOF")]


#base_2=cbind(base_2,dat[,32])
base_2 = base_2[apply(base_2[,2:16], MARGIN = 1, FUN = function(x){ all(x !=99 & !is.na(x)) }),]
modelo_simple=lm(`Aprobación Gobernador`~.,data=base_2[,2:16],weights = base_2$PESOF)
summary(modelo_simple)
library(dplyr)
base_3=base_2|>
  group_by(CVE_MUN)|>
  summarise_all(mean)
library(sf)
shape_hidalgo=read_sf("rasters/municipiosjair.shp")
base_3$CVE_MUN=sprintf("%03d",base_3$CVE_MUN)
base_3=merge(x=base_3,y=shape_hidalgo[,c(3)],by='CVE_MUN',all.x=T)

###GWR
#Siempre sí con los 84 municipios
model <- lm(`Aprobación Gobernador`~ ., data = base_3[,2:16],weights = base_3$PESOF)
summary(model)

map.resids <- cbind(base_3, residuals(model)) 
map.resids= st_as_sf(map.resids)
colnames(map.resids)[18]='error de modelo'
map.resids$`error de modelo`=abs(map.resids$`error de modelo`)
qtm(map.resids, fill = "error de modelo")+tm_fill(col = "error de modelo", style = "cont", palette = "-RdYlBu", breaks = c(min(map.resids$`error de modelo`), 0, max(map.resids$`error de modelo`)))

#Continuamos:
base=st_as_sf(base_3[,c(2:16,18)])
base=as(base, "Spatial")
library(spgwr)
GWRbandwidth <- gwr.sel(Aprobación.Gobernador~ . ,  data =base , adapt = T)
#GWR
gwr.model = gwr(Aprobación.Gobernador~ . ,  data =base,
                adapt=GWRbandwidth,
                hatmatrix=TRUE,
                se.fit=TRUE) 
gwr.model
results <-as.data.frame(gwr.model$SDF)
gwr.map <- cbind(base, as.matrix(results))
library(tmap)
qtm(gwr.map, fill = "localR2")
results

qtm(gwr.map, fill = 'Salud.1')

gwr.map$Salud.1


coefs=gwr.model$SDF@data
coefs=coefs[,c(3:16)]
coefs=cbind(coefs,base_1$`Aprobación Gobernador`)
install.packages("Hmisc")
library(Hmisc)
resultados_corr <- rcorr(as.matrix(coefs))

# Extraer la matriz de correlaciones
matriz_correlacion <- resultados_corr$r

# Extraer la matriz de p-valores
matriz_pvalores <- resultados_corr$P

# Imprimir la matriz de correlaciones
#print(matriz_correlacion)

# Imprimir la matriz de p-valores
#print(matriz_pvalores)

library(ggplot2)
library(reshape2)  # Para derretir las matrices
#install.packages("reshape2")
# Supongamos que ya tienes 'matriz_correlacion' y 'matriz_pvalores'

# Convertir las matrices en formato largo
cor_data <- melt(matriz_correlacion)
pval_data <- melt(matriz_pvalores)

# Combinar las dos matrices en un solo data frame
cor_data$p_value <- pval_data$value
signif_code <- function(p) {
  if(!is.na(p)){
    if (p < 0.001) {
      return("***")
    } else if (p < 0.01) {
      return("**")
    } else if (p < 0.05) {
      return("*")
    } else if (p < 0.1) {
      return(".")
    } else {
      return("")
    }
  }
  else{
    return("-")
  }
}

# Aplicar la función a los p-valores
cor_data$significance <- sapply(pval_data$value, signif_code)
cor_data <- cor_data[lower.tri(matriz_correlacion, diag = TRUE), ]
# Crear la matriz de calor con ggplot2
cor_data$value[cor_data$Var1 == cor_data$Var2] <- NA
write.csv(cor_data,"GWR/WP/Outputs/correlacion.csv")

cor_data2=read.csv("GWR/WP/Outputs/correlacion.csv",fileEncoding = "latin1")
factor(cor_data$Var1)
cor_data$Var1=cor_data2$Var1
cor_data$Var2=cor_data2$Var2
cor_data=cor_data[cor_data2$Var1!="Aprobación Gobernador" & cor_data2$Var2!="Aprobación Gobernador",]
cor_data$Var1=factor(cor_data$Var1,levels = unique(cor_data$Var1))
cor_data$Var2=factor(cor_data$Var2,levels = unique(cor_data$Var2))

# Crear la matriz de calor con ggplot2
p=ggplot(cor_data, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +  # Las celdas del heatmap
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlación", na.value = "white") +  # Personalizar los colores
  geom_text(aes(label = ifelse(is.na(value), "", sprintf("%.2f\n%s", value, significance))), 
            color = "black", size = 4) +  # Añadir etiquetas, pero omitir en la diagonal
  theme_minimal() +  # Tema minimalista
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1),
        axis.text.y = element_text(size = 12)) +  # Ajustar etiquetas
  labs(x = "Variables", y = "Variables", title = "Matriz de Correlación (Triangular Inferior)") +
  coord_fixed()  # Asegurar que las celdas sean cuadradas
p
ggsave("GWR/WP/Outputs/matriz_correlacion_coeficientes.png", plot = p, width = 10, height = 8, dpi = 300)


##Repetimos la matriz de correlacion con los datos originales

coefs=base_2[,c(2:11,13:16,12)]
#coefs=coefs[,c(3:16)]
#coefs=cbind(coefs,base_1$`Aprobación Gobernador`)
install.packages("Hmisc")
library(Hmisc)
resultados_corr <- rcorr(as.matrix(coefs))

# Extraer la matriz de correlaciones
matriz_correlacion <- resultados_corr$r

# Extraer la matriz de p-valores
matriz_pvalores <- resultados_corr$P

# Imprimir la matriz de correlaciones
#print(matriz_correlacion)

# Imprimir la matriz de p-valores
#print(matriz_pvalores)

library(ggplot2)
library(reshape2)  # Para derretir las matrices
#install.packages("reshape2")
# Supongamos que ya tienes 'matriz_correlacion' y 'matriz_pvalores'

# Convertir las matrices en formato largo
cor_data <- melt(matriz_correlacion)
pval_data <- melt(matriz_pvalores)

# Combinar las dos matrices en un solo data frame
cor_data$p_value <- pval_data$value
signif_code <- function(p) {
  if(!is.na(p)){
    if (p < 0.001) {
      return("***")
    } else if (p < 0.01) {
      return("**")
    } else if (p < 0.05) {
      return("*")
    } else if (p < 0.1) {
      return(".")
    } else {
      return("")
    }
  }
  else{
    return("-")
  }
}

# Aplicar la función a los p-valores
cor_data$significance <- sapply(pval_data$value, signif_code)
cor_data <- cor_data[lower.tri(matriz_correlacion, diag = TRUE), ]
# Crear la matriz de calor con ggplot2
cor_data$value[cor_data$Var1 == cor_data$Var2] <- NA

# Crear la matriz de calor con ggplot2
p=ggplot(cor_data, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +  # Las celdas del heatmap
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlación", na.value = "white") +  # Personalizar los colores
  geom_text(aes(label = ifelse(is.na(value), "", sprintf("%.2f\n%s", value, significance))), 
            color = "black", size = 4) +  # Añadir etiquetas, pero omitir en la diagonal
  theme_minimal() +  # Tema minimalista
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1),
        axis.text.y = element_text(size = 12)) +  # Ajustar etiquetas
  labs(x = "Variables", y = "Variables", title = "Matriz de Correlación (Triangular Inferior)") +
  coord_fixed()  # Asegurar que las celdas sean cuadradas
p
ggsave("GWR/WP/Outputs/matriz_correlacion_datos.png", plot = p, width = 10, height = 8, dpi = 300)
#3Con los pesos:

cor_2=cov.wt(as.matrix(coefs),wt = base_2$PESOF,cor = T)


cor_data <- melt(cor_2)
cor_data=cor_data[cor_data$L1=='cor',1:3]
pval_data <- melt(matriz_pvalores)

# Combinar las dos matrices en un solo data frame
cor_data$p_value <- pval_data$value
signif_code <- function(p) {
  if(!is.na(p)){
    if (p < 0.001) {
      return("***")
    } else if (p < 0.01) {
      return("**")
    } else if (p < 0.05) {
      return("*")
    } else if (p < 0.1) {
      return(".")
    } else {
      return("")
    }
  }
  else{
    return("-")
  }
}

# Aplicar la función a los p-valores
cor_data$significance <- sapply(pval_data$value, signif_code)
cor_data <- cor_data[lower.tri(matriz_correlacion, diag = TRUE), ]
# Crear la matriz de calor con ggplot2
cor_data$value[cor_data$Var1 == cor_data$Var2] <- NA

# Crear la matriz de calor con ggplot2
p=ggplot(cor_data, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +  # Las celdas del heatmap
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlación", na.value = "white") +  # Personalizar los colores
  geom_text(aes(label = ifelse(is.na(value), "", sprintf("%.2f\n%s", value, significance))), 
            color = "black", size = 4) +  # Añadir etiquetas, pero omitir en la diagonal
  theme_minimal() +  # Tema minimalista
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1),
        axis.text.y = element_text(size = 12)) +  # Ajustar etiquetas
  labs(x = "Variables", y = "Variables", title = "Matriz de Correlación (Triangular Inferior)") +
  coord_fixed()  # Asegurar que las celdas sean cuadradas
p
ggsave("GWR/WP/Outputs/matriz_correlacion_datos.png", plot = p, width = 10, height = 8, dpi = 300)



#
library(foreign)
base_fea=read.spss("mapas/Winnie pooh/Regresión lineal/Base Hidalgo Julio 2024 con MPO.SAV",to.data.frame = T)
base_fea_=read.spss("mapas/Winnie pooh/Regresión lineal/Base Hidalgo Julio 2024 con MPO.SAV")
base_fea=lapply(base_fea,as.character)
base_fea=melt(base_fea)

write.csv(base_fea,"Base Hidalgo Julio 2024.csv")
#
descriptores_feos=attr(base_fea_, "variable.labels")
descriptores_feos_=as.data.frame(descriptores_feos)
write.csv(descriptores_feos_,"descriptores Base Hidalgo Julio 2024.csv")
