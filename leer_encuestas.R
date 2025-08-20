library(foreign)
cat_sav_2025=read.spss("2025/Datos/Base de datos (con geolocalizacion y codificacion) 2025.SAV")
sav_2025=foreign::read.spss("2025/Datos/Base de datos (con geolocalizacion y codificacion) 2025.SAV",to.data.frame = T)
#mujeres que tienen hijes están afiliadas al pan
variable_labels_2025 <- attr(sav_2025, "variable.labels")
sav_2025[is.na(sav_2025)]='-'
variables_2025=as.data.frame(matrix(c(colnames(sav_2025),variable_labels_2025),ncol=2,nrow=length(colnames(sav_2025)),byrow = F))
# Asigna las etiquetas como nombres de las columnas

cat_sav_2024=read.spss("2024/Datos/Base 437 Hidalgo Julio 2024.SAV")
sav_2024=foreign::read.spss("2024/Datos/Base 437 Hidalgo Julio 2024.SAV",to.data.frame = T)
#mujeres que tienen hijes están afiliadas al pan
variable_labels_2024 <- attr(sav_2024, "variable.labels")
sav_2024[is.na(sav_2024)]='-'
variables_2024=as.data.frame(matrix(c(colnames(sav_2024),variable_labels_2024),ncol=2,nrow=length(colnames(sav_2024)),byrow = F))
# Asigna las etiquetas como nombres de las columnas

##################################
