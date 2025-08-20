#Selección de variables. 
#Ejemplo trivial 

ejemplo.data=sav_2025 |> 
  dplyr::select(P19,edad,Sexo,escolaridad,ocupacion,ingresos,nivel_socieconomico,PESOF) |> 
  dplyr::filter(P19%in%c("Aprueba","Desaprueba")) |> 
  dplyr::mutate(P19=ifelse(P19=='Aprueba',1,0))

head(ejemplo.data)
#install.packages("nnet")
# library(nnet)
# test=multinom(P19 ~ Edad+Sexo+ESC+OCUP+NSER, data = ejemplo.data)
# summary(test)
#mydata$rank <- factor(mydata$rank)

library(sjPlot)
mylogit = glm(P19 ~ edad+Sexo+escolaridad+ocupacion+nivel_socieconomico, data = ejemplo.data, family = "binomial")

summary(mylogit)

confint(mylogit)

#install.packages("aod")
library(aod)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms =168:171)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 17:26)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 27:29)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 30:31)

model.full = glm( P19~ edad+Sexo+escolaridad+ocupacion+nivel_socieconomico,
                  data=ejemplo.data,
                  family = binomial())


levels(ejemplo.data$Sexo)
levels(ejemplo.data$escolaridad)
levels(ejemplo.data$ocupacion)
levels(ejemplo.data$nivel_socieconomico)
##

mylogit = glm(P19 ~ edad+Sexo+escolaridad+ocupacion+nivel_socieconomico, data = ejemplo.data, family = "binomial")
summary(mylogit)
newdata1 <- with(ejemplo.data, data.frame(edad=mean(edad),Sexo='Hombre',escolaridad='Sin escolaridad',
                                          ocupacion="Empleado de gobierno",
                                          nivel_socieconomico=c("Alta","Media","Baja")))

newdata1$NSER_PP <- predict.glm(mylogit, newdata = newdata1, type = "terms")
